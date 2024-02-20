package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util._

/**
 * Object that selects a target from a vector of all possible targets of a BP stage based on the taken mask and hit signal.
 * The order of the targets in the vector should be: [taken_target0, taken_target1, ..., fallThroughAddr, not hit (plus fetch width) & alignment].
 *
 * @tparam T Type of the targets in the vector.
 * @param takenMask Vector of booleans representing the taken mask.
 * @param hit Boolean signal indicating whether the branch hit in the BP
 * @param allTargets Vector of all possible targets of a BP stage.
 * @return The selected target.
 */
object selectByTaken {
  // the para allTarget is a Vec of all possible target of a BP stage
  // in the following order: [taken_target0, taken_target1, ..., fallThroughAddr, not hit (plus fetch width) & alignment]
  def apply[T <: Data](takenMask: Vec[Bool], hit: Bool, allTargets: Vec[T]): T = {
    val selVecOH =
      takenMask.zipWithIndex.map { case (t, i) => !takenMask.take(i).fold(false.B)(_ || _) && t && hit } :+
        (!takenMask.asUInt.orR && hit) :+ !hit
    Mux1H(selVecOH, allTargets)
  }
}

class TableAddr(val idxBits: Int, val banks: Int)(implicit p: Parameters) extends BoomBundle
{
  val instOffsetBits = 1
  def tagBits = vaddrBitsExtended - idxBits - instOffsetBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(instOffsetBits.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(vaddrBitsExtended.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = if (banks > 1) getIdx(x)(log2Up(banks) - 1, 0) else 0.U
  def getBankIdx(x: UInt) = if (banks > 1) getIdx(x)(idxBits - 1, log2Up(banks)) else getIdx(x)
}

class BlockPrediction(implicit p: Parameters) extends BoomBundle()(p)
with HasBoomFTBParameters
{
  val br_taken_mask = Vec(numBr, Bool())

  val slot_valids = Vec(totalSlot, Bool())

  val targets = Vec(totalSlot, UInt(vaddrBitsExtended.W))
  //val jalr_target = UInt(vaddrBitsExtended.W) // special path for indirect predictors
  val offsets = Vec(totalSlot, UInt(log2Ceil(predictWidth).W))
  val fallThroughAddr = UInt(vaddrBitsExtended.W)
  // val fallThroughErr = Bool()

  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val last_may_be_rvi_call = Bool()
  val is_br_sharing = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool() // hit must be manually set
  val blockMask = Vec(predictWidth, Bool())

  // val predCycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None

  /**
   * Returns the valid slots except the last one.
   */
  def br_slot_valids = slot_valids.init

  /**
   * Returns the validity of the last slot in the list of slot validities.
   */
  def tail_slot_valid = slot_valids.last

  // returns the validity of strict br slots (no unconditional jumps)
  def br_valids = {
    VecInit(br_slot_valids :+ (tail_slot_valid && is_br_sharing))
  }

  /**
   * Returns a mask indicating if the branch is taken for each slot.
   * The mask is of length numBr.
   * If the branch is not valid, the corresponding bit is false.
   * However, the hit bit is not considered.
   *
   * @return A VecInit representing the mask of taken branches for each slot.
   */
  def taken_mask_on_slot = {
    VecInit(
      (br_slot_valids zip br_taken_mask.init).map{ case (t, v) => t && v } :+ (
        tail_slot_valid && (
          is_br_sharing && br_taken_mask.last || !is_br_sharing
        )
      )
    )
  }

  // returns true if the slot at idx is valid and predicted taken
  def is_taken(idx: UInt) : Bool = {
    (taken_mask_on_slot zip offsets).map{ case (t, o) => t && o === idx }.reduce(_|_)
  }

  def real_slot_taken_mask(): Vec[Bool] = {
    VecInit(taken_mask_on_slot.map(_ && hit))
  }

  def real_slot_taken(): Bool = {
    real_slot_taken_mask().reduce(_||_)
  }

  // len numBr
  // if the tailslot contains a br and the br is taken, then the tailslot is taken
  def real_br_taken_mask(): Vec[Bool] = {
    VecInit(
      taken_mask_on_slot.map(_ && hit).init :+
      (br_taken_mask.last && tail_slot_valid && is_br_sharing && hit)
    )
  }

  // the vec indicating if ghr should shift on each branch
  def shouldShiftVec =
    VecInit(br_valids.zipWithIndex.map{ case (v, i) =>
      v && !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B)})

  /**
   * Returns a vector indicating the position of the last branch in the entry.
   * The vector is composed of a boolean indicating if there was no hit or no branches in the entry,
   * followed by a sequence of booleans indicating if the branch at that position is the last one in the entry.
   * A branch is considered the last one if there are no branches taken in front of it and no branches behind it.
   * @return a vector indicating the position of the last branch in the entry.
   */
  def lastBrPosOH =
    VecInit((!hit || !br_valids.reduce(_||_)) +: // not hit or no brs in entry
      (0 until numBr).map(i =>
        br_valids(i) &&
        !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B) && // no brs taken in front it
        (real_br_taken_mask()(i) || !br_valids.drop(i+1).reduceOption(_||_).getOrElse(false.B)) && // no brs behind it
        hit
      )
    )

  def brTaken = (br_valids zip br_taken_mask).map{ case (a, b) => a && b && hit}.reduce(_||_)

  def target(pc: UInt): UInt = {
    selectByTaken(taken_mask_on_slot, hit, allTarget(pc))
  }

  def getTarget(idx:UInt, pc:UInt):UInt = {
    val idx_match_mask = VecInit(offsets.map(_ === idx))
    val taken_idx_match_mask = idx_match_mask.zip(taken_mask_on_slot).map{ case (a, b) => a && b}
    selectByTaken(taken_mask_on_slot, hit, allTarget(pc))
  }

  def validPredict(idx: UInt):Bool = {
    val idx_match_mask = VecInit(offsets.map(_ === idx))
    val taken_idx_match_mask = idx_match_mask.zip(taken_mask_on_slot).map{ case (a, b) => a && b}
    taken_idx_match_mask.reduce(_||_)
  }

  // if there is a jmp in the entry, return the target of the jmp
  def getJmpTarget(pc:UInt): UInt = {
    targets.last
  }

  // allTarget return a Vec of all possible target of a BP stage
  // in the following order: [taken_target0, taken_target1, ..., fallThroughAddr, not hit (plus fetch width) & alignment]
  //
  // This exposes internal targets for timing optimization,
  // since usually targets are generated quicker than taken
  def allTarget(pc: UInt): Vec[UInt] = {
    VecInit(targets :+ fallThroughAddr :+ nextFetch(pc))
  }

  // def fallThruError: Bool = hit && fallThroughErr

  def hit_taken_on_jmp =
    !real_slot_taken_mask().init.reduce(_||_) &&
    real_slot_taken_mask().last && !is_br_sharing
  def hit_taken_on_call = hit_taken_on_jmp && is_call
  def hit_taken_on_ret  = hit_taken_on_jmp && is_ret
  def hit_taken_on_jalr = hit_taken_on_jmp && is_jalr
  def hit_taken_on_br = hit && real_slot_taken_mask().reduce(_||_) && !hit_taken_on_jmp

  // returns the index of the cfi that is taken
  // if no cfi is taken, return PredictWidth-1
  def cfiIndex = {
    // val cfiIndex = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
    val cfiIndex = Wire(Valid(UInt(log2Ceil(predictWidth).W)))
    cfiIndex.valid := real_slot_taken_mask().asUInt.orR
    // when no takens, set cfiIndex to PredictWidth-1
    cfiIndex.bits :=
      PriorityMux(real_slot_taken_mask(), offsets) |
      Fill(log2Ceil(predictWidth), (!real_slot_taken_mask().asUInt.orR).asUInt)
    cfiIndex
  }

  def taken = br_taken_mask.reduce(_||_) || slot_valids.last // || (is_jal || is_jalr)

  def brMask: UInt = {
    (br_valids zip offsets).map{ case (t, o) => (t << o)}.reduce(_|_).asUInt()
  }



  def fromFtbEntry(
                    entry: FTBEntry,
                    pc: UInt,
                    last_stage_pc: Option[Tuple2[UInt, Bool]] = None,
                    last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None
                  ) = {
    slot_valids := entry.brSlots.map(_.valid) :+ entry.tailSlot.valid
    targets := entry.getTargetVec(pc, last_stage_pc) // Use previous stage pc for better timing
    // jalr_target := targets.last
    offsets := entry.getOffsetVec
    is_jal := entry.tailSlot.valid && entry.isJal
    is_jalr := entry.tailSlot.valid && entry.isJalr
    is_call := entry.tailSlot.valid && entry.isCall
    is_ret := entry.tailSlot.valid && entry.isRet
    last_may_be_rvi_call := entry.last_may_be_rvi_call
    is_br_sharing := entry.tailSlot.valid && entry.tailSlot.sharing
    // predCycle.map(_ := GTimer())

    // fallThroughAddr := Mux(fallThroughErr, pc + (predictWidth * 2).U, entry.getFallThrough(pc, last_stage_entry))
    fallThroughAddr := Mux(entry.valid, entry.getFallThrough(pc, last_stage_entry), nextFetch(pc))

    // WarnAssert(fallThroughAddr > pc, p"fallthrough error: pc 0x${Hexadecimal(pc)} fallthru 0x${Hexadecimal(fallThroughAddr)} carry: ${entry.carry} pftAddr ${entry.pftAddr}\n")

    blockMask := Mux(entry.carry || !entry.valid, VecInit((0 until predictWidth).map(i => true.B)), VecInit((0 until predictWidth).map(i => i.U < entry.pftAddr)))
    // assert(blockMask.asUInt =/= 0.U, "blockMask should not be zero")
    // assert(fallThroughAddr =/= 0.U, "fallThroughAddr should not be zero")
  }

  def makeDefault(
                  pc: UInt,
                  last_stage_pc: Option[Tuple2[UInt, Bool]] = None,
                  last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None
  ) = {
    br_taken_mask := VecInit(Seq.fill(numBr)(false.B))
    slot_valids := VecInit(Seq.fill(totalSlot)(false.B))
    targets := VecInit(Seq.fill(totalSlot)(0.U))
    // jalr_target := 0.U
    offsets := VecInit(Seq.fill(totalSlot)(0.U))
    fallThroughAddr := nextFetch(pc)
    assert(fallThroughAddr =/= 0.U, "fallThroughAddr should not be zero")

    is_jal := false.B
    is_jalr := false.B
    is_call := false.B
    is_ret := false.B
    last_may_be_rvi_call := false.B
    is_br_sharing := false.B
    // predCycle.map(_ := 0.U)

    // fallThroughErr := false.B
    hit := false.B
    blockMask := VecInit(Seq.fill(predictWidth)(true.B))
  }

  def display(cond: Bool): Unit = {
    XSDebug(cond, "---BlockPrediction---\n")
    XSDebug(cond, p"taken_mask: ${Binary(br_taken_mask.asUInt)} hit: $hit\n")
    XSDebug(cond, p"slot_valids: ${Binary(slot_valids.asUInt)} blockMask: ${Binary(blockMask.asUInt)}\n")
    for (i <- 0 until totalSlot) {
      XSDebug(cond, p"slot $i: valid:${slot_valids(i)} target:0x${Hexadecimal(targets(i))} offset:${offsets(i)}\n")
    }
    XSDebug(cond, p"fallThroughAddr: ${Hexadecimal(fallThroughAddr)}\n")
    XSDebug(cond, p"is_jal: $is_jal is_jalr: $is_jalr is_call: $is_call is_ret: $is_ret last_may_be_rvi_call: $last_may_be_rvi_call is_br_sharing: $is_br_sharing\n")
    XSDebug(cond, "---------------------\n")
  }
}

class BlockPredictionBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
    val pc = UInt(vaddrBitsExtended.W)
    val pred = new BlockPrediction
    val meta = UInt(bpdMaxMetaLength.W)
    val lhist = UInt(localHistoryLength.W)
}

class BPBankUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
  val is_mispredict_update     = Bool()
  val is_repair_update         = Bool()

  val btb_mispredicts  = UInt(fetchWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U

  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc               = UInt(vaddrBitsExtended.W)

  val br_mask          = UInt(fetchWidth.W)
  val cfi_idx          = Valid(UInt(log2Ceil(fetchWidth).W))
  val cfi_taken        = Bool()
  val cfi_mispredicted = Bool()

  val cfi_is_br        = Bool()
  val cfi_is_jal       = Bool()
  val cfi_is_jalr      = Bool()

  val ghist            = UInt(globalHistoryLength.W)
  val lhist            = UInt(localHistoryLength.W)

  val target           = UInt(vaddrBitsExtended.W)

  val meta             = UInt(bpdMaxMetaLength.W)

  val ftb_entry        = new FTBEntry
  val br_taken_mask = Vec(numBr, Bool())
  // val br_committed = Vec(numBr, Bool()) // High only when br valid && br committed // seems not used by any BP in Xiangshan
  // val jmp_taken = Bool() // seems for ittage
  val mispred_mask = Vec(numBr+1, Bool()) // seems for tage
  // val pred_hit = Bool()
  // val false_hit = Bool()
  // val new_br_insert_pos = Vec(numBr, Bool()) // seems not used by any BP in Xiangshan

  def mispredSlotIdx :UInt = {
    val offsets = ftb_entry.getOffsetVec
    val mask = VecInit(offsets.map(_ === cfi_idx.bits))
    PriorityEncoder(mask)
  }

  def display(cond: Bool) :Unit = {
    val prefix = p"BPBankUpdate: "
    // print the infos in Hexadecimal and Binary format
    val condFromBTBCorrect = is_btb_mispredict_update
    val condFromFTQ = !is_btb_mispredict_update && ftb_entry.valid
    XSDebug(cond && condFromFTQ, "-----------------Block Predictor update from FTQ-------------------\n")
    XSDebug(cond && condFromBTBCorrect, "-----------------Block Predictor update from BTBCorrect-------------------\n")
    XSDebug(cond && cfi_is_br && cfi_mispredicted, p"BR Mispredicted Update: PC:0x${Hexadecimal(pc)},cfi_idx: ${cfi_idx}, target:0x${Hexadecimal(target)}\n")
    XSDebug(cond && !cfi_is_br && cfi_mispredicted, p"JMP Mispredicted Update: PC:0x${Hexadecimal(pc)},cfi_idx: ${cfi_idx}, target:0x${Hexadecimal(target)}, cfi_is_jal:${cfi_is_jal}, cfi_is_jalr:${cfi_is_jalr}\n")
    XSDebug(cond, p"pc:0x${Hexadecimal(pc)}, br_mask:${Binary(br_mask)}, cfi_idx.valid:${cfi_idx.valid}, cfi_idx.bits:${cfi_idx.bits} \n")
    XSDebug(cond, p"cfi_taken:${cfi_taken}, cfi_mispredicted:${cfi_mispredicted}, cfi_is_br:${cfi_is_br}, cfi_is_jal:${cfi_is_jal}, cfi_is_jalr:${cfi_is_jalr}, target:0x${Hexadecimal(target)}\n")
    ftb_entry.display(cond, false)
    XSDebug(cond && condFromFTQ, "-------------------------------------------------------------------\n")
    XSDebug(cond && condFromBTBCorrect, "--------------------------------------------------------------------------\n")
  }
}

class BlockUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
  val is_mispredict_update     = Bool()
  val is_repair_update         = Bool()

  val btb_mispredicts  = UInt(fetchWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U

  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc               = UInt(vaddrBitsExtended.W)

  val br_mask          = UInt(fetchWidth.W)
  val cfi_idx          = Valid(UInt(log2Ceil(fetchWidth).W))
  val cfi_taken        = Bool()
  val cfi_mispredicted = Bool()

  val cfi_is_br        = Bool()
  val cfi_is_jal       = Bool()
  val cfi_is_jalr      = Bool()

  val ghist            = new GlobalHistory
  val lhist            = UInt(localHistoryLength.W)

  val target           = UInt(vaddrBitsExtended.W)

  val meta             = UInt(bpdMaxMetaLength.W)

  val pd = new PredecodeBundle

  val ftb_entry = new FTBEntry
  val br_taken_mask = Vec(numBr, Bool())

  def display(cond: Bool , decimal:Boolean = false, _prefix:chisel3.Printable = p""):Unit={
    val prefix = _prefix + p"BranchPredictionUpdate: "
    if(decimal){
      XSDebug(cond, prefix + p"pc: ${pc} br_mask: ${br_mask} cfi_idx.valid: ${cfi_idx.valid} cfi_idx.bits: ${cfi_idx.bits} cfi_taken: ${cfi_taken} cfi_mispredicted: ${cfi_mispredicted} cfi_is_br: ${cfi_is_br} cfi_is_jal: ${cfi_is_jal} cfi_is_jalr: ${cfi_is_jalr} target: ${target}\n")
      XSDebug(cond, prefix + p"btb_mispredicts: ${btb_mispredicts} is_mispredict_update: ${is_mispredict_update} is_repair_update:${is_repair_update}\n")
      pd.display(cond, true, prefix)
      
      // XSDebug(cond, prefix + p"ghist:${ghist}\n")
      // XSDebug(cond, prefix + p"lhist:${lhist}\n")
    }
    else{
      XSDebug(cond, prefix + p"pc:0x${Hexadecimal(pc)}, br_mask:${Binary(br_mask)}, cfi_idx.valid:${cfi_idx.valid}, cfi_idx.bits:${cfi_idx.bits}, cfi_taken:${cfi_taken}, cfi_mispredicted:${cfi_mispredicted}, cfi_is_br:${cfi_is_br}, cfi_is_jal:${cfi_is_jal}, cfi_is_jalr:${cfi_is_jalr}, target:0x${Hexadecimal(target)}\n")
      XSDebug(cond, prefix + p"btb_mispredicts:${Binary(btb_mispredicts)}, is_mispredict_update:${is_mispredict_update}, is_repair_update:${is_repair_update}\n")
      pd.display(cond, false, prefix)
      
      // XSDebug(cond, prefix + p"ghist:${ghist}\n")
      // XSDebug(cond, prefix + p"lhist:${lhist}\n")
    }
  }

}

class BlockUpdateInfo(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
  val is_mispredict_update     = Bool()  // TODO: do we need this?
  val is_repair_update         = Bool()  // TODO: do we need this?

  val btb_mispredicts  = UInt(fetchWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U

  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc               = UInt(vaddrBitsExtended.W)

  val br_mask          = UInt(fetchWidth.W)
  val cfi_idx          = Valid(UInt(log2Ceil(fetchWidth).W))
  val cfi_taken        = Bool()
  val cfi_mispredicted = Bool()

  // val cfi_is_br        = Bool()
  // val cfi_is_jal       = Bool()
  // val cfi_is_jalr      = Bool()

  val ghist            = new GlobalHistory
  val lhist            = UInt(localHistoryLength.W)

  val target           = UInt(vaddrBitsExtended.W)

  val meta             = UInt(bpdMaxMetaLength.W)

  val pd = new PredecodeBundle

  val ftb_entry = new FTBEntry
  val br_taken_mask = Vec(numBr, Bool())
}
// class BlockBranchPredictionRequest(implicit p: Parameters) extends BoomBundle()(p)
// {
//   val pc    = UInt(vaddrBitsExtended.W)
//   val ghist = new GlobalHistory
// }

class BPBankResponse(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
  val f1 = new BlockPrediction 
  val f2 = new BlockPrediction
  val f3 = new BlockPrediction
  val f3_meta = UInt(bpdMaxMetaLength.W)
  val last_stage_entry = new FTBEntry
}

abstract class BlockPredictorBank(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFTBParameters
  with BPUUtils
{
  val metaSz = 0
  def nInputs = 1

  // val mems: Seq[Tuple3[String, Int, Int]]

  val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(vaddrBitsExtended.W))
    val f0_mask  = Input(UInt(predictWidth.W))
    // Local history not available until end of f1
    val f1_ghist = Input(UInt(globalHistoryLength.W))
    val f1_lhist = Input(UInt(localHistoryLength.W))

    val resp_in = Input(Vec(nInputs, new BPBankResponse))
    val resp = Output(new BPBankResponse)

    val f3_fire = Input(Bool())

    val update = Input(Valid(new BPBankUpdate))
  })
  io.resp := io.resp_in(0)

  // io.f3_meta := 0.U

  val s0_idx       = blockFetchIdx(io.f0_pc)
  val s1_idx       = RegNext(s0_idx)
  val s2_idx       = RegNext(s1_idx)
  val s3_idx       = RegNext(s2_idx)

  val s0_valid = io.f0_valid
  val s1_valid = RegNext(s0_valid)
  val s2_valid = RegNext(s1_valid)
  val s3_valid = RegNext(s2_valid)

  val s0_mask = io.f0_mask
  val s1_mask = RegNext(s0_mask)
  val s2_mask = RegNext(s1_mask)
  val s3_mask = RegNext(s2_mask)

  val s0_pc = io.f0_pc
  val s1_pc = RegNext(s0_pc)

  val s0_update     = io.update
  val s0_update_idx = blockFetchIdx(io.update.bits.pc)
  val s0_update_valid = io.update.valid

  val s1_update     = RegNext(s0_update)
  val s1_update_idx = RegNext(s0_update_idx)
  val s1_update_valid = RegNext(s0_update_valid)

}

class NullBlockPredictorBank(implicit p: Parameters) extends BlockPredictorBank()(p)
  with HasBoomFTBParameters
{
  val mems = Nil
  io.resp := 0.U.asTypeOf(new BPBankResponse)
}



class BlockPredictor(implicit p:Parameters) extends BoomModule()(p)
    with HasBoomFTBParameters
{

  val io = IO(new Bundle {

    // Requests and responses
    val f0_req = Input(Valid(new BranchPredictionRequest))

    val resp = Output(new Bundle {
      val f1 = new BlockPredictionBundle
      val f2 = new BlockPredictionBundle
      val f3 = new BlockPredictionBundle
      val last_stage_entry = new FTBEntry
    })

    val f3_fire = Input(Bool())

    // Update
    val update = Input(Valid(new BlockUpdate))
  })

  val bpdStr = new StringBuilder
  bpdStr.append(BoomCoreStringPrefix("==Branch Predictor Memory Sizes==\n")) // TODO: fixme
  // val banked_predictors = (0 until nBPBanks) map ( b => {
  //   val m = Module(if (useBPD) new ComposedBranchPredictorBank else new NullBranchPredictorBank)
  //   for ((n, d, w) <- m.mems) {
  //     bpdStr.append(BoomCoreStringPrefix(f"bank$b $n: $d x $w = ${d * w / 8}"))
  //     total_memsize = total_memsize + d * w / 8
  //   }
  //   m
  // })
  // bpdStr.append(BoomCoreStringPrefix(f"Total bpd size: ${total_memsize / 1024} KB\n"))
  override def toString: String = bpdStr.toString

  // val predictors = Module (new NullBlockPredictorBank)
  val predictors = Module (new ComposedBlockPredictorBank)
  val lhist_providers = Module(if(localHistoryNSets > 0) new LocalBranchPredictorBank else new NullLocalBranchPredictorBank)

  lhist_providers.io.f0_valid := io.f0_req.valid
  lhist_providers.io.f0_pc    := io.f0_req.bits.pc // TODO: the original impl uses bankAlignPC

  predictors.io.f0_valid := io.f0_req.valid
  predictors.io.f0_pc    := io.f0_req.bits.pc
  predictors.io.f0_mask  := fetchMask(io.f0_req.bits.pc)

  predictors.io.f1_ghist := RegNext(io.f0_req.bits.ghist.histories(0)) // TODO: 0?
  predictors.io.f1_lhist := lhist_providers.io.f1_lhist

  predictors.io.resp_in(0) := 0.U.asTypeOf(new BPBankResponse)

  //TODO: f3_taken_br
  lhist_providers.io.f3_taken_br := predictors.io.resp.f3.hit_taken_on_br

  io.resp.f1.pred := predictors.io.resp.f1
  io.resp.f2.pred := predictors.io.resp.f2
  io.resp.f3.pred := predictors.io.resp.f3
  io.resp.f3.meta := predictors.io.resp.f3_meta
  io.resp.f3.lhist := lhist_providers.io.f3_lhist
  assert(io.resp.f3.pred.blockMask.asUInt =/= 0.U, "blockMask should not be zero")

  predictors.io.f3_fire := io.f3_fire
  lhist_providers.io.f3_fire := io.f3_fire

  io.resp.f1.pc := RegNext(io.f0_req.bits.pc)
  io.resp.f2.pc := RegNext(io.resp.f1.pc)
  io.resp.f3.pc := RegNext(io.resp.f2.pc)

  // We don't care about meta from the f1 and f2 resps
  // Use the meta from the latest resp
  io.resp.f1.meta := DontCare
  io.resp.f2.meta := DontCare
  io.resp.f1.lhist := DontCare
  io.resp.f2.lhist := DontCare

  // ftb entry
  io.resp.last_stage_entry := predictors.io.resp.last_stage_entry

  // Update
  predictors.io.update.bits.is_mispredict_update := io.update.bits.is_mispredict_update
  predictors.io.update.bits.is_repair_update := io.update.bits.is_repair_update
  predictors.io.update.bits.meta := io.update.bits.meta(0)
  predictors.io.update.bits.lhist := io.update.bits.lhist
  predictors.io.update.bits.cfi_idx.bits := io.update.bits.cfi_idx.bits
  predictors.io.update.bits.cfi_taken := io.update.bits.cfi_taken
  predictors.io.update.bits.cfi_mispredicted := io.update.bits.cfi_mispredicted
  predictors.io.update.bits.cfi_is_br := io.update.bits.cfi_is_br
  predictors.io.update.bits.cfi_is_jal := io.update.bits.cfi_is_jal
  predictors.io.update.bits.cfi_is_jalr := io.update.bits.cfi_is_jalr
  predictors.io.update.bits.target := io.update.bits.target

  lhist_providers.io.update.mispredict := io.update.bits.is_mispredict_update
  lhist_providers.io.update.repair := io.update.bits.is_repair_update
  lhist_providers.io.update.lhist := io.update.bits.lhist

  predictors.io.update.valid := io.update.valid
  predictors.io.update.bits.pc := io.update.bits.pc // TODO: the original impl uses bankAlignPC
  predictors.io.update.bits.br_mask := io.update.bits.br_mask
  predictors.io.update.bits.btb_mispredicts := io.update.bits.btb_mispredicts
  predictors.io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid
  predictors.io.update.bits.ghist := io.update.bits.ghist.histories(0)

  predictors.io.update.bits.ftb_entry := io.update.bits.ftb_entry
  predictors.io.update.bits.br_taken_mask := io.update.bits.br_taken_mask

  predictors.io.update.bits.mispred_mask := DontCare

  lhist_providers.io.update.valid := io.update.valid && io.update.bits.br_mask =/= 0.U
  lhist_providers.io.update.pc := io.update.bits.pc // TODO: the original impl uses bankAlignPC

  when(io.update.valid){
    when(io.update.bits.cfi_is_br && io.update.bits.cfi_idx.valid){
      assert(io.update.bits.br_mask(io.update.bits.cfi_idx.bits))
    }
  }

  if(enbaleBankPredictorUpdatePrint){
    XSDebug(io.update.valid,p"-----------------Block Predictor update pc :${io.update.bits.pc}-------------------\n")
    io.update.bits.ftb_entry.display(io.update.valid)
    XSDebug(io.update.valid,p"------------------------------------\n")
  }

}
