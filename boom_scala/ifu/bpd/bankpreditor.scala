package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

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

class BlockBranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
with HasBoomFTBParameters
{
  val br_taken_mask = Vec(numBr, Bool())

  val slot_valids = Vec(totalSlot, Bool())

  val targets = Vec(totalSlot, UInt(vaddrBitsExtended.W))
  //val jalr_target = UInt(vaddrBitsExtended.W) // special path for indirect predictors
  val offsets = Vec(totalSlot, UInt(log2Ceil(predictWidth).W))
  val fallThroughAddr = UInt(vaddrBitsExtended.W)
  val fallThroughErr = Bool()

  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val last_may_be_rvi_call = Bool()
  val is_br_sharing = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool()

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

  // seems the length is numBr
  def taken_mask_on_slot = {
    VecInit(
      (br_slot_valids zip br_taken_mask.init).map{ case (t, v) => t && v } :+ (
        tail_slot_valid && (
          is_br_sharing && br_taken_mask.last || !is_br_sharing
        )
      )
    )
  }

  def real_slot_taken_mask(): Vec[Bool] = {
    VecInit(taken_mask_on_slot.map(_ && hit))
  }

  // len numBr
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

  // allTarget return a Vec of all possible target of a BP stage
  // in the following order: [taken_target0, taken_target1, ..., fallThroughAddr, not hit (plus fetch width) & alignment]
  //
  // This exposes internal targets for timing optimization,
  // since usually targets are generated quicker than taken
  def allTarget(pc: UInt): Vec[UInt] = {
    VecInit(targets :+ fallThroughAddr :+ ((pc+predictBytes.U) & ~((1.U << log2Ceil(predictBytes).U) - 1.U))) //pc + predictBytes then align to predictBytes
  }

  def fallThruError: Bool = hit && fallThroughErr

  def hit_taken_on_jmp =
    !real_slot_taken_mask().init.reduce(_||_) &&
    real_slot_taken_mask().last && !is_br_sharing
  def hit_taken_on_call = hit_taken_on_jmp && is_call
  def hit_taken_on_ret  = hit_taken_on_jmp && is_ret
  def hit_taken_on_jalr = hit_taken_on_jmp && is_jalr

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

  // def fromFtbEntry(
  //                   entry: FTBEntry,
  //                   pc: UInt,
  //                   last_stage_pc: Option[Tuple2[UInt, Bool]] = None,
  //                   last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None
  //                 ) = {
  //   slot_valids := entry.brSlots.map(_.valid) :+ entry.tailSlot.valid
  //   targets := entry.getTargetVec(pc, last_stage_pc) // Use previous stage pc for better timing
  //   jalr_target := targets.last
  //   offsets := entry.getOffsetVec
  //   is_jal := entry.tailSlot.valid && entry.isJal
  //   is_jalr := entry.tailSlot.valid && entry.isJalr
  //   is_call := entry.tailSlot.valid && entry.isCall
  //   is_ret := entry.tailSlot.valid && entry.isRet
  //   last_may_be_rvi_call := entry.last_may_be_rvi_call
  //   is_br_sharing := entry.tailSlot.valid && entry.tailSlot.sharing
  //   predCycle.map(_ := GTimer())

  //   val startLower        = Cat(0.U(1.W),    pc(instOffsetBits+log2Ceil(PredictWidth)-1, instOffsetBits))
  //   val endLowerwithCarry = Cat(entry.carry, entry.pftAddr)
  //   fallThroughErr := startLower >= endLowerwithCarry
  //   fallThroughAddr := Mux(fallThroughErr, pc + (FetchWidth * 4).U, entry.getFallThrough(pc, last_stage_entry))
  // }

  // def display(cond: Bool): Unit = {
  //   XSDebug(cond, p"[taken_mask] ${Binary(br_taken_mask.asUInt)} [hit] $hit\n")
  // }
}

class BlockBranchPredictionBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
    val pc = UInt(vaddrBitsExtended.W)
    val pred = new BlockBranchPrediction
    val meta = UInt(bpdMaxMetaLength.W)
    val lhsit = UInt(localHistoryLength.W)
}

class BlockBranchPredictionUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
  // Indicates that this update is due to a speculated misprediction
  // Local predictors typically update themselves with speculative info
  // Global predictors only care about non-speculative updates
  val is_mispredict_update = Bool()
  val is_repair_update = Bool()
  val btb_mispredicts = UInt(fetchWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U
  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc            = UInt(vaddrBitsExtended.W)
  // Mask of instructions which are branches.
  // If these are not cfi_idx, then they were predicted not taken
  val br_mask       = UInt(fetchWidth.W)
  // Which CFI was taken/mispredicted (if any)
  val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))
  // Was the cfi taken?
  val cfi_taken     = Bool()
  // Was the cfi mispredicted from the original prediction?
  val cfi_mispredicted = Bool()
  // Was the cfi a br?
  val cfi_is_br     = Bool()
  // Was the cfi a jal/jalr?
  val cfi_is_jal  = Bool()
  // Was the cfi a jalr
  val cfi_is_jalr = Bool()
  //val cfi_is_ret  = Bool()

  val ghist = new GlobalHistory
  val lhist = Vec(nBPBanks, UInt(localHistoryLength.W))


  // What did this CFI jump to?
  val target        = UInt(vaddrBitsExtended.W)

  val meta          = UInt(bpdMaxMetaLength.W)
}

// class BlockBranchPredictionRequest(implicit p: Parameters) extends BoomBundle()(p)
// {
//   val pc    = UInt(vaddrBitsExtended.W)
//   val ghist = new GlobalHistory
// }

class BlockBranchPredictionBankResponse(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
  val f1 = new BlockBranchPrediction 
  val f2 = new BlockBranchPrediction
  val f3 = new BlockBranchPrediction
}


abstract class BlockBranchPredictorBank(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFTBParameters
{
  val metaSz = 0
  def nInputs = 1

  val mems: Seq[Tuple3[String, Int, Int]]

  val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(vaddrBitsExtended.W))
    val f0_mask  = Input(UInt(predictWidth.W))
    // Local history not available until end of f1
    val f1_ghist = Input(UInt(globalHistoryLength.W))
    val f1_lhist = Input(UInt(localHistoryLength.W))

    val resp_in = Input(Vec(nInputs, new BlockBranchPredictionBankResponse))
    val resp = Output(new BlockBranchPredictionBankResponse)

    // Store the meta as a UInt, use width inference to figure out the shape
    val f3_meta = Output(UInt(bpdMaxMetaLength.W))

    val f3_fire = Input(Bool())

    val update = Input(Valid(new BlockBranchPredictionUpdate))
  })
  io.resp := io.resp_in(0)

  io.f3_meta := 0.U

  val s0_idx       = fetchIdx(io.f0_pc)
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
  val s0_update_idx = fetchIdx(io.update.bits.pc)
  val s0_update_valid = io.update.valid

  val s1_update     = RegNext(s0_update)
  val s1_update_idx = RegNext(s0_update_idx)
  val s1_update_valid = RegNext(s0_update_valid)

  // bpd.io.resp.ftb_entry := DontCare
}

class BlockBranchPredictor(implicit p:Parameters) extends BoomModule()(p)
    with HasBoomFTBParameters
{

  val io = IO(new Bundle {

    // Requests and responses
    val f0_req = Input(Valid(new BranchPredictionRequest))

    val resp = Output(new Bundle {
      val f1 = new BlockBranchPredictionBundle
      val f2 = new BlockBranchPredictionBundle
      val f3 = new BlockBranchPredictionBundle
      val ftb_entry = new FTBEntry
    })

    val f3_fire = Input(Bool())

    // Update
    val update = Input(Valid(new BranchPredictionBankUpdate))
  })

  io.resp.ftb_entry:=DontCare

}
