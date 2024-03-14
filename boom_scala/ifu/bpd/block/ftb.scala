package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import boom.common._
import boom.util._

import scala.math.min

trait FTBParams extends HasBoomFTBParameters {
  val numEntries = 2048
  val numWays    = 4
  val numSets    = numEntries/numWays // 256
  val tagSize    = 20



  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 35
}  


class FtbSlot(val offsetLen: Int, val subOffsetLen: Option[Int] = None)(implicit p: Parameters) extends BoomBundle with FTBParams {
  if (subOffsetLen.isDefined) {
    require(subOffsetLen.get <= offsetLen)
  }
  val offset  = UInt(log2Ceil(predictWidth).W)
  val lower   = UInt(offsetLen.W)
  val tarStat = UInt(TAR_STAT_SZ.W)
  val sharing = Bool()
  val valid   = Bool()

  def setLowerStatByTarget(pc: UInt, target: UInt, isShare: Boolean) = {
    def getTargetStatByHigher(pc_higher: UInt, target_higher: UInt) =
      Mux(target_higher > pc_higher, TAR_OVF,
        Mux(target_higher < pc_higher, TAR_UDF, TAR_FIT))
    def getLowerByTarget(target: UInt, offsetLen: Int) = target(offsetLen, 1)
    val offLen = if (isShare) this.subOffsetLen.get else this.offsetLen
    val pc_higher = pc(vaddrBitsExtended-1, offLen+1)
    val target_higher = target(vaddrBitsExtended-1, offLen+1)
    val stat = getTargetStatByHigher(pc_higher, target_higher)
    val lower = ZeroExt(getLowerByTarget(target, offLen), this.offsetLen)
    this.lower := lower
    this.tarStat := stat
    this.sharing := isShare.B
  }

  def getTarget(pc: UInt, last_stage: Option[Tuple2[UInt, Bool]] = None) = {
    def getTarget(offLen: Int)(pc: UInt, lower: UInt, stat: UInt,
      last_stage: Option[Tuple2[UInt, Bool]] = None) = {
      val h                = pc(vaddrBitsExtended - 1, offLen + 1)
      val higher           = Wire(UInt((vaddrBitsExtended - offLen - 1).W))
      val higher_plus_one  = Wire(UInt((vaddrBitsExtended - offLen - 1).W))
      val higher_minus_one = Wire(UInt((vaddrBitsExtended-offLen-1).W))

      // Switch between previous stage pc and current stage pc
      // Give flexibility for timing
      if (last_stage.isDefined) {
        val last_stage_pc = last_stage.get._1
        val last_stage_pc_h = last_stage_pc(vaddrBitsExtended-1, offLen+1)
        val stage_en = last_stage.get._2
        higher := RegEnable(last_stage_pc_h, stage_en)
        higher_plus_one := RegEnable(last_stage_pc_h+1.U, stage_en)
        higher_minus_one := RegEnable(last_stage_pc_h-1.U, stage_en)
      } else {
        higher := h
        higher_plus_one := h + 1.U
        higher_minus_one := h - 1.U
      }
      val target =
        Cat(
          Mux1H(Seq(
            (stat === TAR_OVF, higher_plus_one),
            (stat === TAR_UDF, higher_minus_one),
            (stat === TAR_FIT, higher),
          )),
          lower(offLen-1, 0), 0.U(1.W)
        )
      require(target.getWidth == vaddrBitsExtended)
      require(offLen != 0)
      target
    }
    if (subOffsetLen.isDefined)
      Mux(sharing,
        getTarget(subOffsetLen.get)(pc, lower, tarStat, last_stage),
        getTarget(offsetLen)(pc, lower, tarStat, last_stage)
      )
    else
      getTarget(offsetLen)(pc, lower, tarStat, last_stage)
  }
  def fromAnotherSlot(that: FtbSlot) = {
    require(
      this.offsetLen > that.offsetLen && this.subOffsetLen.map(_ == that.offsetLen).getOrElse(true) ||
      this.offsetLen == that.offsetLen
    )
    this.offset := that.offset
    this.tarStat := that.tarStat
    this.sharing := (this.offsetLen > that.offsetLen && that.offsetLen == this.subOffsetLen.get).B
    this.valid := that.valid
    this.lower := ZeroExt(that.lower, this.offsetLen)
  }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"v=${valid}, offset=${offset}, lower=${lower}, tarStat=${tarStat}, sharing=${sharing}\n")
  }

  def display(cond : Bool , prefix : chisel3.Printable): Unit = {
    XSDebug(cond, prefix+p"v: ${valid} offset: ${offset} lower: ${lower} tarStat: ${tarStat} sharing: ${sharing}\n")
  }

}

class FTBEntry(implicit p: Parameters) extends BoomBundle with FTBParams with BPUUtils{


  val valid       = Bool()

  val brSlots = Vec(numBrSlot, new FtbSlot(BR_OFFSET_LEN))

  val tailSlot = new FtbSlot(JMP_OFFSET_LEN, Some(BR_OFFSET_LEN))

  // Partial Fall-Through Address
  val pftAddr     = UInt(log2Up(predictWidth).W)
  val carry       = Bool()

  val isCall      = Bool()
  val isRet       = Bool()
  val isJalr      = Bool()

  val last_may_be_rvi_call = Bool()

  val always_taken = Vec(numBr, Bool())

  val br_mask      = Vec(predictWidth, Bool())

  def getSlotForBr(idx: Int): FtbSlot = {
    require(idx <= numBr-1)
    (idx, numBr) match {
      case (i, n) if i == n-1 => this.tailSlot
      case _ => this.brSlots(idx)
    }
  }
  def allSlotsForBr = {
    (0 until numBr).map(getSlotForBr(_))
  }
  def setByBrTarget(brIdx: Int, pc: UInt, target: UInt) = {
    val slot = getSlotForBr(brIdx)
    slot.setLowerStatByTarget(pc, target, brIdx == numBr-1)
  }
  def setByJmpTarget(pc: UInt, target: UInt) = {
    this.tailSlot.setLowerStatByTarget(pc, target, false)
  }

  def getTargetVec(pc: UInt, last_stage: Option[Tuple2[UInt, Bool]] = None) = {
    VecInit((brSlots :+ tailSlot).map(_.getTarget(pc, last_stage)))
  }

  def getOffsetVec = VecInit(brSlots.map(_.offset) :+ tailSlot.offset)
  def isJal = !isJalr
  def getFallThrough(pc: UInt, last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None) = {
    if (last_stage_entry.isDefined) {
      var stashed_carry = RegEnable(last_stage_entry.get._1.carry, last_stage_entry.get._2)
      getFallThroughAddr(pc, stashed_carry, pftAddr)
    } else {
      getFallThroughAddr(pc, carry, pftAddr)
    }
  }

  def hasBr(offset: UInt) =
    brSlots.map{ s => s.valid && s.offset <= offset}.reduce(_||_) ||
    (tailSlot.valid && tailSlot.offset <= offset && tailSlot.sharing)

  def getBrMaskByOffset(offset: UInt) =
    brSlots.map{ s => s.valid && s.offset <= offset } :+
    (tailSlot.valid && tailSlot.offset <= offset && tailSlot.sharing)

  def getSlotIdxfromOffset(offset: UInt) = {
    val mask = brSlots.map{ s => s.valid && s.offset === offset } :+
      (tailSlot.valid && tailSlot.offset === offset)
    PriorityEncoder(mask)
  }

  def getBrRecordedVec(offset: UInt) = {
    VecInit(
      brSlots.map(s => s.valid && s.offset === offset) :+
      (tailSlot.valid && tailSlot.offset === offset && tailSlot.sharing)
    )
  }

  def brIsSaved(offset: UInt) = getBrRecordedVec(offset).reduce(_||_)

  def brValids = {
    VecInit(
      brSlots.map(_.valid) :+ (tailSlot.valid && tailSlot.sharing)
    )
  }

  def noEmptySlotForNewBr = {
    VecInit(brSlots.map(_.valid) :+ tailSlot.valid).reduce(_&&_)
  }

  def newBrCanNotInsert(offset: UInt) = {
    val lastSlotForBr = tailSlot
    lastSlotForBr.valid && lastSlotForBr.offset < offset
  }

  def jmpValid = {
    tailSlot.valid && !tailSlot.sharing
  }

  def brOffset = {
    VecInit(brSlots.map(_.offset) :+ tailSlot.offset)
  }

  def validSlots = {
    VecInit(brSlots.map(_.valid) :+ tailSlot.valid)
  }

  def hasValidSlot = {
    validSlots.reduce(_||_)
  }

  def display(cond: Bool, decimal: Boolean = false): Unit = {
    // XSDebug(cond, p"-----------FTB entry----------- \n")
    val prefix = p"FTBEntry:"
    XSDebug(cond, prefix + p"v: ${valid}\n")
    for(i <- 0 until numBrSlot) {
      allSlotsForBr(i).display(cond, prefix)
    }
    tailSlot.display(cond,prefix)
    if(decimal) {
      XSDebug(cond,prefix+ p"pftAddr: ${pftAddr} carry: $carry\n")
    } else {
      XSDebug(cond,prefix+ p"pftAddr: ${Hexadecimal(pftAddr)}, carry: $carry\n")
    }
    XSDebug(cond,prefix+ p"isCall: $isCall isRet: $isRet isjalr: $isJalr last_may_be_rvi_call: $last_may_be_rvi_call\n")
    if(decimal){
      XSDebug(cond,prefix+ p"always_taken: ${always_taken.asUInt}\n")
    }
    else{
      XSDebug(cond,prefix+ p"always_taken: ${Binary(always_taken.asUInt)}\n")
    }
    // XSDebug(cond, p"------------------------------- \n")
  }

}

class FTBEntryWithTag(implicit p: Parameters) extends BoomBundle with FTBParams {
  val entry = new FTBEntry
  val tag = UInt(tagSize.W)
  def display(cond: Bool, decimal: Boolean = false): Unit = {
    entry.display(cond)
    if(decimal){
      XSDebug(cond, p"ftb entry tag: ${tag}\n")
    }
    else{
      XSDebug(cond, p"ftb entry tag: ${Hexadecimal(tag)}\n")
    }
  }
}

class FTBMeta(implicit p: Parameters) extends BoomBundle with FTBParams {
  val writeWay = UInt(log2Ceil(numWays).W)
  val hit = Bool()
  // val pred_cycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
  val replacer = ReplacementPolicy.fromString("plru", 4)
}

object FTBMeta {
  def apply(writeWay: UInt, hit: Bool, pred_cycle: UInt)(implicit p: Parameters): FTBMeta = {
    val e = Wire(new FTBMeta)
    e.writeWay := writeWay
    e.hit := hit
    // e.pred_cycle.map(_ := pred_cycle)
    e
  }
}


class FTB(implicit p: Parameters) extends BlockPredictorBank with FTBParams{
  require(isPow2(numSets))
  val s1_meta = Wire(new FTBMeta)
  val debug_cycle = RegInit(0.U(64.W))
  debug_cycle := debug_cycle + 1.U

  override val metaSz = s1_meta.asUInt.getWidth
  override val nSets = numSets
  override val nWays = numWays
  val ftbEntrySz = io.resp.last_stage_entry.asUInt.getWidth

  val doing_reset = RegInit(true.B)
  val reset_idx   = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }


  val ftbAddr = new TableAddr(log2Up(numSets), 1, tagSize)

  val tag = Seq.fill(nWays) {SyncReadMem(nSets, UInt(tagSize.W))}
  val ftb = Seq.fill(nWays) {SyncReadMem(nSets, new FTBEntry)}

  //TODO: val mems = ...

  val mems = (((0 until nWays) map ({w:Int => Seq(
    (f"ftb_tag_way$w", nSets, tagSize),
    (f"ftb_data_way$w", nSets, ftbEntrySz))})).flatten)

  val r_s0_idx = ftbAddr.getIdx(s0_pc)
  val s0_tag = ftbAddr.getTag(s0_pc)
  val s1_req_rftb = VecInit(ftb.map(_.read(r_s0_idx, s0_valid)).map(_.asTypeOf(new FTBEntry)))
  val s1_req_rtag = VecInit(tag.map(_.read(r_s0_idx, s0_valid)))
  val s1_req_tag = RegNext(s0_tag)

  val s1_hit_ohs = VecInit( (0 until nWays) map { i =>
    val hit = s1_req_rtag(i) === s1_req_tag
    hit
  })
  val s1_hit = s1_hit_ohs.reduce(_||_)
  val s1_hit_way = PriorityEncoder(s1_hit_ohs)
  val s1_ftb_entry = Mux(s1_hit, s1_req_rftb(s1_hit_way), 0.U.asTypeOf(new FTBEntry))
  val s1_hit_fallthrough_error = false.B

  val alloc_way = if (nWays > 1) {
    // val r_metas = Cat(VecInit(s1_req_rtag.map { w => VecInit(w.map(_.tag)) }).asUInt, s1_req_tag(tagSz-1,0))
    val r_metas = Cat(s1_req_rtag.asUInt, s1_req_tag(tagSize - 1, 0))

    val l = log2Ceil(nWays)
    val nChunks = (r_metas.getWidth + l - 1) / l
    val chunks = (0 until nChunks) map { i =>
      r_metas(min((i+1)*l, r_metas.getWidth)-1, i*l)
    }
    chunks.reduce(_^_)
  } else {
    0.U
  }

  s1_meta.hit := s1_hit && !s1_hit_fallthrough_error
  s1_meta.writeWay := Mux(s1_hit, s1_hit_way, alloc_way)

  io.resp.f2 := io.resp_in(0).f2
  io.resp.f3 := io.resp_in(0).f3
  when(RegNext(s1_hit && !s1_hit_fallthrough_error)) {
    io.resp.f2.fromFtbEntry(RegNext(s1_ftb_entry), RegNext(s1_pc))
    io.resp.f2.hit := true.B
    for (i <- 0 until numBr) {
      io.resp.f2.perfs(i).ftb_hit := true.B
      when(RegNext(s1_ftb_entry.always_taken(i))) {
        io.resp.f2.br_taken_mask(i) := true.B
      }
    }
  }
  if(enableFTBPredictPrint){
    val cond = true.B
    XSDebug(cond, p"-------FTB predict for PC : 0x${Hexadecimal(RegNext(s1_pc))}-------\n")
    XSDebug(cond, p"hit: ${RegNext(s1_hit)} hit_way: ${RegNext(s1_hit_way)} alloc_way: ${RegNext(alloc_way)}\n")
    RegNext(s1_ftb_entry).display(cond)
    // XSDebug(cond, p"--input--\n")
    // io.resp_in(0).f2.display(cond)
    // XSDebug(cond, p"--output--\n")
    // io.resp.f2.display(cond)
    XSDebug(cond, p"-----------------------------------\n")
  }

  if(enableFTBJsonPredictPrint){
    val cond = true.B
    // {
    //   "cycle": "1",
    //   "action": "insert",
    //   "pc": "0x80002038",
    //   "ftb_hit": "0",
    //   "index": "0",
    //   "tag": "0x0",
    //   "way": "1"
    // }
    when(cond){
      printf("{\"cycle\": \"%d\", \"action\": \"predict\", \"pc\": \"0x%x\", \"ftb_hit\": \"%d\", \"index\": \"%d\", \"tag\": \"0x%x\", \"way\": \"%d\"},\n", RegNext(debug_cycle),s1_pc, s1_meta.hit, RegNext(r_s0_idx), s1_req_tag, s1_meta.writeWay)
    }
  }
  

  when(RegNext(RegNext(s1_hit && !s1_hit_fallthrough_error))) {
    io.resp.f3.fromFtbEntry(RegNext(RegNext(s1_ftb_entry)), RegNext(RegNext(s1_pc)))
    io.resp.f3.hit := true.B
    for(i <- 0 until numBr) {
      io.resp.f3.perfs(i).ftb_hit := true.B
      when(RegNext(RegNext(s1_ftb_entry.always_taken(i))) && RegNext(RegNext(s1_ftb_entry.validSlots(i)))) {
        io.resp.f3.br_taken_mask(i) := true.B
      }
    }
  }
  io.resp.f3_meta := RegNext(RegNext(s1_meta)).asUInt
  io.resp.last_stage_entry := Mux(RegNext(RegNext(s1_hit && !s1_hit_fallthrough_error)), RegNext(RegNext(s1_ftb_entry)), io.resp_in(0).last_stage_entry)


  /********************** update ***********************/

  // s0
  val u = io.update
  val u_meta = u.bits.meta.asTypeOf(new FTBMeta)
  val ftbUpdateAddr = new TableAddr(log2Up(numSets), 1, tagSize)
  val u_s0_tag = ftbUpdateAddr.getTag(u.bits.pc)
  val u_s0_idx = ftbUpdateAddr.getIdx(u.bits.pc)
  val u_s0_entry_valid_slot_mask = u.bits.ftb_entry.brValids
  val u_s0_br_update_valids =
    VecInit((0 until numBr).map(w =>
      u.valid &&
      !u.bits.is_btb_mispredict_update &&
      u.bits.ftb_entry.valid &&
      u_s0_entry_valid_slot_mask(w) &&  
      u.bits.ftb_entry.brValids(w) &&
      !(PriorityEncoder(u.bits.br_taken_mask) < w.U))) // TODO: temporarily disable always taken

  if(enableFTBUpdateDetailPrint || enableWatchPC){
    val printCond = u.valid
    val watchCond = u.valid && u.bits.pc === watchPC.U
    val cond = if(enableFTBUpdateDetailPrint) printCond else watchCond
    XSDebug(cond, p"-------FTB update entry for PC : 0x${Hexadecimal(u.bits.pc)}-------\n")
    XSDebug(cond, p"index: ${u_s0_idx} way: ${u_meta.writeWay} tag:${u_s0_tag(tagSize - 1, 0)}\n")
    u.bits.display(cond)
    XSDebug(cond, p"-----------------------------------\n")
  }

    
  // s1
  val u_s1_pc = RegNext(u.bits.pc)
  val u_s1_meta = RegNext(u_meta)
  val u_s1_valid = RegNext(u.valid)
  val u_s1_commit_valid = RegNext(u.valid && !u.bits.is_btb_mispredict_update)
  val u_s1_tag       = RegNext(u_s0_tag)
  val u_s1_idx = RegNext(u_s0_idx)
  val u_s1_ftb_entry = RegEnable(u.bits.ftb_entry, u.valid)
  val u_s1_ftb_entry_empty = !u_s1_ftb_entry.valid || !u_s1_ftb_entry.hasValidSlot // if the entry is invalid or contains no valid slot

  if(enableFTBJsonInsertPrint){
    // val cond = u.valid && u.bits.ftb_entry.valid && u.bits.ftb_entry.hasValidSlot
    val cond = u_s1_valid && u_s1_ftb_entry.valid && u_s1_ftb_entry.hasValidSlot
    // {
    //   "cycle": "1",
    //   "action": "insert",
    //   "pc": "0x80002038",
    //   "ftb_hit": "0",
    //   "index": "0",
    //   "tag": "0x0",
    //   "way": "1"
    // }
    // XSDebug(cond, p"{{\"cycle\": \"${debug_cycle}\", \"action\": \"insert\", \"pc\": \"0x${Hexadecimal(u.bits.pc)}\", \"ftb_hit\": \"${u_meta.hit}\"}}\n")
    when(cond){
      // printf("{\"cycle\": \"%d\", \"action\": \"insert\", \"pc\": \"0x%x\", \"ftb_hit\": \"%d\"},\n", debug_cycle, u.bits.pc, u_meta.hit)
      printf("{\"cycle\": \"%d\", \"action\": \"insert\", \"pc\": \"0x%x\", \"ftb_hit\": \"%d\", \"index\": \"%d\", \"tag\": \"0x%x\", \"way\": \"%d\"},\n", debug_cycle,RegNext(u.bits.pc), u_s1_meta.hit, u_s1_idx, u_s1_tag, u_s1_meta.writeWay)
    }
  }
  for ( w <- 0 until nWays){
    when (doing_reset || ((u_s1_meta.writeWay === w.U || (w == 0 && nWays == 1).B) && !u_s1_ftb_entry_empty && u_s1_valid)) {
      ftb(w).write(
        Mux(doing_reset, reset_idx, u_s1_idx),
        Mux(doing_reset, 0.U.asTypeOf(new FTBEntry), u_s1_ftb_entry)
      )
      tag(w).write(
        Mux(doing_reset, reset_idx, u_s1_idx),
        Mux(doing_reset, 0.U.asTypeOf(UInt(tagSize.W)), u_s1_tag)
      )
    }
  }

}
