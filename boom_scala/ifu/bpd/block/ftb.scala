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
  val extendedNSets = 128



  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20
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

  val needExtend = Bool()

  val always_taken = Vec(numBr, Bool())

  // val br_mask      = Vec(predictWidth, Bool())

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

  def isFull = {
    validSlots.reduce(_&&_)
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
    XSDebug(cond,prefix+ p"isCall: $isCall isRet: $isRet isjalr: $isJalr\n")
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
}

object FTBMeta {
  def apply(writeWay: UInt, hit: Bool)(implicit p: Parameters): FTBMeta = {
    val e = Wire(new FTBMeta)
    e.writeWay := writeWay
    e.hit := hit
    // e.pred_cycle.map(_ := pred_cycle)
    e
  }
}

class FTBTag(implicit p: Parameters) extends BoomBundle with FTBParams{
  val tag = UInt(tagSize.W)
  val valid = Bool()
}

class FTBImpl(implicit p: Parameters) extends BoomModule()(p) with FTBParams {
  val io = IO(new Bundle {
    val update_pc = Input(Valid(UInt(vaddrBitsExtended.W)))
    val update_entry = Input(new FTBEntry)
    val update_meta = Input(new FTBMeta)
    val req_pc = Input(Valid(UInt(vaddrBitsExtended.W)))
    val resp = Output(new FTBEntry)
    val meta = Output(new FTBMeta)
  })

  override val nSets = numSets
  override val nWays = numWays

  val doing_reset = RegInit(true.B)
  val reset_idx   = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }

  val ftbAddr = new FTBTableAddr(log2Up(numSets), 1, tagSize)
  val tag = Seq.fill(nWays) {SyncReadMem(nSets, UInt(tagSize.W))}
  val ftb = Seq.fill(nWays) {SyncReadMem(nSets, new FTBEntry)}

  // --------------------------------------------------------
  // **** (S0) ****
  // --------------------------------------------------------
  val s0_valid = io.req_pc.valid
  val r_s0_idx = ftbAddr.getIdx(io.req_pc.bits)
  val s0_tag = ftbAddr.getTag(io.req_pc.bits)

  val ftbUpdateAddr = new FTBTableAddr(log2Up(numSets), 1, tagSize)
  val u_s0_tag = ftbUpdateAddr.getTag(io.update_pc.bits)
  val u_s0_idx = ftbUpdateAddr.getIdx(io.update_pc.bits)
  val u_s0_ftb_entry = io.update_entry
  val u_s0_valid = io.update_pc.valid
  val u_s0_meta  = io.update_meta

  // --------------------------------------------------------
  // **** (S1) ****
  // --------------------------------------------------------
  val s1_req_rftb = VecInit(ftb.map(_.read(r_s0_idx, s0_valid)).map(_.asTypeOf(new FTBEntry)))
  val s1_req_rtag = VecInit(tag.map(_.read(r_s0_idx, s0_valid)))
  val s1_req_ridx = RegNext(r_s0_idx)
  val s1_req_tag = RegNext(s0_tag)

  val s1_hit_ohs = VecInit( (0 until nWays) map { i =>
    val hit_valid = s1_req_rtag(i) === s1_req_tag && s1_req_rftb(i).valid
    hit_valid
  })
  val s1_valids = VecInit((0 until nWays) map { i =>
    val valid = s1_req_rftb(i).valid
    valid
  })
  val s1_hit = s1_hit_ohs.reduce(_||_)
  XSDebug(PopCount(s1_hit_ohs) > 1.U, p"PC 0x${Hexadecimal(io.req_pc.bits)} has ${PopCount(s1_hit_ohs)} hits\n")
  val s1_hit_way = PriorityEncoder(s1_hit_ohs)
  val s1_ftb_entry = Mux(s1_hit, s1_req_rftb(s1_hit_way), 0.U.asTypeOf(new FTBEntry))

  io.resp := RegNext(s1_ftb_entry)
  io.meta := RegNext(FTBMeta(s1_hit_way, s1_hit))

  /*-------- Update --------*/
  val u_s1_idx = RegNext(u_s0_idx)
  val u_s1_tag = RegNext(u_s0_tag)
  val u_s1_meta = RegNext(u_s0_meta)
  val u_s1_valid = RegNext(u_s0_valid)
  val u_s1_ftb_entry = RegNext(u_s0_ftb_entry)
  val u_s1_req_rftb = VecInit(ftb.map(_.read(u_s0_idx, u_s0_valid)).map(_.asTypeOf(new FTBEntry)))
  val u_s1_req_rtag = VecInit(tag.map(_.read(u_s0_idx, u_s0_valid)))
  val u_s1_req_valids = VecInit((0 until nWays) map { i =>
    val valid = u_s1_req_rftb(i).valid
    valid
  })
  val u_s1_req_hits = VecInit((0 until nWays) map { i =>
    val hit_valid = u_s1_req_rtag(i) === u_s1_tag && u_s1_req_rftb(i).valid
    hit_valid
  })
  val u_s1_hit_way = PriorityEncoder(u_s1_req_hits)
  val u_s1_hit = u_s1_req_hits.reduce(_||_)

  val alloc_way = if (nWays > 1) {
    val r_metas = Cat(u_s1_req_rtag.asUInt, u_s1_tag(tagSize - 1, 0))

    val l = log2Ceil(nWays)
    val nChunks = (r_metas.getWidth + l - 1) / l
    val chunks = (0 until nChunks) map { i =>
      r_metas(min((i+1)*l, r_metas.getWidth)-1, i*l)
    }

    val valids = u_s1_req_valids.asUInt
    val valid = valids.andR
    
    val w = Wire(UInt(log2Ceil(numWays).W))
    w := Mux(valid, chunks.reduce(_^_), PriorityEncoder(~valids))

    w
  } else {
    0.U
  }

  val u_s1_write_way = Mux(u_s1_meta.hit,
    u_s1_meta.writeWay,
    Mux(u_s1_hit, u_s1_hit_way, alloc_way)
   )

  val u_s2_write_way = RegNext(u_s1_write_way)
  for ( w <- 0 until nWays){
    when (doing_reset || ((u_s2_write_way === w.U || (w == 0 && nWays == 1).B) && RegNext(u_s1_valid))) {
      ftb(w).write(
        Mux(doing_reset, reset_idx, RegNext(u_s1_idx)),
        Mux(doing_reset, 0.U.asTypeOf(new FTBEntry), RegNext(u_s1_ftb_entry))
      )
      tag(w).write(
        Mux(doing_reset, reset_idx, RegNext(u_s1_idx)),
        Mux(doing_reset, 0.U.asTypeOf(UInt(tagSize.W)), RegNext(u_s1_tag))
      )
    }
  }

}


class FTB(implicit p: Parameters) extends BlockPredictorBank with FTBParams{
  require(isPow2(numSets))
  val s1_meta = Wire(new FTBMeta)
  s1_meta := DontCare
  val debug_cycle = RegInit(0.U(64.W))
  debug_cycle := debug_cycle + 1.U

  override val metaSz = s1_meta.asUInt.getWidth
  override val nSets = numSets
  override val nWays = numWays
  val ftbEntrySz = io.resp.last_stage_entry.asUInt.getWidth

  val impl = Module(new FTBImpl())

  val fauftbImpl = Module(new FauFTBImpl)

  val ebtb     = SyncReadMem(extendedNSets, UInt(vaddrBitsExtended.W))

  val mems = (((0 until nWays) map ({w:Int => Seq(
    (f"ftb_tag_way$w", nSets, tagSize),
    (f"ftb_data_way$w", nSets, ftbEntrySz))})).flatten++ Seq(("ebtb", extendedNSets, vaddrBitsExtended)))

  // --------------------------------------------------------
  // **** (S0) ****
  // --------------------------------------------------------
  val u = io.update
  val u_meta = u.bits.meta.asTypeOf(new FTBMeta)
  val u_s0_valid = u.valid && u.bits.ftb_entry.valid && u.bits.ftb_entry.hasValidSlot && (u.bits.is_commit_update || u.bits.is_btb_mispredict_update)
  val u_s0_commit_valid = u.valid && u.bits.is_commit_update && u.bits.ftb_entry.valid && u.bits.ftb_entry.hasValidSlot
  val u_s0_need_extend = u_s0_commit_valid && u.bits.ftb_entry.needExtend

  val u_s0_entry_valid_slot_mask = u.bits.ftb_entry.brValids
  val u_s0_br_update_valids =
    VecInit((0 until numBr).map(w =>
      u.valid &&
      // !u.bits.is_btb_mispredict_update &&
      u.bits.is_commit_update &&
      u.bits.ftb_entry.valid &&
      u_s0_entry_valid_slot_mask(w) &&  
      u.bits.ftb_entry.brValids(w) &&
      !(PriorityEncoder(u.bits.br_taken_mask) < w.U))) // TODO: temporarily disable always taken

  fauftbImpl.io.update_pc.valid := u.valid
  fauftbImpl.io.update_pc.bits := io.update.bits.pc
  fauftbImpl.io.update_entry := io.update.bits.ftb_entry
  fauftbImpl.io.update_meta := io.update.bits.meta.asTypeOf(new FTBMeta)
  fauftbImpl.io.is_commit_update := io.update.bits.is_commit_update
  fauftbImpl.io.is_btb_mispredict_update := io.update.bits.is_btb_mispredict_update
  fauftbImpl.io.br_taken_mask := io.update.bits.br_taken_mask
  fauftbImpl.io.br_update_mask := u_s0_br_update_valids
  fauftbImpl.io.req_pc.valid := s0_valid
  fauftbImpl.io.req_pc.bits := s0_pc

  val evict_valid = fauftbImpl.io.evict_entry.valid
  val evicted_entry = fauftbImpl.io.evict_entry.bits
  val evicted_pc = fauftbImpl.io.evict_tag << instOffsetBits
  impl.io.update_pc.valid := evict_valid
  impl.io.update_pc.bits := evicted_pc
  impl.io.update_entry := evicted_entry
  impl.io.update_meta := DontCare
  impl.io.req_pc.valid := s0_valid
  impl.io.req_pc.bits := s0_pc

  when(u_s0_need_extend){
    ebtb.write(s0_update_idx, u.bits.target)
  }

  // --------------------------------------------------------
  // **** (S1) ****
  // --------------------------------------------------------

  val s1_req_rebtb = ebtb.read(s0_idx, s0_valid)

  val s1_ftb_entry = fauftbImpl.io.resp_entry

  val empty_pred = Wire(new BlockPrediction)
  empty_pred.makeDefault(s1_pc)
  empty_pred.perfs := DontCare
  val hit_pred = Wire(new BlockPrediction)
  hit_pred.fromFtbEntry(s1_ftb_entry, s1_pc)
  hit_pred.perfs := DontCare
  hit_pred.br_taken_mask := fauftbImpl.io.resp_br_taken
  hit_pred.hit := s1_ftb_entry.valid

  io.resp.f1 := Mux(s1_ftb_entry.valid, hit_pred, empty_pred)
  io.resp.f1.hit := s1_ftb_entry.valid
  for ( i <- 0 until numBr){
    io.resp.f1.perfs(i).fauftb_hit := s1_ftb_entry.valid
    io.resp.f1.perfs(i).fauftb_taken := fauftbImpl.io.resp_br_taken(i)

    io.resp.f1.perfs(i).ftb_entry_hit := false.B
    io.resp.f1.perfs(i).ftb_slot_hit  := false.B
    io.resp.f1.perfs(i).bim_taken := false.B

    io.resp.f1.perfs(i).tage_hit := false.B
    io.resp.f1.perfs(i).tage_taken := false.B
  }
  // --------------------------------------------------------
  // **** (S2) ****
  // --------------------------------------------------------

  val s2_entry = Mux(RegNext(s1_ftb_entry.valid), RegNext(s1_ftb_entry), impl.io.resp)

  io.resp.f2 := RegNext(io.resp.f1)
  for (i <- 0 until numBr) {
    when(io.resp_in(0).f2.br_taken_mask(i) || (s2_entry.valid && s2_entry.always_taken(i))) {
      io.resp.f2.br_taken_mask(i) := true.B
    }
  }
  io.resp.f2.br_taken_mask := io.resp_in(0).f2.br_taken_mask
  io.resp.f2.jalr_target.bits := Mux(RegNext(s1_req_rebtb)=/=0.U, RegNext(s1_req_rebtb), nextFetch(s2_pc))
  when(s2_entry.valid) {
    io.resp.f2.fromFtbEntry(s2_entry, s2_pc)
    io.resp.f2.jalr_target.valid := s2_entry.needExtend
    for (i <- 0 until numBr) {
      io.resp.f2.perfs(i).ftb_entry_hit := true.B
    }
  }
  io.resp.f2.hit := s2_entry.valid


  // --------------------------------------------------------
  // **** (S3) ****
  // --------------------------------------------------------
  io.resp.f3 := RegNext(io.resp.f2)

  io.resp.f3_meta := RegNext(impl.io.meta.asUInt)
  io.resp.last_stage_entry := RegNext(s2_entry)
}
