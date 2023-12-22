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
  val numSets    = numEntries/numWays // 512
  val tagSize    = 20



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

  val last_may_be_rvi_call = Bool()

  val always_taken = Vec(numBr, Bool())

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

  def display(cond: Bool, decimal: Boolean = true): Unit = {
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

