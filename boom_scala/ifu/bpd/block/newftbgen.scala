package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}

import boom.common._
import boom.exu._
import boom.util._
import scala.CanEqual.derived

class PredecodeFTBEntryGen(implicit p: Parameters) extends BoomModule with HasBoomFTBParameters {
  val io = IO(new Bundle {
    val start_addr = Input(UInt(vaddrBitsExtended.W))
    val pd = Input(new PredecodeBundle)
    val old_entry = Input(new FTBEntry)
    val cfiIndex = Flipped(Valid(UInt(log2Ceil(predictWidth).W)))
    val target = Input(UInt(vaddrBitsExtended.W))

    val new_entry = Output(new FTBEntry)
    // val need_write_back = Output(Bool())
  })
  val pd = io.pd
  val entry_has_jmp = pd.jmpInfo.valid
  val new_jmp_is_jal  = entry_has_jmp && !pd.jmpInfo.bits(0)
  val new_jmp_is_jalr = entry_has_jmp &&  pd.jmpInfo.bits(0)
  val new_jmp_is_call = entry_has_jmp &&  pd.jmpInfo.bits(1)
  val new_jmp_is_ret  = entry_has_jmp &&  pd.jmpInfo.bits(2)
  val last_jmp_rvi = entry_has_jmp && pd.jmpOffset === (predictWidth-1).U && !pd.rvcMask.last

  def carryPos = log2Ceil(predictWidth)+instOffsetBits
  def getLower(pc: UInt) = pc(carryPos-1, instOffsetBits)
  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))
  init_entry.valid := entry_has_jmp

  when (entry_has_jmp) {
    init_entry.tailSlot.offset := pd.jmpOffset
    init_entry.tailSlot.valid := new_jmp_is_jal || new_jmp_is_jalr
    init_entry.tailSlot.setLowerStatByTarget(io.start_addr, Mux(new_jmp_is_jalr, 
      nextFetch(io.start_addr),
      pd.jalTarget),
    isShare=false)
    val recordedTarget = init_entry.tailSlot.getTarget(io.start_addr)
    init_entry.needExtend := !new_jmp_is_jalr && recordedTarget =/= pd.jalTarget // only care the JAL target for now
  }

  val jmpPft = pd.jmpOffset +& Mux(pd.rvcMask(pd.jmpOffset), 1.U, 2.U) // Pft stands for partial fall through
  val defaultPft = Mux(isLastBankInBlock(io.start_addr), 4.U, 0.U) // when nBanks === 1 , isLastBankInBlock is always false
  val defaultCarry = Mux(isLastBankInBlock(io.start_addr), false.B, true.B)
  init_entry.pftAddr := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft, defaultPft)// FTB： align to boundary
  init_entry.carry   := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft(carryPos-instOffsetBits), defaultCarry)
  init_entry.isJalr := new_jmp_is_jalr
  init_entry.isCall := new_jmp_is_call
  init_entry.isRet  := new_jmp_is_ret

  val oe = io.old_entry
  val derived_from_old_entry = WireInit(oe)
  for (i <- 0 until numBr) {
      val correctBrTargetCond = io.cfiIndex.valid && oe.brValids(i) && io.cfiIndex.bits === oe.brOffset(i)
      when(correctBrTargetCond){
        derived_from_old_entry.allSlotsForBr(i).setLowerStatByTarget(io.start_addr, io.target, i == numBr-1)
      }    
  }

  io.new_entry := Mux(!oe.valid, init_entry, derived_from_old_entry)
}

class CommitFTBEntryGen(implicit p: Parameters) extends BoomModule with HasBoomFTBParameters {
  val io = IO(new Bundle {
    val start_addr = Input(UInt(vaddrBitsExtended.W))
    val target = Input(UInt(vaddrBitsExtended.W))
    val cfiIndex = Flipped(Valid(UInt(log2Ceil(predictWidth).W)))
    val cfiTaken = Input(Bool())
    val old_entry = Input(new FTBEntry)
    val cfi_type  = Input(UInt(CFI_SZ.W))
    val new_entry = Output(new FTBEntry)
    val taken_mask = Output(Vec(numBr, Bool()))
  })

  val hit = io.old_entry.valid
  // --------------------------------------------------------
  // **** Init by Br ****
  // --------------------------------------------------------
  
  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))
  val cfi_is_br = io.cfiIndex.valid && io.cfi_type === CFI_BR
  when (cfi_is_br) {
    init_entry.valid := true.B
    init_entry.getSlotForBr(0).valid := true.B
    init_entry.getSlotForBr(0).offset := io.cfiIndex.bits
    init_entry.getSlotForBr(0).setLowerStatByTarget(io.start_addr, io.target, numBr == 1)
    init_entry.always_taken(0) := true.B // set to always taken on init
    init_entry.needExtend := false.B
  }

  // --------------------------------------------------------
  // **** Insert new Br ****
  // --------------------------------------------------------
  val oe = io.old_entry
  val br_recorded_vec = oe.getBrRecordedVec(io.cfiIndex.bits)
  val br_recorded = br_recorded_vec.asUInt.orR
  val is_new_br = cfi_is_br && !br_recorded
  val new_br_offset = io.cfiIndex.bits
  // vec(i) means new br will be inserted BEFORE old br(i)
  val allBrSlotsVec = oe.allSlotsForBr
  val new_br_insert_onehot = VecInit((0 until numBr).map{
    i => i match {
      case 0 =>
        !allBrSlotsVec(0).valid || new_br_offset < allBrSlotsVec(0).offset
      case idx =>
        allBrSlotsVec(idx-1).valid && new_br_offset > allBrSlotsVec(idx-1).offset &&
        (!allBrSlotsVec(idx).valid || new_br_offset < allBrSlotsVec(idx).offset)
    }
  })

  XSDebug(is_new_br && oe.valid, p"new br inserted for PC 0x${Hexadecimal(io.start_addr)} at 0x${Hexadecimal(new_br_offset)}\n")
  XSDebug(is_new_br && new_br_offset >= oe.pftAddr && oe.valid && !oe.carry, p"new br overflow at 0x${Hexadecimal(new_br_offset)}\n")

  val old_entry_modified = WireInit(io.old_entry)
  for (i <- 0 until numBr) {
    val slot = old_entry_modified.allSlotsForBr(i)
    when (new_br_insert_onehot(i)) {
      slot.valid := true.B
      slot.offset := new_br_offset
      slot.setLowerStatByTarget(io.start_addr, io.target, i == numBr-1)
      old_entry_modified.always_taken(i) := true.B
    }.elsewhen (new_br_offset > oe.allSlotsForBr(i).offset) {
      old_entry_modified.always_taken(i) := false.B
      // all other fields remain unchanged
    }.otherwise {
      // case i == 0, remain unchanged
      if (i != 0) {
        val noNeedToMoveFromFormerSlot = (i == numBr-1).B && !oe.brSlots.last.valid
        when (!noNeedToMoveFromFormerSlot) {
          slot.fromAnotherSlot(oe.allSlotsForBr(i-1))
          old_entry_modified.always_taken(i) := oe.always_taken(i)
        }
      }
    }
  }

  // two circumstances:
  // 1. oe: | br | j  |, new br should be in front of j, thus addr of j should be new pft
  // 2. oe: | br | br |, new br could be anywhere between, thus new pft is the addr of either
  //        the previous last br or the new br
  val may_have_to_replace = oe.noEmptySlotForNewBr
  val pft_need_to_change = is_new_br && may_have_to_replace
  // it should either be the given last br or the new br
  when (pft_need_to_change) {
    val new_pft_offset =
      Mux(!new_br_insert_onehot.asUInt.orR,
        new_br_offset, oe.allSlotsForBr.last.offset)

    // set jmp to invalid
    old_entry_modified.pftAddr := new_pft_offset
    old_entry_modified.carry := false.B // when the new pft is a br, carry cannot be true
    old_entry_modified.isCall := false.B
    old_entry_modified.isRet := false.B
    old_entry_modified.isJalr := false.B
    old_entry_modified.needExtend := false.B
  }


  // --------------------------------------------------------
  // **** Modify Jalr Target ****
  // --------------------------------------------------------
  val cfi_is_jalr =io.cfiIndex.valid && io.cfiIndex.bits === oe.tailSlot.offset && io.cfi_type === CFI_JALR && oe.tailSlot.valid && oe.isJalr
  val old_entry_jmp_target_modified = WireInit(oe)
  val old_target = oe.tailSlot.getTarget(io.start_addr) // may be wrong because we store only 20 lowest bits
  val old_tail_is_jmp = !oe.tailSlot.sharing
  val jalr_target_modified = cfi_is_jalr && (old_target =/= io.target) && old_tail_is_jmp // TODO: pass full jalr target
  when (jalr_target_modified) {
    old_entry_jmp_target_modified.setByJmpTarget(io.start_addr, Mux(io.target === 0.U, nextFetch(io.start_addr), io.target))
    old_entry_jmp_target_modified.always_taken := 0.U.asTypeOf(Vec(numBr, Bool()))
    // old_entry_jmp_target_modified.br_mask := pd.brMask
    val recordedTarget = old_entry_jmp_target_modified.tailSlot.getTarget(io.start_addr)
    old_entry_jmp_target_modified.needExtend := recordedTarget =/= io.target && !oe.isRet
  }

  // --------------------------------------------------------
  // **** Modify Always Taken ****
  // --------------------------------------------------------
  val old_entry_always_taken = WireInit(oe)
  val always_taken_modified_vec = Wire(Vec(numBr, Bool())) // whether modified or not
  for (i <- 0 until numBr) {
    old_entry_always_taken.always_taken(i) :=
      oe.always_taken(i) && io.cfiIndex.valid && oe.brValids(i) && io.cfiIndex.bits === oe.brOffset(i) && io.cfiTaken
    always_taken_modified_vec(i) := oe.always_taken(i) && !old_entry_always_taken.always_taken(i)
  }
  val always_taken_modified = always_taken_modified_vec.reduce(_||_)

  val derived_from_old_entry =
    Mux(is_new_br, old_entry_modified,
      Mux(jalr_target_modified, old_entry_jmp_target_modified, old_entry_always_taken))

  io.new_entry := Mux(!hit, init_entry, derived_from_old_entry)

  io.taken_mask := VecInit((io.new_entry.brOffset zip io.new_entry.validSlots).map{
    case (off, v) => io.cfiIndex.bits === off && io.cfiIndex.valid && v && io.cfiTaken
  })
}