package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}

import boom.common._
import boom.exu._
import boom.util._
class FTBEntryGen(implicit p: Parameters) extends BoomModule with HasBoomFTBParameters {
  val io = IO(new Bundle {
    val start_addr = Input(UInt(vaddrBitsExtended.W))
    val old_entry = Input(new FTBEntry)
    val pd = Input(new PredecodeBundle)
    val cfiIndex = Flipped(Valid(UInt(log2Ceil(predictWidth).W)))
    val target = Input(UInt(vaddrBitsExtended.W))
    val hit = Input(Bool())
    val cfiTaken = Input(Bool())
    val isF3Correction = Input(Bool())
    val mispredict_vec = Input(Vec(predictWidth, Bool())) // seems this input does not affect the new FTB entry gen

    val new_entry = Output(new FTBEntry)
    val new_br_insert_pos = Output(Vec(numBr, Bool()))
    val taken_mask = Output(Vec(numBr, Bool()))
    val jmp_taken = Output(Bool())
    val mispred_mask = Output(Vec(numBr+1, Bool()))

    // for perf counters
    val is_init_entry = Output(Bool())
    val is_old_entry = Output(Bool())
    val is_new_br = Output(Bool())
    val is_jalr_target_modified = Output(Bool())
    val is_always_taken_modified = Output(Bool())
    val is_br_full = Output(Bool())
  })
  // no mispredictions detected at predecode

  if(enableWatchPC){
    val cond = io.start_addr === watchPC.U && io.cfiIndex.valid
    XSDebug(cond, p"---------------FTBEntryGen---------------\n")
    XSDebug(cond, p"start_addr: 0x${Hexadecimal(io.start_addr)}\n")
    XSDebug(cond, p"target: 0x${Hexadecimal(io.target)}\n")
    XSDebug(cond, p"cfiIndex: ${io.cfiIndex.bits} valid: ${io.cfiIndex.valid} \n")
    io.old_entry.display(cond)
    XSDebug(cond, p"------------------------------------------\n")
  }
  val hit = io.old_entry.valid
  val pd = io.pd

  when(io.cfiIndex.valid && pd.jmpOffset === io.cfiIndex.bits && pd.jmpInfo.valid){
    assert(io.cfiTaken || io.isF3Correction, "when not from F3 correction, jmp should be taken")
  }
  when(io.cfiIndex.valid && pd.jmpInfo.valid){
    assert(io.isF3Correction || !(io.cfiIndex.bits > pd.jmpOffset), "when not from F3 correction, cfiIndex should not be larger than jmpOffset")
  }


  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))


  val cfi_is_br = pd.brMask(io.cfiIndex.bits) && io.cfiIndex.valid
  val entry_has_jmp = pd.jmpInfo.valid
  val new_jmp_is_jal  = entry_has_jmp && !pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_jalr = entry_has_jmp &&  pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_call = entry_has_jmp &&  pd.jmpInfo.bits(1) && io.cfiIndex.valid
  val new_jmp_is_ret  = entry_has_jmp &&  pd.jmpInfo.bits(2) && io.cfiIndex.valid
  val last_jmp_rvi = entry_has_jmp && pd.jmpOffset === (predictWidth-1).U && !pd.rvcMask.last
  // val last_br_rvi = cfi_is_br && io.cfiIndex.bits === (PredictWidth-1).U && !pd.rvcMask.last

  val cfi_is_jal = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jal
  val cfi_is_jalr = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jalr

  // predictWidth = 4
  // instOffsetBits = 1
  def carryPos = log2Ceil(predictWidth)+instOffsetBits
  def getLower(pc: UInt) = pc(carryPos-1, instOffsetBits)
  // if not hit, establish a new entry
  init_entry.valid := cfi_is_br || entry_has_jmp
  // tag is left for ftb to assign

  // case br
  val init_br_slot = init_entry.getSlotForBr(0)
  when (cfi_is_br) {
    init_br_slot.valid := true.B
    init_br_slot.offset := io.cfiIndex.bits
    init_br_slot.setLowerStatByTarget(io.start_addr, io.target, numBr == 1)
    // When the difference between the start_addr and the target is too large, the assert will fail
    init_entry.always_taken(0) := true.B // set to always taken on init
    init_entry.needExtend := false.B
  }

  // case jmp
  when (entry_has_jmp) {
    init_entry.tailSlot.offset := pd.jmpOffset
    init_entry.tailSlot.valid := new_jmp_is_jal || new_jmp_is_jalr
    init_entry.tailSlot.setLowerStatByTarget(io.start_addr, Mux(cfi_is_jalr, 
      Mux(io.target === 0.U, nextFetch(io.start_addr), io.target),
      pd.jalTarget),
    isShare=false)
    val recordedTarget = init_entry.tailSlot.getTarget(io.start_addr)
    init_entry.needExtend := cfi_is_jalr && !new_jmp_is_ret && recordedTarget =/= io.target
    // assert(init_entry.tailSlot.getTarget(io.start_addr) === Mux(cfi_is_jalr, io.target, pd.jalTarget), "jmp target not match")
  }

  //TODO: how's the jmpPft calculated?
  val jmpPft = pd.jmpOffset +& Mux(pd.rvcMask(pd.jmpOffset), 1.U, 2.U)
  val defaultPft = Mux(isLastBankInBlock(io.start_addr), 4.U, 0.U) // when nBanks === 1 , isLastBankInBlock is always false
  val defaultCarry = Mux(isLastBankInBlock(io.start_addr), false.B, true.B)
  init_entry.pftAddr := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft, defaultPft)// FTB： align to boundary
  init_entry.carry   := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft(carryPos-instOffsetBits), defaultCarry)
  init_entry.isJalr := new_jmp_is_jalr
  init_entry.isCall := new_jmp_is_call
  init_entry.isRet  := new_jmp_is_ret
  // that means fall thru points to the middle of an inst
  // init_entry.br_mask := pd.brMask

  // if hit, check whether a new cfi(only br is possible) is detected
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
    // old_entry_modified.br_mask := pd.brMask
  }

  val old_entry_jmp_target_modified = WireInit(oe)
  val old_target = oe.tailSlot.getTarget(io.start_addr) // may be wrong because we store only 20 lowest bits
  val old_tail_is_jmp = !oe.tailSlot.sharing
  val jalr_target_modified = cfi_is_jalr && (old_target =/= io.target) && old_tail_is_jmp // TODO: pass full jalr target
  when (jalr_target_modified) {
    old_entry_jmp_target_modified.setByJmpTarget(io.start_addr, Mux(io.target === 0.U, nextFetch(io.start_addr), io.target))
    old_entry_jmp_target_modified.always_taken := 0.U.asTypeOf(Vec(numBr, Bool()))
    // old_entry_jmp_target_modified.br_mask := pd.brMask
    val recordedTarget = old_entry_jmp_target_modified.tailSlot.getTarget(io.start_addr)
    old_entry_jmp_target_modified.needExtend := recordedTarget =/= io.target && !new_jmp_is_ret
  }

  val old_entry_always_taken = WireInit(oe)
  val always_taken_modified_vec = Wire(Vec(numBr, Bool())) // whether modified or not
  for (i <- 0 until numBr) {
    when(io.isF3Correction){
      val correctBrTargetCond = io.cfiIndex.valid && oe.brValids(i) && io.cfiIndex.bits === oe.brOffset(i)
      when(correctBrTargetCond){
        old_entry_always_taken.allSlotsForBr(i).setLowerStatByTarget(io.start_addr, io.target, i == numBr-1)
      }    
    }.otherwise{
      old_entry_always_taken.always_taken(i) :=
        oe.always_taken(i) && io.cfiIndex.valid && oe.brValids(i) && io.cfiIndex.bits === oe.brOffset(i) && io.cfiTaken
    }
    always_taken_modified_vec(i) := oe.always_taken(i) && !old_entry_always_taken.always_taken(i)
  }
  val always_taken_modified = always_taken_modified_vec.reduce(_||_)



  val derived_from_old_entry =
    Mux(is_new_br, old_entry_modified,
      Mux(jalr_target_modified, old_entry_jmp_target_modified, old_entry_always_taken))
  
  if(enableFTBGenInternalPrint){
    val prefix = p"InsideFTBGen: "
    when(hit){
      XSDebug(is_new_br, prefix + p"new br inserted\n")
      XSDebug(jalr_target_modified, prefix + p"jalr target modified\n")
    }
  }


  io.new_entry := Mux(!hit, init_entry, derived_from_old_entry)

  io.new_br_insert_pos := new_br_insert_onehot
  io.taken_mask := VecInit((io.new_entry.brOffset zip io.new_entry.validSlots).map{
    case (off, v) => io.cfiIndex.bits === off && io.cfiIndex.valid && v && io.cfiTaken
  })
  io.jmp_taken := io.new_entry.jmpValid && io.new_entry.tailSlot.offset === io.cfiIndex.bits
  for (i <- 0 until numBr) {
    io.mispred_mask(i) := io.new_entry.brValids(i) && io.mispredict_vec(io.new_entry.brOffset(i))
  }
  io.mispred_mask.last := io.new_entry.jmpValid && io.mispredict_vec(pd.jmpOffset)

  // for perf counters
  io.is_init_entry := !hit
  io.is_old_entry := hit && !is_new_br && !jalr_target_modified && !always_taken_modified
  io.is_new_br := hit && is_new_br
  io.is_jalr_target_modified := hit && jalr_target_modified
  io.is_always_taken_modified := hit && always_taken_modified
  io.is_br_full := hit && is_new_br && may_have_to_replace
}
