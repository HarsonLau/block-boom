package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util._

import scala.math.min

case class BlockTageParams(
  // //                                           nSets, histLen, tagSz
  // tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((  128 * 8 / 2,       2,     7),
  //                                             (  128 * 8 / 2,       4,     7),
  //                                             (  256 * 8 / 2,       8,     8),
  //                                             (  256 * 8 / 2,      16,     8),
  //                                             (  128 * 8 / 2,      32,     9),
  //                                             (  128 * 8 / 2,      64,     9)),
  tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((1024,   8,   8),
                                              (1024,   13,  8),
                                              (1024,   32,  8),
                                              (1024,   64,  8),
                                              ),
  uBitPeriod: Int = 2048
)

class BlockTage(params: BlockTageParams = BlockTageParams())(implicit p: Parameters) extends BlockPredictorBank()(p)
{
  val nColumns = numBr
  val tageUBitPeriod = params.uBitPeriod
  val tageNTables    = params.tableInfo.size

  class TageMeta extends Bundle
  {
    val provider      = Vec(nColumns, Valid(UInt(log2Ceil(tageNTables).W)))
    val alt_differs   = Vec(nColumns, Output(Bool()))
    val provider_u    = Vec(nColumns, Output(UInt(2.W)))
    val provider_ctr  = Vec(nColumns, Output(UInt(3.W)))
    val allocate      = Vec(nColumns, Valid(UInt(log2Ceil(tageNTables).W)))
  }

  val f3_meta = Wire(new TageMeta)
  override val metaSz = f3_meta.asUInt.getWidth
  require(metaSz <= bpdMaxMetaLength)

  def inc_u(u: UInt, alt_differs: Bool, mispredict: Bool): UInt = {
    Mux(!alt_differs, u,
    Mux(mispredict, Mux(u === 0.U, 0.U, u - 1.U),
                    Mux(u === 3.U, 3.U, u + 1.U)))
  }

  val tt = params.tableInfo map {
    case (n, l, s) => {
      val t = Module(new TageTable(nColumns, n, s, l, params.uBitPeriod))
      t.io.f1_req_valid := RegNext(io.f0_valid)
      t.io.f1_req_pc    := RegNext(io.f0_pc)
      t.io.f1_req_ghist := io.f1_ghist
      (t, t.mems)
    }
  }
  val tables = tt.map(_._1)
  val mems = tt.map(_._2).flatten

  val f3_resps = VecInit(tables.map(_.io.f3_resp))

  // val s1_br_update_valids  = VecInit((0 until numBr).map(w => s1_update.bits.ftb_entry.valid &&
  //   s1_update.bits.ftb_entry.brValids(w) && s1_update.valid && !s1_update.bits.ftb_entry.always_taken(w) &&
  //   !(PriorityEncoder(s1_update.bits.br_taken_mask) < w.U)))

  val s1_update_meta = s1_update.bits.meta.asTypeOf(new TageMeta)

  val s1_update_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(nColumns, Bool()))))
  val s1_update_u_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(nColumns, UInt(1.W)))))

  val s1_update_taken   = Wire(Vec(tageNTables, Vec(nColumns, Bool())))
  val s1_update_old_ctr = Wire(Vec(tageNTables, Vec(nColumns, UInt(3.W))))
  val s1_update_alloc   = Wire(Vec(tageNTables, Vec(nColumns, Bool())))
  val s1_update_u       = Wire(Vec(tageNTables, Vec(nColumns, UInt(2.W))))

  s1_update_taken   := DontCare
  s1_update_old_ctr := DontCare
  s1_update_alloc   := DontCare
  s1_update_u       := DontCare

  for (w <- 0 until nColumns) {
    var altpred = io.resp_in(0).f3.br_taken_mask(w)
    val final_altpred = WireInit(io.resp_in(0).f3.br_taken_mask(w))
    var provided = false.B
    var provider = 0.U
    io.resp.f3.br_taken_mask(w) := io.resp_in(0).f3.br_taken_mask(w)
    io.resp.f3.perfs(w).tage_taken := io.resp.f3.br_taken_mask(w)

    for (i <- 0 until tageNTables) {
      val hit = f3_resps(i)(w).valid
      val ctr = f3_resps(i)(w).bits.ctr
      when (hit) {
        io.resp.f3.br_taken_mask(w) := Mux(ctr === 3.U || ctr === 4.U, altpred, ctr(2))
        final_altpred       := altpred
        io.resp.f3.perfs(w).tage_hit := true.B
      }

      provided = provided || hit
      provider = Mux(hit, i.U, provider)
      altpred  = Mux(hit, f3_resps(i)(w).bits.ctr(2), altpred)
    }
    f3_meta.provider(w).valid := provided
    f3_meta.provider(w).bits  := provider
    f3_meta.alt_differs(w)    := final_altpred =/= io.resp.f3.br_taken_mask(w)
    f3_meta.provider_u(w)     := f3_resps(provider)(w).bits.u
    f3_meta.provider_ctr(w)   := f3_resps(provider)(w).bits.ctr

    // Create a mask of tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    val allocatable_slots = (
      VecInit(f3_resps.map(r => !r(w).valid && r(w).bits.u === 0.U)).asUInt &
      ~(MaskLower(UIntToOH(provider)) & Fill(tageNTables, provided))
    )
    val alloc_lfsr = random.LFSR(tageNTables max 2)

    val first_entry = PriorityEncoder(allocatable_slots)
    val masked_entry = PriorityEncoder(allocatable_slots & alloc_lfsr)
    val alloc_entry = Mux(allocatable_slots(masked_entry),
      masked_entry,
      first_entry)

    f3_meta.allocate(w).valid := allocatable_slots =/= 0.U
    f3_meta.allocate(w).bits  := alloc_entry

    val s1_slot_offset = s1_update.bits.ftb_entry.getOffsetVec(w)
    val s1_slot_mispredicted = s1_update.bits.cfi_idx.valid && (s1_update.bits.cfi_idx.bits === s1_slot_offset) && s1_update.bits.cfi_mispredicted
    val update_was_taken = (s1_update.bits.cfi_idx.valid &&
                            (s1_update.bits.cfi_idx.bits === s1_slot_offset) &&
                            s1_update.bits.cfi_taken)
  
    when (s1_update.bits.br_mask(s1_slot_offset) && s1_update.valid && s1_update.bits.is_commit_update) {
      when (s1_update_meta.provider(w).valid) {
        val provider = s1_update_meta.provider(w).bits

        s1_update_mask(provider)(w) := true.B
        s1_update_u_mask(provider)(w) := true.B

        val new_u = inc_u(s1_update_meta.provider_u(w),
                          s1_update_meta.alt_differs(w),
                          s1_slot_mispredicted)
        s1_update_u      (provider)(w) := new_u
        s1_update_taken  (provider)(w) := update_was_taken
        s1_update_old_ctr(provider)(w) := s1_update_meta.provider_ctr(w)
        s1_update_alloc  (provider)(w) := false.B

      }
    }
  }

  when (s1_update.valid && s1_update.bits.is_commit_update && s1_update.bits.cfi_mispredicted && s1_update.bits.cfi_idx.valid && s1_update.bits.br_mask(s1_update.bits.cfi_idx.bits)) {
    // the Vec mispred_mask is of length numBr + 1
    // extract the first numBr elements 
    val idx = s1_update.bits.mispredSlotIdx

    val allocate = s1_update_meta.allocate(idx)
    when (allocate.valid) {
      s1_update_mask (allocate.bits)(idx) := true.B
      s1_update_taken(allocate.bits)(idx) := s1_update.bits.cfi_taken
      s1_update_alloc(allocate.bits)(idx) := true.B

      s1_update_u_mask(allocate.bits)(idx) := true.B
      s1_update_u     (allocate.bits)(idx) := 0.U

    } .otherwise {
      val provider = s1_update_meta.provider(idx)
      val decr_mask = Mux(provider.valid, ~MaskLower(UIntToOH(provider.bits)), 0.U)

      for (i <- 0 until tageNTables) {
        when (decr_mask(i)) {
          s1_update_u_mask(i)(idx) := true.B
          s1_update_u     (i)(idx) := 0.U
        }
      }
    }

  }
  

  for (i <- 0 until tageNTables) {
    for (w <- 0 until nColumns) {
      tables(i).io.update_mask(w)    := RegNext(s1_update_mask(i)(w))
      tables(i).io.update_taken(w)   := RegNext(s1_update_taken(i)(w))
      tables(i).io.update_alloc(w)   := RegNext(s1_update_alloc(i)(w))
      tables(i).io.update_old_ctr(w) := RegNext(s1_update_old_ctr(i)(w))

      tables(i).io.update_u_mask(w) := RegNext(s1_update_u_mask(i)(w))
      tables(i).io.update_u(w)      := RegNext(s1_update_u(i)(w))
    }
    tables(i).io.update_pc    := RegNext(s1_update.bits.pc)
    tables(i).io.update_hist  := RegNext(s1_update.bits.ghist)
  }


  //io.f3_meta := Cat(f3_meta.asUInt, micro.io.f3_meta(micro.metaSz-1,0), base.io.f3_meta(base.metaSz-1, 0))
  io.resp.f3_meta := f3_meta.asUInt
  if(enableTagePredictPrint) {
    val cond = true.B
    XSDebug(cond, p"-----------BlockTage Predict for PC 0x${Hexadecimal(s3_pc)}-----------\n")
    XSDebug(cond, p"provider: ${f3_meta.provider}\n")
    XSDebug(cond, p"provider_u: ${f3_meta.provider_u}\n")
    XSDebug(cond, p"provider_ctr: ${f3_meta.provider_ctr}\n")
    XSDebug(cond, p"allocate: ${f3_meta.allocate}\n")
    XSDebug(cond, p"alt_differs: ${f3_meta.alt_differs}\n")
    XSDebug(cond, p"------------------------------------------------------------\n")
  }

  if(enableTageJsonPredictPrint){
    /*
    {
      pc: 0xfda
      history: 8080809
      provider: -
      provider_ctr:
        allocate: 
        alt_differs
    }
    
    */
    // print the information above in json format
    val s3_ghist = RegNext(RegNext(io.f1_ghist))

    printf("{\"action\":\"predict\",\"pc\":\"0x%x\", \"history\":\"0x%x\", \"provider\":\"%d\", \"provider_ctr\":\"%d\",  \"alt_differs\":\"%d\"}\n",
      s3_pc, s3_ghist, f3_meta.provider.asUInt, f3_meta.provider_ctr.asUInt, f3_meta.alt_differs.asUInt)
  }

  if(enableTageUpdatePrint) {
    val cond = true.B
    XSDebug(cond, p"-----------BlockTage Update for PC 0x${Hexadecimal(s1_update.bits.pc)}-----------\n")
    // XSDebug(cond, p"mis-predict mask: 0b${Binary(s1_update_mispredict_mask.asUInt)}\n")
    XSDebug(cond, p"UpdateAllocate: ${s1_update_meta.allocate}\n")
    for (tt <- 0 until tageNTables) {
      XSDebug(cond, p"Table ${tt}:")
      XSDebug(cond, p"update_mask: ${s1_update_mask(tt)} ")
      XSDebug(cond, p"update_taken: ${s1_update_taken(tt)} ")
      XSDebug(cond, p"update_alloc: ${s1_update_alloc(tt)} ")
      XSDebug(cond, p"update_u_mask: ${s1_update_u_mask(tt)} ")
      XSDebug(cond, p"update_u: ${s1_update_u(tt)}\n")
      XSDebug(cond, p"update_old_ctr: ${s1_update_old_ctr(tt)}\n")
    }
    XSDebug(cond, p"------------------------------------------------------------\n")
  }
  if(enableTageJsonUpdatePrint){
    when(io.update.valid && io.update.bits.is_commit_update){
      // pc, history, cfi_mispredicted, cfi_idx, cfi_taken, br_mask, mispredSlotIdx
      printf("{\"action\":\"update\", \"pc\":\"0x%x\", \"history\":\"0x%x\", \"cfi_mispredicted\":\"%d\", \"cfi_idx\":\"%d\", \"cfi_taken\":\"%d\", \"br_mask\":\"0x%x\", \"mispredSlotIdx\":\"%d\"}\n",
        s1_update.bits.pc, s1_update.bits.ghist, s1_update.bits.cfi_mispredicted, s1_update.bits.cfi_idx.bits, s1_update.bits.cfi_taken, s1_update.bits.br_mask.asUInt, s1_update.bits.mispredSlotIdx)
    }
  }
}
