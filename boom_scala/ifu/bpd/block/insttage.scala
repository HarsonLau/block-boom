package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util._

import scala.math.min
import javax.swing.text.html.HTML.Tag
import javax.swing.InputMap

class InstTageResp extends Bundle {
  val ctr = UInt(3.W)
  val u   = UInt(2.W)
}

class InstTageTable(val nColumns: Int, val nRows: Int, val tagSz: Int, val histLength: Int, val uBitPeriod: Int)
  (implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFTBParameters
{
  require(histLength <= globalHistoryLength)

  val nWrBypassEntries = 2
  val io = IO( new Bundle {
    val f1_req_valid = Input(Bool())
    val f1_req_pc    = Input(Vec(nColumns, UInt(vaddrBitsExtended.W)))
    val f1_req_ghist = Input(UInt(globalHistoryLength.W))

    val f3_resp = Output(Vec(nColumns, Valid(new TageResp)))

    val update_mask    = Input(Vec(nColumns, Bool()))
    val update_taken   = Input(Vec(nColumns, Bool()))
    val update_alloc   = Input(Vec(nColumns, Bool()))
    val update_old_ctr = Input(Vec(nColumns, UInt(3.W)))

    val update_pc    = Input(Vec(nColumns, UInt(vaddrBitsExtended.W)))
    val update_hist  = Input(UInt())

    val update_u_mask = Input(Vec(nColumns, Bool()))
    val update_u = Input(Vec(nColumns, UInt(2.W)))
  })
  io.f3_resp := DontCare

  def compute_folded_hist(hist: UInt, l: Int) = {
    val nChunks = (histLength + l - 1) / l
    val hist_chunks = (0 until nChunks) map {i =>
      hist(min((i+1)*l, histLength)-1, i*l)
    }
    hist_chunks.reduce(_^_)
  }

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt) = {
    val idx_history = compute_folded_hist(hist, log2Ceil(nRows))
    val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows)-1,0)
    val tag_history = compute_folded_hist(hist, tagSz)
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history)(tagSz-1,0)
    (idx, tag)
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = {
    Mux(!taken, Mux(ctr === 0.U, 0.U, ctr - 1.U),
                Mux(ctr === 7.U, 7.U, ctr + 1.U))
  }


  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }


  class TageEntry extends Bundle {
    val valid = Bool() // TODO: Remove this valid bit
    val tag = UInt(tagSz.W)
    val ctr = UInt(3.W)
  }


  val tageEntrySz = 1 + tagSz + 3

  val hi_us  = SyncReadMem(nRows,  Bool())
  val lo_us  = SyncReadMem(nRows,  Bool())
  val table  = SyncReadMem(nRows,  UInt(tageEntrySz.W))
  val mems = Seq((f"tage_l$histLength", nRows, tageEntrySz))
  for (w <- 0 until nColumns) {
    val (s1_hashed_idx, s1_tag) = compute_tag_and_hash(blockFetchIdx(io.f1_req_pc(w)), io.f1_req_ghist)
    val s2_tag = RegNext(s1_tag)
    val s2_req_rtage = table.read(s1_hashed_idx, io.f1_req_valid).asTypeOf(new TageEntry)
    val s2_req_rhius = hi_us.read(s1_hashed_idx, io.f1_req_valid)
    val s2_req_rlous = lo_us.read(s1_hashed_idx, io.f1_req_valid)
    val s2_req_rhits = s2_req_rtage.valid && s2_req_rtage.tag === s2_tag && !doing_reset

    io.f3_resp(w).valid    := RegNext(s2_req_rhits)
    io.f3_resp(w).bits.u   := RegNext(Cat(s2_req_rhius, s2_req_rlous))
    io.f3_resp(w).bits.ctr := RegNext(s2_req_rtage.ctr)

  }

  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  for (w <- 0 until nColumns) {
    val (update_idx, update_tag) = compute_tag_and_hash(blockFetchIdx(io.update_pc(w)), io.update_hist)

    val update_wdata = Wire(new TageEntry)

    when(doing_reset || io.update_mask(w)){
      table.write(
        Mux(doing_reset, reset_idx, update_idx),
        Mux(doing_reset, 0.U(tageEntrySz.W), update_wdata.asUInt)
      )
    }

    val update_hi_wdata = Wire(Bool())
    when(doing_reset || doing_clear_u_hi || io.update_u_mask(w)){
      hi_us.write(
        Mux(doing_reset, reset_idx, Mux(doing_clear_u_hi, clear_u_idx, update_idx)),
        Mux(doing_reset || doing_clear_u_hi, 0.B, update_hi_wdata)
      )
    }

    val update_lo_wdata = Wire(Bool())
    when(doing_reset || doing_clear_u_lo || io.update_u_mask(w)){
      lo_us.write(
        Mux(doing_reset, reset_idx, Mux(doing_clear_u_lo, clear_u_idx, update_idx)),
        Mux(doing_reset || doing_clear_u_lo, 0.B, update_lo_wdata)
      )
    }

    val wrbypass_tags    = Reg(Vec(nWrBypassEntries, UInt(tagSz.W)))
    val wrbypass_idxs    = Reg(Vec(nWrBypassEntries, UInt(log2Ceil(nRows).W)))
    val wrbypass         = Reg(Vec(nWrBypassEntries, UInt(3.W)))
    val wrbypass_enq_idx = RegInit(0.U(log2Ceil(nWrBypassEntries).W))

    val wrbypass_hits    = VecInit((0 until nWrBypassEntries) map { i =>
      !doing_reset &&
      wrbypass_tags(i) === update_tag &&
      wrbypass_idxs(i) === update_idx
    })
    val wrbypass_hit     = wrbypass_hits.reduce(_||_)
    val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)


    update_wdata.ctr   := Mux(io.update_alloc(w),
      Mux(io.update_taken(w), 4.U, 3.U),
      Mux(wrbypass_hit, inc_ctr(wrbypass(wrbypass_hit_idx), io.update_taken(w)),
                        inc_ctr(io.update_old_ctr(w), io.update_taken(w))
      )
    )
    update_wdata.valid := true.B
    update_wdata.tag   := update_tag
    update_hi_wdata    := io.update_u(w)(1)
    update_lo_wdata    := io.update_u(w)(0)

    when (io.update_mask(w)) {
      when(wrbypass_hits.reduce(_||_)) {
        wrbypass(wrbypass_hit_idx) := update_wdata.ctr
      } .otherwise {
        wrbypass     (wrbypass_enq_idx) := update_wdata.ctr
        wrbypass_tags(wrbypass_enq_idx) := update_tag
        wrbypass_idxs(wrbypass_enq_idx) := update_idx
        wrbypass_enq_idx := WrapInc(wrbypass_enq_idx, nWrBypassEntries)
      }
    }
  }
}

case class InstTageParams(
  //                                           nSets, histLen, tagSz
  tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((  128 * 8,       2,     7),
                                              (  128 * 8,       4,     7),
                                              (  256 * 8,       8,     8),
                                              (  256 * 8,      16,     8),
                                              (  128 * 8,      32,     9),
                                              (  128 * 8,      64,     9)),
  uBitPeriod: Int = 2048
)

class InstTage(params: InstTageParams = InstTageParams())(implicit p: Parameters) extends BlockPredictorBank()(p)
{
  val nColumns = 1
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
  f3_meta:=DontCare
  override val metaSz = f3_meta.asUInt.getWidth
  require(metaSz <= bpdMaxMetaLength)

  val cfi_offsets = io.resp_in(0).f1.offsets
  val s1_bankAlignPC = bankAlign(s1_pc)
  val s1_cfi_pcs     = VecInit((0 until nColumns).map(i => s1_bankAlignPC + cfi_offsets(i)<<instOffsetBits))
  val s1_u_bankAlignPC = bankAlign(s1_update.bits.pc)
  val s1_u_offsets   = s1_update.bits.ftb_entry.getOffsetVec
  val s1_u_cfi_pcs   = VecInit((0 until nColumns).map(i => s1_u_bankAlignPC + s1_u_offsets(i)<<instOffsetBits))

  def inc_u(u: UInt, alt_differs: Bool, mispredict: Bool): UInt = {
    Mux(!alt_differs, u,
    Mux(mispredict, Mux(u === 0.U, 0.U, u - 1.U),
                    Mux(u === 3.U, 3.U, u + 1.U)))
  }

  val tt = params.tableInfo map {
    case (n, l, s) => {
      val t = Module(new InstTageTable(nColumns, n, s, l, params.uBitPeriod))
      t.io.f1_req_valid := RegNext(io.f0_valid)
      t.io.f1_req_pc    := s1_cfi_pcs
      t.io.f1_req_ghist := io.f1_ghist
      (t, t.mems)
    }
  }
  val tables = tt.map(_._1)
  val mems = tt.map(_._2).flatten

  val f3_resps = VecInit(tables.map(_.io.f3_resp))


  val s1_update_meta = s1_update.bits.meta.asTypeOf(new TageMeta)

  val s1_update_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(nColumns, Bool()))))
  val s1_update_u_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(nColumns, UInt(1.W)))))

  val s1_update_taken   = Wire(Vec(tageNTables, Vec(nColumns, Bool())))
  val s1_update_old_ctr = Wire(Vec(tageNTables, Vec(nColumns, UInt(3.W))))
  val s1_update_alloc   = Wire(Vec(tageNTables, Vec(nColumns, Bool())))
  val s1_update_u       = Wire(Vec(tageNTables, Vec(nColumns, UInt(2.W))))

  val s1_br_update_valids  = VecInit((0 until numBr).map(w => 
    s1_update.bits.is_commit_update &&
    s1_update.bits.ftb_entry.valid &&
    s1_update.bits.ftb_entry.brValids(w) &&
    s1_update.valid &&
    // !s1_update.bits.ftb_entry.always_taken(w) &&
    !(PriorityEncoder(s1_update.bits.br_taken_mask) < w.U)))
  
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
    val update_was_taken = s1_update.bits.br_taken_mask(w)
  
    when (s1_update_meta.provider(w).valid && s1_br_update_valids(w)) {
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

  when (s1_update.bits.mispredSlotIdx < nColumns.U && s1_br_update_valids(s1_update.bits.mispredSlotIdx) && s1_update.bits.cfi_mispredicted) {
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
      tables(i).io.update_pc(w)    := RegNext(s1_u_cfi_pcs(w))
    }
    tables(i).io.update_hist  := RegNext(s1_update.bits.ghist)
  }


  //io.f3_meta := Cat(f3_meta.asUInt, micro.io.f3_meta(micro.metaSz-1,0), base.io.f3_meta(base.metaSz-1, 0))
  io.resp.f3_meta := f3_meta.asUInt

}
