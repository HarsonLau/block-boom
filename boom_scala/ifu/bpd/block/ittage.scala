package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix, MaskLower, WrapInc}

import scala.math.min



class IttageResp (implicit p: Parameters) extends BoomBundle()(p) 
{
  val ctr = UInt(2.W)
  val u   = UInt(2.W)
  val target  = UInt(vaddrBitsExtended.W)
}

class IttageTable(val nRows: Int, val tagSz: Int, val histLength: Int, val uBitPeriod: Int)
  (implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  require(histLength <= globalHistoryLength)

  val nWrBypassEntries = 2
  val io = IO( new Bundle {
    val f1_req_valid = Input(Bool())
    val f1_req_pc    = Input(UInt(vaddrBitsExtended.W))
    val f1_req_ghist = Input(UInt(globalHistoryLength.W))

    val f3_resp = Output(Valid(new IttageResp))

    val update_mask         = Input(Bool())
    // val update_taken     = Input(Bool())
    val update_mispredict   = Input(Bool())
    val update_alloc        = Input(Bool())
    val update_old_ctr      = Input(UInt(2.W))

    val update_pc           = Input(UInt())
    val update_hist         = Input(UInt())
    val update_target       = Input(UInt(vaddrBitsExtended.W))
    val update_old_target   = Input(UInt(vaddrBitsExtended.W))

    val update_u_mask       = Input(Bool())
    val update_u            = Input(UInt(2.W))
  })

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

  def inc_ctr(ctr: UInt, mispredict: Bool): UInt = {
    Mux(mispredict, Mux(ctr === 0.U, 0.U, ctr - 1.U),
                Mux(ctr === 3.U, 3.U, ctr + 1.U))
  }


  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }


  class IttageEntry extends Bundle {
    val valid = Bool() // TODO: Remove this valid bit
    val tag = UInt(tagSz.W)
    val ctr = UInt(2.W)
    val target= UInt(vaddrBitsExtended.W)
  }


  val ittageEntrySz = 1 + tagSz + 2 + vaddrBitsExtended

  val (s1_hashed_idx, s1_tag) = compute_tag_and_hash((io.f1_req_pc >> 1.U), io.f1_req_ghist)

  // printf("f1_req_pc: 0x%x\n", io.f1_req_pc)

  val hi_us  = SyncReadMem(nRows, Bool())
  val lo_us  = SyncReadMem(nRows, Bool())
  val table  = SyncReadMem(nRows, UInt(ittageEntrySz.W))

  val mems = Seq((f"ittage_l$histLength", nRows, 1 * ittageEntrySz))

  val s2_tag       = RegNext(s1_tag)

  val s2_req_rittage = table.read(s1_hashed_idx, io.f1_req_valid).asTypeOf(new IttageEntry)
  val s2_req_rhius = hi_us.read(s1_hashed_idx, io.f1_req_valid)
  val s2_req_rlous = lo_us.read(s1_hashed_idx, io.f1_req_valid)
  val s2_req_rhits = s2_req_rittage.valid && s2_req_rittage.tag === s2_tag && !doing_reset

  // when(s2_req_rhits === true.B){
  //   printf("pc: 0x%x, fetchIdx: 0x%x, ghist: 0x%x, hashed_idx: 0x%x, s1_tag: 0x%x\n", 
  //     RegNext(io.f1_req_pc), RegNext(fetchIdx(io.f1_req_pc)), RegNext(io.f1_req_ghist), RegNext(s1_hashed_idx), RegNext(s1_tag))
  //   printf("s2_req_rittage.tag: 0x%x, s2_tag: 0x%x\n", s2_req_rittage.tag, s2_tag)
  // }

  // This bit indicates the ITTAGE table matched here
  io.f3_resp.valid    := RegNext(s2_req_rhits)
  io.f3_resp.bits.u   := RegNext(Cat(s2_req_rhius, s2_req_rlous))
  io.f3_resp.bits.ctr := RegNext(s2_req_rittage.ctr)
  io.f3_resp.bits.target:= RegNext(s2_req_rittage.target)

  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  val (update_idx, update_tag) = compute_tag_and_hash((io.update_pc >> 1.U), io.update_hist)


  val update_wdata = Wire(new IttageEntry)

  when(io.update_mask){
    table.write(
      Mux(doing_reset, reset_idx,             update_idx),
      Mux(doing_reset, 0.U(ittageEntrySz.W),  update_wdata.asUInt)
    )
  }

  // when(io.update_mask(0) === true.B){
  //   printf("update data: misp: %b, alloc: %b, old_ctr: %d, pc: 0x%x, hist: 0x%x, fetchIdx: 0x%x, update idx: 0x%x, update tag: 0x%x, update u_mask: 0x%x, update_u: 0x%x\n", io.update_mispredict(0), io.update_alloc(0), io.update_old_ctr(0), io.update_pc, io.update_hist, fetchIdx(io.update_pc), update_idx, update_tag, io.update_u_mask(0), io.update_u(0))
  // }
  val update_hi_wdata = io.update_u(1)
  val update_lo_wdata = io.update_u(0)
  when(io.update_u_mask){
    hi_us.write(
      Mux(doing_reset, reset_idx, Mux(doing_clear_u_hi, clear_u_idx, update_idx)),
      Mux(doing_reset || doing_clear_u_hi, false.B, update_hi_wdata)
    )
    lo_us.write(
      Mux(doing_reset, reset_idx, Mux(doing_clear_u_lo, clear_u_idx, update_idx)),
      Mux(doing_reset || doing_clear_u_lo, false.B, update_lo_wdata)
    )
  }

  val wrbypass_tags    = Reg(Vec(nWrBypassEntries, UInt(tagSz.W)))
  val wrbypass_idxs    = Reg(Vec(nWrBypassEntries, UInt(log2Ceil(nRows).W)))
  val wrbypass         = Reg(Vec(nWrBypassEntries, UInt(2.W)))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(nWrBypassEntries).W))

  val wrbypass_hits    = VecInit((0 until nWrBypassEntries) map { i =>
    !doing_reset &&
    wrbypass_tags(i) === update_tag &&
    wrbypass_idxs(i) === update_idx
  })
  val wrbypass_hit     = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  update_wdata.ctr   := Mux(io.update_alloc, 0.U,
    Mux(wrbypass_hit,       inc_ctr(wrbypass(wrbypass_hit_idx), io.update_mispredict),
                            inc_ctr(io.update_old_ctr, io.update_mispredict)
    )
  )

  update_wdata.valid := true.B
  update_wdata.tag   := update_tag
  // update_wdata.target:= io.update_target
  update_wdata.target:= Mux(io.update_old_ctr === 0.U && io.update_mispredict, io.update_target, io.update_old_target)

  // when(io.update_old_ctr =/= 0.U){
  //   printf("test3: update_old_ctr: %d, update_target: 0x%x, update_old_target: 0x%x\n", io.update_old_ctr, io.update_target, io.update_old_target)
  // }

  when (io.update_mask) {
    when (wrbypass_hits.reduce(_||_)) {
      wrbypass(wrbypass_hit_idx) := update_wdata.ctr
    } .otherwise {
      wrbypass     (wrbypass_enq_idx) := update_wdata.ctr
      wrbypass_tags(wrbypass_enq_idx) := update_tag
      wrbypass_idxs(wrbypass_enq_idx) := update_idx
      wrbypass_enq_idx := WrapInc(wrbypass_enq_idx, nWrBypassEntries)
    }
  }



}


case class BoomIttageParams(
  //                                           nSets, histLen, tagSz
  tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((  64,     2,   9),
                                              (  64,     4,   9),
                                              (  128,    8,   11),
                                              (  128,    16,  11),
                                              (  64,     32,  13),
                                              (  64,     64,  13)),
  uBitPeriod: Int = 2048
)


class IttageBranchPredictorBank(params: BoomIttageParams = BoomIttageParams())(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  val ittageUBitPeriod = params.uBitPeriod
  val ittageNTables    = params.tableInfo.size

  class IttageMeta extends Bundle
  {
    val provider            = Valid(UInt(log2Ceil(ittageNTables).W))
    val altProvider         = Valid(UInt(log2Ceil(ittageNTables).W))
    val alt_differs         = Output(Bool())
    val provider_u          = Output(UInt(2.W))
    val provider_ctr        = Output(UInt(2.W))
    val altProvider_ctr     = Output(UInt(2.W))
    val allocate            = Valid(UInt(log2Ceil(ittageNTables).W))
    val provider_target     = Output(UInt(vaddrBitsExtended.W))
    val altProvider_target  = Output(UInt(vaddrBitsExtended.W))
  }

  val f3_meta = Wire(new IttageMeta)
  override val metaSz = f3_meta.asUInt.getWidth
  require(metaSz <= bpdMaxMetaLength)

  def inc_u(u: UInt, alt_differs: Bool, mispredict: Bool): UInt = {
    Mux(!alt_differs, u,
    Mux(mispredict, Mux(u === 0.U, 0.U, u - 1.U),
                    Mux(u === 3.U, 3.U, u + 1.U)))
  }

  val tt = params.tableInfo map {
    case (n, l, s) => {
      val t = Module(new IttageTable(n, s, l, params.uBitPeriod))
      t.io.f1_req_valid := RegNext(io.f0_valid)
      t.io.f1_req_pc    := RegNext(io.f0_pc + io.f0_pc_bankOffset)
      t.io.f1_req_ghist := io.f1_ghist
      (t, t.mems)
    }
  }
  val tables = tt.map(_._1)
  val mems = tt.map(_._2).flatten

  val f3_resps = VecInit(tables.map(_.io.f3_resp))

  val s1_update_meta = s1_update.bits.meta.asTypeOf(new IttageMeta)
  val s1_update_mispredict_mask = s1_update.bits.cfi_mispredicted

  val s1_update_mask  = WireInit((0.U).asTypeOf(Vec(ittageNTables, Bool())))
  val s1_update_u_mask  = WireInit((0.U).asTypeOf(Vec(ittageNTables, Bool())))

  val s1_update_old_ctr = Wire(Vec(ittageNTables, UInt(2.W)))
  val s1_update_alloc   = Wire(Vec(ittageNTables, Bool()))
  val s1_update_u       = Wire(Vec(ittageNTables, UInt(2.W)))
  val s1_update_old_target  = Wire(Vec(ittageNTables, UInt(vaddrBitsExtended.W)))

  s1_update_old_ctr := DontCare
  s1_update_alloc   := DontCare
  s1_update_u       := DontCare
  s1_update_old_target := DontCare

  for (w <- 0 until bankWidth) {
    io.resp.f3(w).taken := io.resp_in(0).f3(w).taken
  }

  // mgy for debug
  val cycle_cnt = RegInit(0.U(64.W))
  cycle_cnt := cycle_cnt + 1.U

  val jalr_mask = s1_update.bits.cfi_is_jalr && (!s1_update.bits.cfi_is_ret)

  var provided            = false.B
  var altProvided         = false.B
  var provider            = 0.U
  var altProvider         = 0.U
  var provider_ctr        = 0.U(2.W)
  var altProvider_ctr     = 0.U(2.W)
  var provider_target     = 0.U(vaddrBitsExtended.W)
  var altProvider_target  = 0.U(vaddrBitsExtended.W)

  // printf("ITTAGE cycle: 0x%x, F0_PC: 0x%x\n", cycle_cnt, io.f0_pc + io.f0_pc_bankOffset)

  for (i <- 0 until ittageNTables) {
    val hit = f3_resps(i).valid
    val ctr = f3_resps(i).bits.ctr
    val target = f3_resps(i).bits.target
    altProvided         = Mux(hit && provided, true.B, altProvided)
    altProvider         = Mux(hit && provided, provider, altProvider)
    altProvider_ctr     = Mux(hit && provided, provider_ctr, altProvider_ctr)
    altProvider_target  = Mux(hit && provided, provider_target, altProvider_target)
    provided            = Mux(hit, true.B, provided)
    provider            = Mux(hit, i.U, provider)
    provider_ctr        = Mux(hit, ctr, provider_ctr)
    provider_target     = Mux(hit, target, provider_target)
  }

  io.predicted_pc_ittage.valid  := provided
  io.predicted_pc_ittage.bits   := Mux(provider_ctr === 0.U && altProvided, altProvider_target, provider_target)
  // printf("cycle: 0x%x, provider_ctr: 0x%x, provided: 0x%x, altProvider_target: 0x%x, provider_target: 0x%x, io.predicted_pc_ittage.valid: 0b%b, io.predicted_pc_ittage.bits: 0x%x\n",
  //   cycle_cnt, provider_ctr, provided, altProvider_target, provider_target, io.predicted_pc_ittage.valid, io.predicted_pc_ittage.bits)

  f3_meta.provider.valid     := provided
  f3_meta.provider.bits      := provider
  f3_meta.altProvider.valid  := altProvided
  f3_meta.altProvider.bits   := altProvider
  f3_meta.alt_differs        := altProvider_target =/= provider_target
  f3_meta.provider_u         := f3_resps(provider).bits.u
  f3_meta.provider_ctr       := provider_ctr
  f3_meta.altProvider_ctr    := altProvider_ctr
  f3_meta.provider_target    := provider_target
  f3_meta.altProvider_target := altProvider_target

  // Create a mask of tables which did not hit our query, and also contain useless entries
  // and also uses a longer history than the provider
  val allocatable_slots = (
    VecInit(f3_resps.map(r => !r.valid && r.bits.u === 0.U)).asUInt &
    ~(MaskLower(UIntToOH(provider)) & Fill(ittageNTables, provided))
  )
  val alloc_lfsr = random.LFSR(ittageNTables max 2)

  val first_entry = PriorityEncoder(allocatable_slots)
  val masked_entry = PriorityEncoder(allocatable_slots & alloc_lfsr)
  val alloc_entry = Mux(allocatable_slots(masked_entry),
    masked_entry,
    first_entry)

  f3_meta.allocate.valid := allocatable_slots =/= 0.U
  f3_meta.allocate.bits  := alloc_entry

  when (jalr_mask && s1_update.valid && s1_update.bits.is_commit_update) {
    when (s1_update_meta.provider.valid && !(s1_update_meta.provider_ctr === 0.U && s1_update_meta.altProvider.valid)) {
      val provider = s1_update_meta.provider.bits

      s1_update_mask(provider)    := true.B
      s1_update_u_mask(provider)  := true.B

      val new_u = inc_u(s1_update_meta.provider_u,
                        s1_update_meta.alt_differs,
                        s1_update_mispredict_mask)
      s1_update_u      (provider)      := new_u
      s1_update_old_ctr(provider)      := s1_update_meta.provider_ctr
      s1_update_alloc  (provider)      := false.B
      s1_update_old_target(provider)   := s1_update_meta.provider_target
      // printf("update provider -> cycle: 0x%x, pc: 0x%x, provider: %d, new_u: %d, s1_update_old_ctr: %d, s1_update_old_target: 0x%x\n",
      //   cycle_cnt, s1_update.bits.pc, provider, new_u, s1_update_old_ctr(provider), s1_update_old_target(provider))
    }
    when(s1_update_meta.provider_ctr === 0.U && s1_update_meta.altProvider.valid){
      val altProvider = s1_update_meta.altProvider.bits
      s1_update_mask(altProvider)         := true.B
      s1_update_u_mask(altProvider)       := false.B
      s1_update_old_ctr(altProvider)      := s1_update_meta.altProvider_ctr
      s1_update_alloc  (altProvider)      := false.B
      s1_update_old_target  (altProvider) := s1_update_meta.altProvider_target
      // printf("update altProvider -> cycle: 0x%x, pc: 0x%x, provider: %d, s1_update_old_ctr: %d, s1_update_old_target: 0x%x\n",
      //   cycle_cnt, s1_update.bits.pc, altProvider, s1_update_old_ctr(altProvider), s1_update_old_target(altProvider))
    }
  }
  when (jalr_mask && s1_update.valid && s1_update.bits.is_commit_update && s1_update.bits.cfi_mispredicted && s1_update.bits.cfi_idx.valid) {
    val allocate = s1_update_meta.allocate
    when (allocate.valid) {
      s1_update_mask (allocate.bits) := true.B
      s1_update_alloc(allocate.bits) := true.B
      s1_update_u_mask(allocate.bits) := true.B
      s1_update_u     (allocate.bits) := 0.U
      printf("allocate TableN: %x\n", allocate.bits)
    } .otherwise {
      val provider = s1_update_meta.provider
      val decr_mask = Mux(provider.valid, ~MaskLower(UIntToOH(provider.bits)), 0.U)
      printf("Clear table mask: 0b%b\n", decr_mask)
      for (i <- 0 until ittageNTables) {
        when (decr_mask(i)) {
          s1_update_u_mask(i) := true.B
          s1_update_u     (i) := 0.U
        }
      }
    }
  }

  // printf("s1_update.bits.pc_bankOffset: 0x%x\n", s1_update.bits.pc_bankOffset)

  for (i <- 0 until ittageNTables) {
    tables(i).io.update_mask       := RegNext(s1_update_mask(i))
    tables(i).io.update_mispredict := RegNext(s1_update_mispredict_mask)
    tables(i).io.update_alloc      := RegNext(s1_update_alloc(i))
    tables(i).io.update_old_ctr    := RegNext(s1_update_old_ctr(i))
    tables(i).io.update_u_mask     := RegNext(s1_update_u_mask(i))
    tables(i).io.update_u          := RegNext(s1_update_u(i))
    tables(i).io.update_old_target := RegNext(s1_update_old_target(i))
    tables(i).io.update_pc    := RegNext(s1_update.bits.pc + s1_update.bits.pc_bankOffset)
    tables(i).io.update_hist  := RegNext(s1_update.bits.ghist)
    tables(i).io.update_target:= RegNext(s1_update.bits.target)
    when(tables(i).io.update_mask === true.B){
      printf("cycle: 0x%x, table %d -> update_m: %d, misp_m: %d, update_alloc: %d, update_old_ctr: %d, update_u_m: %d, update_u: %d, update_pc: 0x%x, update_hist: 0x%x, update_target: 0x%x, update_old_target: 0x%x, bankOffset: 0x%x\n",
       cycle_cnt, i.U, tables(i).io.update_mask, tables(i).io.update_mispredict, tables(i).io.update_alloc, tables(i).io.update_old_ctr, tables(i).io.update_u_mask, tables(i).io.update_u, 
       tables(i).io.update_pc, tables(i).io.update_hist, tables(i).io.update_target, tables(i).io.update_old_target, RegNext(s1_update.bits.pc_bankOffset))        
    }
  }

  //io.f3_meta := Cat(f3_meta.asUInt, micro.io.f3_meta(micro.metaSz-1,0), base.io.f3_meta(base.metaSz-1, 0))
  io.f3_meta := f3_meta.asUInt
}
