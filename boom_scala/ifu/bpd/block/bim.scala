package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util._
import scala.math.min



class BlockBIMMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFTBParameters
{
  val bims  = Vec(numBr, UInt(2.W))
}

case class BlockBIMParams(
  nSets: Int = 2048 * 4 / 2 // same slot count with original BIM
)

class BlockBIM(params: BlockBIMParams = BlockBIMParams())(implicit p: Parameters) extends BlockPredictorBank()(p)
{
  override val nSets = params.nSets

  require(isPow2(nSets))

  val nWrBypassEntries = 2

  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken  &&  taken, 3.U,
      Mux(old_bim_sat_ntaken && !taken, 0.U,
      Mux(taken, v + 1.U, v - 1.U)))
  }
  val s1_meta           = Wire(new BlockBIMMeta)
  val s2_meta           = Wire(new BlockBIMMeta)
  override val metaSz   = s2_meta.asUInt.getWidth

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }


  val data  = SyncReadMem(nSets, Vec(numBr, UInt(2.W)))

  val mems = Seq(("bim", nSets, numBr * 2))


  val s1_req_rdata    = data.read(s0_idx, s0_valid)
  val s2_req_rdata    = RegNext(s1_req_rdata)

  val s1_resp         = Wire(Vec(numBr, Bool()))
  val s2_resp         = Wire(Vec(numBr, Bool()))

  if(enableBIMPredictPrint){
    val cond = true.B
    XSDebug(cond, p"---------BIM: PC 0x${Hexadecimal(s2_pc)}}-----------\n")
    XSDebug(cond, p"BIM Read index: 0x${Hexadecimal(RegNext(RegNext(s0_idx)))}\n")
    for (w <- 0 until numBr) {
      XSDebug(cond, p"BIM: s2_req_rdata(${w.U}) = 0x${Hexadecimal(s2_req_rdata(w))}\n")
    }
    XSDebug(cond, p"-------------------------------\n")
  }

  for (w <- 0 until numBr) {
    s1_resp(w)        := s1_valid && s1_req_rdata(w)(1) && !doing_reset // TODO: FTB entry should be used
    s1_meta.bims(w)   := s1_req_rdata(w)


    s2_resp(w)        := RegNext(s1_resp(w))
    s2_meta.bims(w)   := RegNext(s1_meta.bims(w))
  }

  for (w <- 0 until numBr) {
    io.resp.f1.br_taken_mask(w) := s1_resp(w)
    io.resp.f1.perfs(w).bim_taken := s1_resp(w)
    io.resp.f2.br_taken_mask(w) := s2_resp(w)
    io.resp.f2.perfs(w).bim_taken := s2_resp(w)
    io.resp.f3.br_taken_mask(w) := RegNext(io.resp.f2.br_taken_mask(w))
    io.resp.f3.perfs(w).bim_taken := RegNext(io.resp.f2.perfs(w).bim_taken)
  }
  io.resp.f3_meta := RegNext(s2_meta.asUInt)

  /********************** update ***********************/

  val s1_update_wdata   = Wire(Vec(numBr, UInt(2.W)))
  val s1_update_wmask   = Wire(Vec(numBr, Bool()))
  val s1_update_meta    = s1_update.bits.meta.asTypeOf(new BlockBIMMeta)
  val s1_update_index   = s1_update_idx 
  val s1_br_update_valids  = VecInit((0 until numBr).map(w => 
    s1_update.bits.is_commit_update &&
    s1_update.bits.ftb_entry.valid &&
    s1_update.bits.ftb_entry.brValids(w) &&
    s1_update.valid &&
    // !s1_update.bits.ftb_entry.always_taken(w) &&
    !(PriorityEncoder(s1_update.bits.br_taken_mask) < w.U)))

  val wrbypass_idxs = Reg(Vec(nWrBypassEntries, UInt(log2Ceil(nSets).W)))
  val wrbypass      = Reg(Vec(nWrBypassEntries, Vec(numBr, UInt(2.W))))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(nWrBypassEntries).W))

  val wrbypass_hits = VecInit((0 until nWrBypassEntries) map { i =>
    !doing_reset &&
    wrbypass_idxs(i) === s1_update_index(log2Ceil(nSets)-1,0)
  })
  val wrbypass_hit = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  for (w <- 0 until numBr) {
    s1_update_wmask(w)         := false.B
    s1_update_wdata(w)         := DontCare

    when(s1_br_update_valids(w)) {
      val was_taken = s1_update.bits.br_taken_mask(w)
      
      val old_bim_value = Mux(wrbypass_hit, wrbypass(wrbypass_hit_idx)(w), s1_update_meta.bims(w))

      s1_update_wmask(w)     := true.B

      s1_update_wdata(w)     := bimWrite(old_bim_value, was_taken)
    }
  }

  when (doing_reset || (s1_update.valid && s1_update_wmask.reduce(_||_) && s1_update.bits.is_commit_update)) {
    val write_index = Mux(doing_reset, reset_idx, s1_update_index)
    val write_data = Mux(doing_reset, VecInit(Seq.fill(numBr) { 2.U(2.W) }), s1_update_wdata)
    val write_mask = Mux(doing_reset, (~(0.U(numBr.W))), s1_update_wmask.asUInt).asBools
    data.write(write_index, write_data, write_mask)
    if(enableBIMUpdatePrint){
      val cond = true.B 
      XSDebug(cond, p"----------BIM update----------\n")
      XSDebug(cond, p"BIM: write index 0x${Hexadecimal(write_index)} pc 0x${Hexadecimal(RegNext(io.update.bits.pc))}\n")
      for (w <- 0 until numBr) {
        XSDebug(cond, p"BIM: write data(${w.U}) = 0x${Hexadecimal(write_data(w))} mask(${w.U}) = ${write_mask(w)}\n")
      }
      XSDebug(cond, p"-------------------------------\n")
    }
  }

  when (s1_update_wmask.reduce(_||_) && s1_update.valid && s1_update.bits.is_commit_update) {
    when (wrbypass_hit) {
      wrbypass(wrbypass_hit_idx) := s1_update_wdata
    } .otherwise {
      wrbypass(wrbypass_enq_idx)      := s1_update_wdata
      wrbypass_idxs(wrbypass_enq_idx) := s1_update_index
      wrbypass_enq_idx := WrapInc(wrbypass_enq_idx, nWrBypassEntries)
    }
  }

}
