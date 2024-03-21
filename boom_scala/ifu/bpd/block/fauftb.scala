package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import boom.common._
import boom.util._
import scala.{Tuple2 => &}

import scala.math.min

trait FauFTBParams extends HasBoomFTBParameters {
  val numWays = 16 // note: the uBTB in BOOM has 16 ways
  val tagSize = 16

  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20

  def getTag(pc: UInt) = pc(tagSize+instOffsetBits-1, instOffsetBits)
}

class FauFTBEntry(implicit p: Parameters) extends FTBEntry()(p) {}

class FauFTBWay(implicit p: Parameters) extends BoomModule()(p) with FauFTBParams {
  val io = IO(new Bundle{
    val req_tag = Input(UInt(tagSize.W))
    val resp = Output(new FauFTBEntry)
    val resp_hit = Output(Bool())
    val update_req_tag = Input(UInt(tagSize.W))
    val update_hit = Output(Bool())
    val write_valid = Input(Bool())
    val write_entry = Input(new FauFTBEntry)
    val write_tag = Input(UInt(tagSize.W))
    val tag_read = Output(UInt(tagSize.W))
  })

  val data = Reg(new FauFTBEntry)
  val tag = Reg(UInt(tagSize.W))
  val valid = RegInit(false.B)

  io.resp := data
  io.resp_hit := tag === io.req_tag && valid
  // write bypass to avoid multiple hit
  io.update_hit := ((tag === io.update_req_tag) && valid) ||
                   ((io.write_tag === io.update_req_tag) && io.write_valid)
  io.tag_read := tag

  when (io.write_valid) {
    when (!valid) {
      valid := true.B
    }
    tag   := io.write_tag
    data  := io.write_entry
  }
}

class FauFTB(implicit p: Parameters) extends BlockPredictorBank with FauFTBParams {
  class FauFTBMeta(implicit p: Parameters) extends BoomBundle with FauFTBParams {
    // val pred_way = UInt(log2Ceil(numWays).W)
    val hit = Bool()
  }
  val resp_meta = Wire(new FauFTBMeta)
  resp_meta := DontCare
  val meta_size = resp_meta.getWidth

  val ways = Seq.tabulate(numWays)(w => Module(new FauFTBWay))
  val ctrs = Seq.tabulate(numWays)(w => Seq.tabulate(numBr)(b => RegInit(2.U(2.W))))
  val replacer = ReplacementPolicy.fromString("plru", numWays)
  val replacer_touch_ways = Wire(Vec(2, Valid(UInt(log2Ceil(numWays).W))))

  val mems = Nil

  // --------------------------------------------------------
  // **** Prediction Logic ****
  // --------------------------------------------------------

  // pred req
  ways.foreach(_.io.req_tag := getTag(s1_pc)) // use the F1 pc as the BOOM.ubtb and XiangShan.FauFTB do
  
  // pred resp
  val s1_hit_oh = VecInit(ways.map(_.io.resp_hit)).asUInt
  val s1_hit = s1_hit_oh.orR
  val s1_hit_way = OHToUInt(s1_hit_oh)
  val s1_possible_full_preds = Wire(Vec(numWays, new BlockPrediction))

  val s1_all_entries = VecInit(ways.map(_.io.resp))
  for (c & fp & e <- ctrs zip s1_possible_full_preds zip s1_all_entries) {
    fp.hit := DontCare // Note: the hit bit will be assigned later
    fp.perfs := DontCare
    fp.fromFtbEntry(e, s1_pc)
    for (i <- 0 until numBr) {
      fp.br_taken_mask(i) := e.validSlots(i) && (c(i)(1) || e.always_taken(i))
    }
  }

  val empty_pred = Wire(new BlockPrediction)
  empty_pred.makeDefault(s1_pc)
  empty_pred.perfs := DontCare
  val s1_hit_full_pred =Mux(PopCount(s1_hit_oh)=/=0.U, Mux1H(s1_hit_oh, s1_possible_full_preds), empty_pred)
  val s1_hit_ftb_entry = Mux(s1_hit_oh =/= 0.U, Mux1H(s1_hit_oh, s1_all_entries), 0.U.asTypeOf(new FauFTBEntry))
  XSError(PopCount(s1_hit_oh) > 1.U, "fauftb has multiple hits!\n")

  io.resp.f1 := s1_hit_full_pred
  io.resp.f1.hit := s1_hit // assign hit bit
  for ( i <- 0 until numBr){
    io.resp.f1.perfs(i).fauftb_hit := s1_hit
    io.resp.f1.perfs(i).fauftb_taken := s1_hit_full_pred.br_taken_mask(i)

    io.resp.f1.perfs(i).ftb_entry_hit := false.B
    io.resp.f1.perfs(i).ftb_slot_hit  := false.B
    io.resp.f1.perfs(i).bim_taken := false.B

    io.resp.f1.perfs(i).tage_hit := false.B
    io.resp.f1.perfs(i).tage_taken := false.B
  }

  io.resp.f2 := RegNext(io.resp.f1)
  io.resp.f3 := RegNext(RegNext(io.resp.f1))

  io.resp.last_stage_entry := RegNext(RegNext(s1_hit_ftb_entry))
  io.resp.f3_meta := DontCare

  // pred update replacer state
  replacer_touch_ways(0).valid := RegNext(s1_valid && s1_hit)
  replacer_touch_ways(0).bits := RegEnable(s1_hit_way, s1_valid && s1_hit)

  // --------------------------------------------------------
  // **** Update Logic ****
  // --------------------------------------------------------

  // s0
  val u = io.update
  val u_meta = u.bits.meta.asTypeOf(new FauFTBMeta)
  val u_s0_tag = getTag(u.bits.pc)

  ways.foreach(_.io.update_req_tag := u_s0_tag)
  val u_s0_hit_oh = VecInit(ways.map(_.io.update_hit)).asUInt
  val u_s0_hit = u_s0_hit_oh.orR

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

  // s1
  val u_s1_pc = RegNext(u.bits.pc)
  val u_s1_valid = RegNext(u.valid)
  val u_s1_commit_valid = RegNext(u.valid && u.bits.is_commit_update)
  val u_s1_tag       = RegEnable(u_s0_tag, u.valid)
  val u_s1_hit_oh    = RegEnable(u_s0_hit_oh, u.valid)
  val u_s1_hit       = RegEnable(u_s0_hit, u.valid)

  val u_s1_alloc_way = replacer.way
  val u_s1_write_way_oh = Mux(u_s1_hit, u_s1_hit_oh, UIntToOH(u_s1_alloc_way))

  val u_s1_ftb_entry = RegEnable(u.bits.ftb_entry, u.valid)
  val u_s1_ftb_entry_empty = !u_s1_ftb_entry.valid || !u_s1_ftb_entry.hasValidSlot // if the entry is invalid or contains no valid slot

  val u_s1_ways_write_valid = VecInit((0 until numWays).map(w => u_s1_write_way_oh(w).asBool && u_s1_valid && !u_s1_ftb_entry_empty))
  for (w <- 0 until numWays) {
    ways(w).io.write_valid := u_s1_ways_write_valid(w)
    ways(w).io.write_tag   := u_s1_tag
    ways(w).io.write_entry := u_s1_ftb_entry
  }

  // update saturating counters
  val u_s1_br_update_valids = RegEnable(u_s0_br_update_valids, u.valid)
  val u_s1_br_takens        = RegEnable(u.bits.br_taken_mask,  u.valid)
  for (w <- 0 until numWays) {
    when (u_s1_ways_write_valid(w)) {
      for (br <- 0 until numBr) {
        when (u_s1_br_update_valids(br)) {
          ctrs(w)(br) := satUpdate(ctrs(w)(br), 2, u_s1_br_takens(br))
        }
      }
    }
  }

  // commit update replacer state
  replacer_touch_ways(1).valid := u_s1_commit_valid
  replacer_touch_ways(1).bits  := OHToUInt(u_s1_write_way_oh)
  replacer.access(replacer_touch_ways)

  // --------------------------------------------------------
  // **** Debug Messages ****
  // --------------------------------------------------------

  if(enableFauFTBUpdateDetailPrint || enableWatchPC){
    val printCond = u.valid
    val watchCond = u.valid && u.bits.pc === watchPC.U
    val cond = if(enableFauFTBUpdateDetailPrint) printCond else watchCond
    XSDebug(cond, p"-------FauFTB update entry for PC : ${u.bits.pc}-------\n")
    u.bits.display(cond)
    XSDebug(cond, p"-----------------------------------\n")
  }

  // check fall through error
  val cond = u.valid && u.bits.ftb_entry.valid && u.bits.ftb_entry.getFallThrough(u.bits.pc) <= u.bits.pc
  XSDebug(cond, p"FauFTB fall through error for PC : 0x${Hexadecimal(u.bits.pc)}\n")
  u.bits.display(cond)
  XSDebug(cond, p"-----------------------------------\n")
  if(enableFauFTBInsertionPrint || enableWatchPC){
    val printCond = u_s1_valid && !u_s1_ftb_entry_empty
    val watchCond = u_s1_valid && !u_s1_ftb_entry_empty && u_s1_pc === watchPC.U
    val cond = if(enableFauFTBInsertionPrint) printCond else watchCond
    XSDebug(cond, p"-------FauFTB insert entry for PC : ${u_s1_pc}-------\n")
    u_s1_ftb_entry.display(cond)
    XSDebug(cond, p"-----------------------------------\n")
  }

}