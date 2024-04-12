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
  val fauftbNumWays = 16 // note: the uBTB in BOOM has 16 ways
  val fauftbTagSize = vaddrBitsExtended - instOffsetBits

  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20

  def getTag(pc: UInt) = pc(fauftbTagSize+instOffsetBits-1, instOffsetBits)
}

// class FTBEntry(implicit p: Parameters) extends FTBEntry()(p) {}

class FauFTBWay(implicit p: Parameters) extends BoomModule()(p) with FauFTBParams {
  val io = IO(new Bundle{
    val req_tag = Input(UInt(fauftbTagSize.W))
    val resp = Output(new FTBEntry)
    val resp_hit = Output(Bool())
    val update_req_tag = Input(UInt(fauftbTagSize.W))
    val update_hit = Output(Bool())
    val write_valid = Input(Bool())
    val write_entry = Input(new FTBEntry)
    val write_tag = Input(UInt(fauftbTagSize.W))
    val tag_read = Output(UInt(fauftbTagSize.W))
  })

  val data = Reg(new FTBEntry)
  val tag = Reg(UInt(fauftbTagSize.W))
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

class FauFTBMeta(implicit p: Parameters) extends BoomBundle with FauFTBParams {
  // val pred_way = UInt(log2Ceil(fauftbNumWays).W)
  val hit = Bool()
}

class FauFTBImpl(implicit p: Parameters) extends BoomModule()(p) with FauFTBParams with BPUUtils {
  val io = IO(new Bundle{
    val update_pc = Input(Valid(UInt(vaddrBitsExtended.W)))
    val update_entry = Input(new FTBEntry)
    val update_meta = Input(new FTBMeta)
    val is_commit_update = Input(Bool())
    val is_btb_mispredict_update = Input(Bool())
    val br_taken_mask = Input(Vec(numBr, Bool()))
    val br_update_mask = Input(Vec(numBr, Bool()))
    val req_pc = Input(Valid(UInt(vaddrBitsExtended.W)))
    val resp_entry = Output(new FTBEntry)
    val resp_br_taken = Output(Vec(numBr, Bool()))
    val evict_entry = Output(Valid(new FTBEntry))
    val evict_tag = Output(UInt(fauftbTagSize.W))
    val meta = Output(new FTBMeta)
  })
  val resp_meta = Wire(new FauFTBMeta)
  resp_meta := DontCare
  val meta_size = resp_meta.getWidth

  val ways = Seq.tabulate(fauftbNumWays)(w => Module(new FauFTBWay))
  val ctrs = Seq.tabulate(fauftbNumWays)(w => Seq.tabulate(numBr)(b => RegInit(2.U(2.W))))
  val replacer = ReplacementPolicy.fromString("plru", fauftbNumWays)
  val replacer_touch_ways = Wire(Vec(2, Valid(UInt(log2Ceil(fauftbNumWays).W))))

  val mems = Nil


  // --------------------------------------------------------
  // **** Prediction Logic ****
  // --------------------------------------------------------
  val s1_pc = RegNext(io.req_pc.bits)
  val s1_valid = RegNext(io.req_pc.valid)

  ways.foreach(_.io.req_tag := getTag(s1_pc)) // use the F1 pc as the BOOM.ubtb and XiangShan.FauFTB do
  
  // pred resp
  val s1_all_entries = VecInit(ways.map(_.io.resp))
  val s1_all_tags = VecInit(ways.map(_.io.tag_read))
  val s1_hit_oh = VecInit(ways.map(_.io.resp_hit)).asUInt
  val s1_hit = s1_hit_oh.orR
  val s1_hit_way = OHToUInt(s1_hit_oh)

  val s1_hit_ftb_entry = Mux(s1_hit_oh =/= 0.U, Mux1H(s1_hit_oh, s1_all_entries), 0.U.asTypeOf(new FTBEntry))
  XSError(PopCount(s1_hit_oh) > 1.U, "fauftb has multiple hits!\n")
  val raw_ctr = Mux1H(s1_hit_oh, VecInit((0 until fauftbNumWays).map(w => VecInit((0 until numBr).map(b => ctrs(w)(b))))))
  val taken_mask = VecInit((0 until numBr).map(b => s1_hit_ftb_entry.validSlots(b) && (raw_ctr(b)(1) || s1_hit_ftb_entry.always_taken(b))))

  io.resp_entry := s1_hit_ftb_entry
  io.resp_br_taken := taken_mask

  replacer_touch_ways(0).valid := RegNext(s1_valid && s1_hit)
  replacer_touch_ways(0).bits := RegEnable(s1_hit_way, s1_valid && s1_hit)
  // --------------------------------------------------------
  // **** Update Logic ****
  // --------------------------------------------------------

  // s0
  val u_s0_pc = io.update_pc.bits
  val u_s0_tag = getTag(io.update_pc.bits)
  val u_s0_valid = io.update_pc.valid && io.update_entry.valid && io.update_entry.hasValidSlot && (io.is_commit_update || io.is_btb_mispredict_update)
  val u_s0_commit_valid = io.is_commit_update && io.update_pc.valid

  ways.foreach(_.io.update_req_tag := u_s0_tag)
  val u_s0_hit_oh = VecInit(ways.map(_.io.update_hit)).asUInt
  val u_s0_hit = u_s0_hit_oh.orR

  val u_s0_br_update_valids = io.br_update_mask
  val u_s0_hit_way = Wire(UInt(log2Ceil(fauftbNumWays).W))
  u_s0_hit_way := OHToUInt(u_s0_hit_oh)

  // s1
  val u_s1_pc = RegNext(u_s0_pc)
  val u_s1_valid = RegNext(u_s0_valid)
  val u_s1_tag       = RegNext(u_s0_tag)
  val u_s1_hit_oh    = RegNext(u_s0_hit_oh)
  val u_s1_hit       = RegNext(u_s0_hit)
  val u_s1_hit_way   = RegNext(u_s0_hit_way)
  val u_s1_commit_valid = RegNext(u_s0_commit_valid)

  val u_s1_alloc_way = replacer.way
  val u_s1_write_way_oh = Mux(u_s1_hit, u_s1_hit_oh, UIntToOH(u_s1_alloc_way)).asUInt

  val u_s1_ftb_entry = RegNext(io.update_entry)
  val u_s1_ftb_entry_empty = !u_s1_ftb_entry.valid || !u_s1_ftb_entry.hasValidSlot // if the entry is invalid or contains no valid slot

  val u_s1_ways_write_valid = VecInit((0 until fauftbNumWays).map(w => u_s1_write_way_oh(w).asBool && u_s1_valid && !u_s1_ftb_entry_empty))
  for (w <- 0 until fauftbNumWays) {
    ways(w).io.write_valid := u_s1_ways_write_valid(w)
    ways(w).io.write_tag   := u_s1_tag
    ways(w).io.write_entry := u_s1_ftb_entry
  }

  // update saturating counters
  val u_s1_br_update_valids = RegNext(u_s0_br_update_valids)
  val u_s1_br_takens        = RegNext(io.br_taken_mask)
  for (w <- 0 until fauftbNumWays) {
    when (u_s1_ways_write_valid(w)) {
      for (br <- 0 until numBr) {
        when (u_s1_br_update_valids(br)) {
          ctrs(w)(br) := satUpdate(ctrs(w)(br), 2, u_s1_br_takens(br))
        }
      }
    }
  }

  io.evict_entry.valid := u_s1_valid && !u_s1_hit && !u_s1_ftb_entry_empty
  io.evict_entry.bits := Mux1H(u_s1_write_way_oh, s1_all_entries)
  io.evict_tag := Mux1H(u_s1_write_way_oh, s1_all_tags)

  val u_s1_write_way = Wire(UInt(log2Ceil(fauftbNumWays).W))
  u_s1_write_way :=Mux(u_s1_hit, RegNext(u_s0_hit_way), u_s1_alloc_way)
  // commit update replacer state
  replacer_touch_ways(1).valid := u_s1_commit_valid
  replacer_touch_ways(1).bits  := u_s1_write_way
  replacer.access(replacer_touch_ways)
}

class FauFTB(implicit p: Parameters) extends BlockPredictorBank with FauFTBParams {
  val resp_meta = Wire(new FauFTBMeta)
  resp_meta := DontCare
  val meta_size = resp_meta.getWidth
  val mems = Nil

  val fauftbImpl = Module(new FauFTBImpl)

  // --------------------------------------------------------
  // **** Update Logic ****
  // --------------------------------------------------------

  // s0
  val u = io.update
  val u_s0_valid = u.valid && u.bits.ftb_entry.valid && u.bits.ftb_entry.hasValidSlot && (u.bits.is_commit_update || u.bits.is_btb_mispredict_update)

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

  // --------------------------------------------------------
  // **** Prediction Logic ****
  // --------------------------------------------------------
  fauftbImpl.io.req_pc.valid := s0_valid
  fauftbImpl.io.req_pc.bits := s0_pc

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

  io.resp.f2 := RegNext(io.resp.f1)
  io.resp.f3 := RegNext(RegNext(io.resp.f1))

  io.resp.last_stage_entry := RegNext(RegNext(s1_ftb_entry))
  io.resp.f3_meta := resp_meta.asUInt

}