/*package boom.ifu

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

trait VFTBParams extends HasBoomFTBParameters {
  val numWays = 16 // note: the uBTB in BOOM has 16 ways
  val tagSize = 20
  val numEntries = 2048
  val numWays    = 4
  val numSets    = numEntries/numWays // 256
  val extendedNSets = 128

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20

  def getTag(pc: UInt) = pc(tagSize+instOffsetBits-1, instOffsetBits)
}

class VFTB(implicit p: Parameters) extends BlockPredictorBank with VFTBParams {
  // uFTB memory array

  // --------------------------------------------------------
  // **** FauFTB Memory Array ****
  // --------------------------------------------------------

  val ways = Seq.tabulate(numWays)(w => Module(new FauFTBWay))
  val ctrs = Seq.tabulate(numWays)(w => Seq.tabulate(numBr)(b => RegInit(2.U(2.W))))
  val replacer = ReplacementPolicy.fromString("plru", numWays)
  val replacer_touch_ways = Wire(Vec(2, Valid(UInt(log2Ceil(numWays).W))))

  val doing_reset = RegInit(true.B)
  val reset_idx   = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }

  // --------------------------------------------------------
  // **** FTB Memory Array ****
  // --------------------------------------------------------
  val ftbAddr = new FTBTableAddr(log2Up(numSets), 1, tagSize)

  val tag = Seq.fill(nWays) {SyncReadMem(nSets, UInt(tagSize.W))}
  val ftb = Seq.fill(nWays) {SyncReadMem(nSets, new FTBEntry)}

  val ebtb     = SyncReadMem(extendedNSets, UInt(vaddrBitsExtended.W))

  val mems = (((0 until nWays) map ({w:Int => Seq(
    (f"ftb_tag_way$w", nSets, tagSize),
    (f"ftb_data_way$w", nSets, ftbEntrySz))})).flatten++ Seq(("ebtb", extendedNSets, vaddrBitsExtended)))
  
  
  // --------------------------------------------------------
  // **** FauFTB Prediction Logic ****
  // --------------------------------------------------------
  
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
  io.resp.f3 := RegNext(io.resp.f2)

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
  val u_s0_valid = u.valid && u.bits.ftb_entry.valid && u.bits.ftb_entry.hasValidSlot && (u.bits.is_commit_update || u.bits.is_btb_mispredict_update)

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
  val u_s1_valid = RegNext(u_s0_valid)
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
  val u_s1_need_evict = u_s1_valid && !u_s1_hit && !u_s1_ftb_entry_empty
  val u_s1_evict_entry = Mux1H(u_s1_write_way_oh, s1_all_entries)

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

}
*/