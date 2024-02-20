package boom.ifu

import chisel3._
import chisel3.util._

import boom.common._
import boom.util._

import scala.math.min

trait hasFTBDebugConfigs {
  /* Debug log control */
  val enableF4FTBGenIOPrint = false // print the input and output of FTBEntryGen in F4
  val enableFTBGenInternalPrint = false // print the internal signals of FTBEntryGen
  val enableF4BTBCorrectionInputPrint = false // print the input of BTBCorrection in F4
  val enableWatchPC = true
  val watchPC = 0x80002038L
  val enableF3MaskPrint = false
  val enbaleBankPredictorUpdatePrint = false
  val enableFauFTBInsertionPrint = false // print the insertion infos in FauFTB
  val enableFauFTBUpdateDetailPrint = false // print the update details in FauFTB
  val enableF0PCPrint = false // print info about F0 NPC
  val enableF1RedirectInfoPrint = true // print info about F1 NPC
  val enableF2RedirectInfoPrint = true // print info about F2 NPC
  val enableF2ZeroVPCPrint = false
  val enableF3RedirectInfoPrint = true // print info about F3 redirect
  val enableF3ZeroVPCPrint = false
  val enableF5RedirectInfoPrint = true// print info about F5 redirect
  val enablePCTracePrint = true//

  val enableFTBUpdateDetailPrint = true
  val enableFTBPredictPrint = true
  val enableF1vsF2BPRespDiff = false

  val enableCommitTracePrint = true
}
