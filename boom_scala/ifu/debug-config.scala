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
  val watchPC = 0x00800020a8L 
  val enableF3MaskPrint = false
  val enbaleBankPredictorUpdatePrint = false
  val enableFauFTBInsertionPrint = false // print the insertion infos in FauFTB
  val enableFauFTBUpdateDetailPrint = false // print the update details in FauFTB
  val enableF0PCPrint = false // print info about F0 NPC
  val enableF1RedirectInfoPrint = false // print info about F1 NPC
  val enableF2RedirectInfoPrint = false // print info about F2 NPC
  val enableF3RedirectInfoPrint = false // print info about F3 redirect
  val enableF5RedirectInfoPrint = false// print info about F5 redirect

  val enableF1vsF2BPRespDiff = false
}
