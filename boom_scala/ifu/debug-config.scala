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
  val enableWatchPC = false
  val enableF3MaskPrint = false
  val enbaleBankPredictorUpdatePrint = false
  val enableFauFTBInsertionPrint = false // print the insertion infos in FauFTB
  val watchPC = 2147492914L
}
