package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix, WrapInc}

import scala.math.min

case class BoomFTBParams(
  nSets: Int = 128,
  nWays: Int = 8,
  offsetSz: Int = 13
)

