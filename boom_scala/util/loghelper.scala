package boom.util

// import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.config.{Field, Parameters}
import chisel3._
// import xiangshan.DebugOptionsKey
// import boom.util.DebugOptionsKey
import boom.util.XSLogLevel.XSLogLevel
// import boom.utility.{GTimer, LogPerfHelper, LogPerfIO}
import chisel3._
import chisel3.util.HasBlackBoxInline

class LogPerfIO extends Bundle {
  val timer = UInt(64.W)
  val logEnable = Bool()
  val clean = Bool()
  val dump = Bool()
}

class LogPerfHelper extends BlackBox with HasBlackBoxInline {
  val io = IO(Output(new LogPerfIO))

  // val verilog =
  //   """`ifndef SIM_TOP_MODULE_NAME
  //     |  `define SIM_TOP_MODULE_NAME SimTop
  //     |`endif
  //     |
  //     |/*verilator tracing_off*/
  //     |
  //     |module LogPerfHelper (
  //     |  output [63:0] timer,
  //     |  output        logEnable,
  //     |  output        clean,
  //     |  output        dump
  //     |);
  //     |
  //     |assign timer         = `SIM_TOP_MODULE_NAME.timer;
  //     |assign logEnable     = `SIM_TOP_MODULE_NAME.logEnable;
  //     |assign clean         = `SIM_TOP_MODULE_NAME.clean;
  //     |assign dump          = `SIM_TOP_MODULE_NAME.dump;
  //     |
  //     |endmodule
  //     |
  //     |""".stripMargin
  // setInline("LogPerfHelper.v", verilog)
}


case class DebugOptions
(
  FPGAPlatform: Boolean = false,
  EnableDifftest: Boolean = false,
  AlwaysBasicDiff: Boolean = true,
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  UseDRAMSim: Boolean = false,
  EnableConstantin: Boolean = false,
  EnableChiselDB: Boolean = false,
  AlwaysBasicDB: Boolean = true,
  EnableRollingDB: Boolean = false
)

case object DebugOptionsKey extends Field[DebugOptions]

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value

  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val PERF  = Value("PERF ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

object XSLog {
  val MagicStr = "__PERCENTAGE_M__"
  def apply(debugLevel: XSLogLevel, ctrlInfoOpt: Option[LogPerfIO] = None)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Any =
  {
    // val debugOpts = p(DebugOptionsKey)
    // val enableDebug = debugOpts.EnableDebug && debugLevel != XSLogLevel.PERF
    // val enablePerf = debugOpts.EnablePerfDebug && debugLevel == XSLogLevel.PERF
    val enableDebug = true
    val enablePerf = false
    if ( (enableDebug || enablePerf || debugLevel == XSLogLevel.ERROR)) {
      // val ctrlInfo = ctrlInfoOpt.getOrElse(Module(new LogPerfHelper).io)
      val logEnable = true
      // val logTimestamp = ctrlInfo.timer
      when (cond) {
        val commonInfo = p" $MagicStr: "
        printf((if (prefix) commonInfo else p"") + pable)
        if (debugLevel >= XSLogLevel.ERROR) {
          assert(false.B)
        }
      }
    }
  }
  def apply(debugLevel: XSLogLevel, ctrlInfo: LogPerfIO)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Any = {
    apply(debugLevel, Some(ctrlInfo))(prefix, cond, pable)
  }
}

sealed abstract class LogHelper(val logLevel: XSLogLevel){

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Any =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit p: Parameters): Any = apply(false, cond, pable)
  def apply(fmt: String, data: Bits*)(implicit p: Parameters): Any =
    apply(Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit p: Parameters): Any = apply(false.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Any =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Any ={
    XSLog(logLevel)(prefix, cond, pable)
  }
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)
