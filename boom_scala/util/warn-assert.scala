package boom.util

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
// import sourcecode.{File, Line}

object WarnAssert{
    def apply(condition: Bool, message: Printable, isFatal: Boolean = false): Unit = {
        when(!condition) {
            printf(s"Warning:")
            printf(message)
            printf("\n")
        }
        if(isFatal){
            assert(condition)
        }
    }
}
// def WarnAssert(condition: Bool, message: Printable, isFatal: Boolean = false)(implicit file: File, line: Line): Unit = {
//     when(!condition) {
//         printf(s"Warning: ${file.value}:${line.value}: ")
//         printf(message)
//         printf("\n")
//     }
//     if(isFatal) {
//         assert(condition)
//     }
// }
