package dfhdl.core
import dfhdl.compiler.ir
import ir.{Time, Freq, Ratio}
import ir.Timer.Func.Op as FuncOp
import dfhdl.internals.*

sealed class Timer private (val irValue: ir.Timer | DFError) extends DFMember[ir.Timer]
object Timer:
  extension (timer: ir.Timer) private def asFE: Timer = new Timer(timer)
  type Period = Time | Freq
  extension (period: Period)
    def time = period match
      case t: Time => t
      case f: Freq => f.period
end Timer
