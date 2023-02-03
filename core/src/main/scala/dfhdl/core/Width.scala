package dfhdl
package core
import internals.*

trait Width[T]:
  type Out <: Int
object Width:
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
end Width

extension [T](t: T)(using tc: DFType.TC[T])
  def width(using w: Width[tc.Type]): Inlined[w.Out] =
    Inlined.forced[w.Out](tc(t).asIR.width)
