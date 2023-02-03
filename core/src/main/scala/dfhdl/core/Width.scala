package dfhdl
package core
import internals.*
import scala.quoted.*
import compiler.ir
import annotation.targetName

trait Width[T]:
  type Out <: Int
object Width:
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
end Width

extension [T <: DFTypeAny](token: DFToken[T])
  @targetName("tokenWidth")
  def width(using w: Width[T]): Inlined[w.Out] =
    Inlined.forced[w.Out](token.asIR.width)

extension [T](t: T)(using tc: DFType.TC[T])
  @targetName("tWidth")
  def width(using w: Width[tc.Type]): Inlined[w.Out] =
    Inlined.forced[w.Out](tc(t).asIR.width)
