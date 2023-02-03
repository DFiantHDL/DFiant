package dfhdl
package core
import compiler.ir

type DFBoolOrBit = DFType[ir.DFBoolOrBit]
object DFBoolOrBit:
  given DFBool = DFBool
  given DFBit = DFBit
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type]
final lazy val DFBit = ir.DFBit.asFE[DFBit]

trait Width[T]:
  type Out <: Int
object Width:
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
end Width

extension [T](t: T)(using tc: DFType.TC[T]) def width: Unit = tc(t).asIR
