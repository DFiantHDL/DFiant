package dfhdl
package core

trait Width[T]:
  type Out <: Int
object Width:
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
end Width

extension [T](t: T)(using tc: DFType.TC[T]) def width: Unit = tc(t).asIR
