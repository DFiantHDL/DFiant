package dfhdl.core

object ir:
  sealed trait DFType
  sealed trait DFBoolOrBit extends DFType
  case object DFBit extends DFBoolOrBit

final class DFError extends Exception("") derives CanEqual

final class DFType[+T](val value: T | DFError) extends AnyVal:
  override def toString: String = value.toString
type DFTypeAny = DFType[ir.DFType]

object DFType:
  extension [T <: ir.DFType](dfType: DFType[T])
    def asIR: T = dfType.value match
      case dfTypeIR: T @unchecked => dfTypeIR
      case err: DFError           => throw new DFError
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = new DFType(dfType).asInstanceOf[T]
  export DFBoolOrBit.given

  trait TC[T]:
    type Type <: DFTypeAny
    def apply(t: T): Type
  object TC:
    transparent inline given ofDFType[T <: DFTypeAny]: TC[T] = new TC[T]:
      type Type = T
      def apply(t: T): Type = t
  end TC
end DFType

export DFType.asFE

object Timer

type DFBoolOrBit = DFType[ir.DFBoolOrBit]
object DFBoolOrBit:
  given DFBit = DFBit
end DFBoolOrBit

type DFBit = DFType[ir.DFBit.type]
final lazy val DFBit = ir.DFBit.asFE[DFBit]

trait Width[T]:
  type Out <: Int
object Width:
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
end Width

extension [T](t: T)(using tc: DFType.TC[T]) def width: Unit = tc(t).asIR
