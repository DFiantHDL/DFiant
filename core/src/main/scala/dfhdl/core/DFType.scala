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
end DFType

export DFType.asFE
object Timer

type DFBit = DFType[ir.DFBit.type]
final lazy val DFBit = ir.DFBit.asFE[DFBit]

extension [T](t: T) def width: Unit = DFBit.asIR
