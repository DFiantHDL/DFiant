package dfhdl.core

object ir:
  sealed trait DFType
  case object DFBit extends DFType

final class DFError extends Exception("") derives CanEqual
final class DFType[+T](val value: T | DFError) extends AnyVal

extension [T <: ir.DFType](dfType: DFType[T])
  def asIR: T = dfType.value match
    case dfTypeIR: T @unchecked =>
      println("Match OK!")
      dfTypeIR
    case err: DFError => throw new DFError

object Timer

final lazy val DFBit = new DFType[ir.DFBit.type](ir.DFBit)

extension [T](t: T) def width: Unit = DFBit.asIR
