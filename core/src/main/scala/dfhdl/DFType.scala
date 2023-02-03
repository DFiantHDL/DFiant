package dfhdl

case object DFBit

final class DFError extends Exception("") derives CanEqual
final class DFType[+T](val value: T | DFError) extends AnyVal

extension (dfType: DFType[DFBit.type])
  def asIR: DFBit.type = dfType.value match
    case dfTypeIR: DFBit.type => dfTypeIR
    case err: DFError         => throw new DFError

final lazy val Bit = new DFType[DFBit.type](DFBit)
