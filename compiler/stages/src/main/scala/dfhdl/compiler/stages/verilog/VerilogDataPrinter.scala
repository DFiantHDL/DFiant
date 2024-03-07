package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VerilogDataPrinter extends AbstractDataPrinter:
  type TPrinter <: VerilogPrinter
  val allowBitsBinModeInHex: Boolean = false
  val allowBitsExplicitWidth: Boolean = true
  def csDFBitBubbleChar: Char = 'x'
  def csDFBitsBinFormat(binRep: String): String = s"""${binRep.length}'b$binRep"""
  def csDFBitsHexFormat(hexRep: String): String = s"""${hexRep.length * 4}'h$hexRep"""
  def csDFBitsHexFormat(hexRep: String, width: IntParamRef): String =
    s"""${width.refCodeString.applyBrackets()}'h$hexRep"""
  def csDFBoolFormat(value: Boolean): String = if (value) "1" else "0"
  def csDFBitFormat(bitRep: String): String = csDFBitsBinFormat(bitRep)
  val allowDecimalBigInt: Boolean = true
  def csDFUIntFormatBig(value: BigInt, width: IntParamRef): String =
    s"""${width.refCodeString.applyBrackets()}'d$value"""
  def csDFSIntFormatBig(value: BigInt, width: IntParamRef): String =
    val csWidth = width.refCodeString.applyBrackets()
    if (value >= 0) s"""$csWidth'sd$value"""
    else s"""-$csWidth'sd${-value}"""
  def csDFUIntFormatSmall(value: BigInt, width: Int): String =
    csDFUIntFormatBig(value, IntParamRef(width))
  def csDFSIntFormatSmall(value: BigInt, width: Int): String =
    csDFSIntFormatBig(value, IntParamRef(width))
  def csDFUIntDataFromBits(csBits: String): String = s"""$$unsigned($csBits)"""
  def csDFSIntDataFromBits(csBits: String): String = s"""$$signed($csBits)"""
  def csDFUIntBubble(width: Int): String = bubbleBits(width)
  def csDFSIntBubble(width: Int): String = csDFSIntDataFromBits(bubbleBits(width))
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt]): String =
    data match
      case Some(value) =>
        val entryName = dfType.entries.find(_._2 == value).get._1
        s"${dfType.getName}_${entryName}"
      case None => "?"
  val maxElementsPerLine = 64
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String =
    given CanEqual[Any, Any] = CanEqual.derived
    if (data.allElementsAreEqual) s"'{${data.length}{${csConstData(dfType.cellType, data.head)}}}"
    else
      val allElements = data.view.grouped(maxElementsPerLine).map(line =>
        line.map(x => csConstData(dfType.cellType, x)).mkString(", ")
      ).mkString(",\n")
      if (allElements.contains("\n")) s"'{\n${allElements.hindent}\n}"
      else s"'{$allElements}"
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String =
    csConstData(dfType.actualType, data)
  def csDFStructData(dfType: DFStruct, data: List[Any]): String =
    dfType.fieldMap
      .lazyZip(data)
      .map { case ((n, t), d) =>
        s"$n: ${csConstData(t, d)}"
      }
      .mkString("'{", ", ", "}")
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String = printer.unsupported
  def csDFUnitData(dfType: DFUnit, data: Unit): String = printer.unsupported
end VerilogDataPrinter
