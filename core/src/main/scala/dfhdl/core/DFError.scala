package dfhdl.core
import dfhdl.compiler.ir

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg)
    derives CanEqual

object DFError:
  final class Basic(
      val opName: String,
      val iae: IllegalArgumentException
  ) extends DFError(iae.getMessage)
  final class Derived(from: DFError) extends DFError(from.dfMsg)

  extension (dfErr: DFError) def asFE[T <: DFTypeAny]: T = DFType(dfErr).asInstanceOf[T]
end DFError
