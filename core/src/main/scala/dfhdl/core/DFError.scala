package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.targetName

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg)
    derives CanEqual

object DFError:
  final class Basic(
      val opName: String,
      val iae: IllegalArgumentException
  ) extends DFError(iae.getMessage)
  object FakeEnum extends DFError("This value of enum is no meant to be accessed.")
  final class Derived(from: DFError) extends DFError(from.dfMsg)

  extension (dfErr: DFError) def asFE[T <: DFTypeAny]: T = DFType(dfErr).asInstanceOf[T]
end DFError

class Logger:
  private var errors: List[DFError] = Nil
  def logError(err: DFError): Unit =
    errors = err :: errors
  def getErrors: List[DFError] = errors.reverse
  def clearErrors(): Unit = errors = Nil

def exitWithError(msg: String): Unit =
  System.err.println(msg)
  sys.exit(1)
