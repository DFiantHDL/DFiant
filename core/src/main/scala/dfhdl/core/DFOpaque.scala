package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.quoted.*

import scala.annotation.unchecked.uncheckedVariance

type DFOpaque[+T <: DFOpaque.Abstract] =
  DFType[ir.DFOpaque, Args1[T @uncheckedVariance]]
object DFOpaque:
  protected[core] sealed trait Abstract extends HasTypeName, ir.DFOpaque.CustomId:
    type ActualType <: DFTypeAny
    protected[core] val actualType: ActualType

  abstract class Frontend[T <: DFTypeAny](final protected[core] val actualType: T) extends Abstract:
    type ActualType = T

  def apply[T <: Abstract](
      t: T
  ): DFOpaque[T] = ???

  type Token[T <: Abstract] = DFToken[DFOpaque[T]]
  object Token:
    def apply[T <: DFTypeAny, TFE <: Frontend[T]](
        tfe: TFE,
        token: T <> TOKEN
    ): Token[TFE] = ???
    def forced[TFE <: Abstract](
        tfe: TFE,
        token: DFTokenAny
    ): Token[TFE] = ???

    object TC
  end Token

  object Val:
    object TC
  end Val

end DFOpaque
