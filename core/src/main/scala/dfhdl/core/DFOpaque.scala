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

  abstract class Frontend2[T <: DFTypeAny](final protected[core] val actualType: T)(arg: Int)
      extends Abstract:
    type ActualType = T

  given [T <: Abstract](using ce: ClassEv[T]): DFOpaque[T] = DFOpaque(ce.value)

  def apply[T <: Abstract](
      t: T
  ): DFOpaque[T] =
    ir.DFOpaque(t.typeName, t, t.actualType.asIR).asFE[DFOpaque[T]]
  extension [T <: DFTypeAny, TFE <: Frontend[T]](dfType: DFOpaque[TFE])
    def actualType: T = dfType.asIR.actualType.asFE[T]

  type Token[T <: Abstract] = DFToken[DFOpaque[T]]
  object Token:
    def apply[T <: DFTypeAny, TFE <: Frontend[T]](
        tfe: TFE,
        token: T <> TOKEN
    ): Token[TFE] =
      ir.DFToken(DFOpaque(tfe).asIR)(token.asIR.data).asTokenOf[DFOpaque[TFE]]
    def forced[TFE <: Abstract](
        tfe: TFE,
        token: DFTokenAny
    ): Token[TFE] =
      ir.DFToken(DFOpaque(tfe).asIR)(token.asIR.data).asTokenOf[DFOpaque[TFE]]

    object TC:
      import DFToken.TC
      given DFOpaqueTokenFromDFOpaqueToken[
          T <: Abstract,
          RT <: Abstract,
          V <: DFOpaque[RT] <> TOKEN
      ](using dfc: DFC, st: RT <:< T): TC[DFOpaque[T], V] with
        def conv(dfType: DFOpaque[T], value: V): Out =
          value.asIR.asTokenOf[DFOpaque[T]]
  end Token

  object Val:
    object TC:
      import DFVal.TC
      given DFOpaqueValFromDFOpaqueVal[
          T <: Abstract,
          RT <: Abstract,
          V <: DFOpaque[RT] <> VAL
      ](using dfc: DFC, st: RT <:< T): TC[DFOpaque[T], V] with
        def conv(dfType: DFOpaque[T], value: V): Out =
          value.asIR.asValOf[DFOpaque[T]]
  end Val

end DFOpaque
