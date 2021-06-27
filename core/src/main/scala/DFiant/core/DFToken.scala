package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import DFiant.internals.*

import scala.annotation.implicitNotFound

opaque type DFToken = ir.DFType.Token
object DFToken:
  extension (of: DFToken)
    def asIR: ir.DFType.Token = of
    def codeString(using printer: Printer): String = printer.csDFToken(asIR)

  opaque type Of[+T <: DFType, D] <: DFToken = DFToken
  object Of:
    extension [T <: DFType, D](token: Of[T, D])
      def data: D = token.asIR.data.asInstanceOf[D]
      def dfType: T = token.asIR.dfType.asInstanceOf[T]
      def width(using w: Width[T]): Inlined.Int[w.Out] =
        Inlined.Int.forced[w.Out](token.asIR.width)
  @implicitNotFound("Unsupported token value ${V} for type ${T}")
  trait TC[T <: DFType, V]:
    type Out <: DFToken
    def apply(dfType: T, value: V): Out
  object TC:
    /////////////////////////////////////////////////////////////////////////////
    // DFBool Token
    /////////////////////////////////////////////////////////////////////////////
    given DFBoolTokenFromBubble[V <: Bubble]: TC[DFBoolOrBit, V] with
      type Out = DFBoolOrBit.Token
      def apply(dfType: DFBoolOrBit, value: V): Out =
        DFBoolOrBit.Token(dfType, value)
    given DFBoolTokenFromBoolean[V <: Boolean]: TC[DFBoolOrBit, V] with
      type Out = DFBoolOrBit.Token
      def apply(dfType: DFBoolOrBit, value: V): Out =
        DFBoolOrBit.Token(dfType, value)
    given DFBoolTokenFrom1Or0[V <: 0 | 1]: TC[DFBoolOrBit, V] with
      type Out = DFBoolOrBit.Token
      def apply(dfType: DFBoolOrBit, value: V): Out =
        DFBoolOrBit.Token(dfType, value)
    given DFBoolTokenFromToken[V <: DFBoolOrBit.Token]: TC[DFBoolOrBit, V] with
      type Out = DFBoolOrBit.Token
      def apply(dfType: DFBoolOrBit, value: V): Out =
        ??? //DFBoolOrBit.Token(dfType, value.data)

    /////////////////////////////////////////////////////////////////////////////
    // DFBits Token
    /////////////////////////////////////////////////////////////////////////////
    object `W == VW`
        extends Check2[
          Int,
          Int,
          [W <: Int, VW <: Int] =>> W == VW,
          [W <: Int, VW <: Int] =>> "The token width (" +
            ToString[VW] +
            ") is different than the DFType width (" +
            ToString[W] +
            ")."
        ]
    given DFBitsTokenFromBubble[W <: Int, V <: Bubble]: TC[DFBits[W], V] with
      type Out = DFBits.Token[W]
      def apply(dfType: DFBits[W], value: V): Out =
        ??? //DFBits.Token[W](dfType.width, value)
    given DFBitsTokenFromSBV[W <: Int, V <: SameBitsVector]: TC[DFBits[W], V]
      with
      type Out = DFBits.Token[W]
      def apply(dfType: DFBits[W], value: V): Out =
        ??? //DFBits.Token[W](dfType.width, value)
    given DFBitsTokenFromToken[W <: Int, VW <: Int](using
        check: `W == VW`.Check[W, VW]
    ): TC[DFBits[W], DFBits.Token[VW]] with
      type Out = DFBits.Token[W]
      def apply(dfType: DFBits[W], value: DFBits.Token[VW]): Out =
        check(dfType.width, value.width)
        ??? //DFBits.Token[W](dfType, value.data)

  end TC
