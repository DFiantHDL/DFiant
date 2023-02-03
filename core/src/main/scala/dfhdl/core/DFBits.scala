package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import scala.util.control.NonLocalReturns.*
type DFBits[W <: Int] = DFType[ir.DFBits, Args1[W]]
import DFDecimal.Constraints.`LW == RW`

object DFBits:
  def apply[W <: Int](width: Inlined[W])(using
      check: Arg.Width.Check[W]
  ): DFBits[W] =
    check(width)
    ir.DFBits(width).asFE[DFBits[W]]
  @targetName("applyNoArg")
  def apply[W <: Int with Singleton](using ValueOf[W])(using
      Arg.Width.Check[W]
  ): DFBits[W] =
    DFBits[W](Inlined.forced[W](valueOf[W]))

  given [W <: Int](using ValueOf[W])(using
      Arg.Width.Check[W]
  ): DFBits[W] = DFBits[W](Inlined.forced[W](valueOf[W]))

  type Token[W <: Int] = CompanionsDFBits.Token[W]
  val Token = CompanionsDFBits.Token
  val Val = CompanionsDFBits.Val
end DFBits

private object CompanionsDFBits:
  protected object `AW == TW`
      extends Check2[
        Int,
        Int,
        [AW <: Int, TW <: Int] =>> AW == TW,
        [AW <: Int, TW <: Int] =>> "The alias width (" + AW +
          ") is different than the dataflow value width (" + TW + ")."
      ]
  protected object `LW >= RW`
      extends Check2[
        Int,
        Int,
        [LW <: Int, RW <: Int] =>> LW >= RW,
        [LW <: Int, RW <: Int] =>> "The new width (" + RW +
          ") is larger than the original width (" + LW + ")."
      ]
  protected[core] object BitIndex
      extends Check2[
        Int,
        Int,
        [I <: Int, W <: Int] =>> (I < W) && (I >= 0),
        [I <: Int, W <: Int] =>> "Index " + I + " is out of range of width/length " + W
      ]
  protected object BitsHiLo
      extends Check2[
        Int,
        Int,
        [H <: Int, L <: Int] =>> H >= L,
        [H <: Int, L <: Int] =>> "Low index " + L + " is bigger than High bit index " + H
      ]
  trait CompareCheck[
      ValW <: Int,
      ArgW <: Int,
      Castle <: Boolean // castling of dfVal and arg
  ]:
    def apply(dfValWidth: Int, argWidth: Int): Unit
  given [
      ValW <: Int,
      ArgW <: Int,
      Castle <: Boolean
  ](using
      lw: Id[ITE[Castle, ArgW, ValW]],
      rw: Id[ITE[Castle, ValW, ArgW]]
  )(using
      checkW: `LW == RW`.Check[lw.Out, rw.Out],
      castle: ValueOf[Castle]
  ): CompareCheck[ValW, ArgW, Castle] with
    def apply(dfValWidth: Int, argWidth: Int): Unit =
      val lw = if (castle) argWidth else dfValWidth
      val rw = if (castle) dfValWidth else argWidth
      checkW(lw, rw)
  end given

  type Token[W <: Int] = DFToken[DFBits[W]]
  object Token:
    protected[core] def apply[W <: Int](
        dfType: DFBits[W],
        data: (BitVector, BitVector)
    ): Token[W] =
      ir.DFToken(dfType.asIR)(data).asTokenOf[DFBits[W]]
    protected[core] def apply[W <: Int](
        width: Inlined[W],
        valueBits: BitVector,
        bubbleBits: BitVector
    ): Token[W] =
      Token(DFBits(width), (valueBits, bubbleBits))
    protected[core] def apply[W <: Int](
        width: Inlined[W],
        value: Bubble
    ): Token[W] =
      Token(
        width,
        BitVector.low(width.value),
        BitVector.high(width.value)
      )
    protected[core] def apply[W <: Int, T <: BitOrBool](
        width: Inlined[W],
        sev: SameElementsVector[T]
    ): Token[W] =
      val boolVal = sev.value match
        case b: Boolean => b
        case i: Int     => i > 0
      Token(
        width,
        BitVector.fill(width.value)(boolVal),
        BitVector.low(width.value)
      )
    extension [W <: Int](token: DFBits.Token[W])
      def valueBits: BitVector = token.data._1
      def bubbleBits: BitVector = token.data._2

    object Conversions:
      given DFBitsTokenConversionSing[W <: Int & Singleton, V](using
          tc: DFToken.TC[DFBits[W], V],
          w: ValueOf[W]
      ): Conversion[V, DFBits[W] <> TOKEN] = value => tc(DFBits(valueOf[W]), value)
    end Conversions

    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a Bits token."
    )
    trait Candidate[R]:
      type OutW <: Int
      def apply(arg: R): Token[OutW]
    protected trait CandidateLP:
      protected inline val intErrMsg =
        "An integer value cannot be a candidate for a Bits type.\nTry explicitly using a decimal token via the `d\"<width>'<number>\"` string interpolation."
      inline given errorOnInt[V <: Int]: Candidate[V] =
        compiletime.error(intErrMsg)
    object Candidate extends CandidateLP:
      type Aux[R, W <: Int] = Candidate[R] { type OutW = W }
      transparent inline given fromDFBitsToken[W <: Int, R <: Token[W]]: Candidate[R] =
        new Candidate[R]:
          type OutW = W
          def apply(arg: R): Token[OutW] = arg
      transparent inline given fromDFUIntToken[W <: Int, R <: DFUInt.Token[W]]: Candidate[R] =
        new Candidate[R]:
          type OutW = W
          def apply(arg: R): Token[OutW] = ???
      transparent inline given fromDFBitCandidate[R, T <: DFBoolOrBit](using
          ic: DFBoolOrBit.Token.Candidate.Aux[R, T]
      )(using T =:= DFBit): Candidate[R] = ???
      private def valueToBits(value: Any): DFBits[Int] <> TOKEN = ???
      transparent inline given fromTuple[R <: NonEmptyTuple, V <: NonEmptyTuple]: Candidate[V] = ${
        DFBitsMacro[V]
      }
      def DFBitsMacro[V](using
          Quotes,
          Type[V]
      ): Expr[Candidate[V]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[V]
        val wType = rTpe.calcValWidth(true).asTypeOf[Int]
        '{
          new Candidate[V]:
            type OutW = wType.Underlying
            def apply(value: V): DFToken[DFBits[OutW]] =
              valueToBits(value).asIR.asTokenOf[DFBits[OutW]]
        }
      end DFBitsMacro
    end Candidate

    object TC:
      import DFToken.TC
      protected object `W == VW`
          extends Check2[
            Int,
            Int,
            [W <: Int, VW <: Int] =>> W == VW,
            [W <: Int, VW <: Int] =>> "The token width (" + VW +
              ") is different than the DFType width (" + W + ")."
          ]

      // TODO: minimize error when removing aux pattern
      given DFBitsTokenFromCandidate[W <: Int, R, VW <: Int](using
          ic: Candidate.Aux[R, VW]
      )(using check: `W == VW`.Check[W, VW]): TC[DFBits[W], R] with
        def conv(dfType: DFBits[W], value: R): Out =
          val tokenArg = ic(value)
          check(dfType.width, tokenArg.asIR.width)
          tokenArg.asInstanceOf[Out]

      given DFBitsTokenFromSEV[W <: Int, T <: BitOrBool, V <: SameElementsVector[T]]
          : TC[DFBits[W], V] with
        def conv(dfType: DFBits[W], value: V): Out =
          DFBits.Token(dfType.width, value)
    end TC

    private val widthExp = "([0-9]+)'(.*)".r
    def fromBinString(
        bin: String
    ): Either[String, (BitVector, BitVector)] = returning[Either[String, (BitVector, BitVector)]] {
      val (explicitWidth, word) = bin match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, bin)
      val (valueBits, bubbleBits) =
        word.foldLeft((BitVector.empty, BitVector.empty)) {
          case (t, '_' | ' ') => t // ignoring underscore or space
          case ((v, b), c) =>
            c match // bin mode
              case '?' => (v :+ false, b :+ true)
              case '0' => (v :+ false, b :+ false)
              case '1' => (v :+ true, b :+ false)
              case x =>
                throwReturn[Either[String, (BitVector, BitVector)]](
                  Left(s"Found invalid binary character: $x")
                )
        }
      val actualWidth = valueBits.lengthOfValue.toInt
      explicitWidth match
        case Some(width) if width < actualWidth =>
          Left(
            s"Explicit given width ($width) is smaller than the actual width ($actualWidth)"
          )
        case Some(width) =>
          Right((valueBits.resize(width), bubbleBits.resize(width)))
        case None => Right((valueBits, bubbleBits))
    }
    private val isHex = "[0-9a-fA-F]".r
    def fromHexString(
        hex: String
    ): Either[String, (BitVector, BitVector)] = returning[Either[String, (BitVector, BitVector)]] {
      val (explicitWidth, word) = hex match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, hex)
      val (valueBits, bubbleBits, binMode) =
        word.foldLeft((BitVector.empty, BitVector.empty, false)) {
          case (t, '_' | ' ') => t // ignoring underscore or space
          case ((v, b, false), c) =>
            c match // hex mode
              case '{' => (v, b, true)
              case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
              case isHex() =>
                (
                  v ++ BitVector.fromHex(c.toString).get,
                  b ++ BitVector.low(4),
                  false
                )
              case x =>
                throwReturn[Either[String, (BitVector, BitVector)]](
                  Left(s"Found invalid hex character: $x")
                )
          case ((v, b, true), c) =>
            c match // bin mode
              case '}' => (v, b, false)
              case '?' => (v :+ false, b :+ true, true)
              case '0' => (v :+ false, b :+ false, true)
              case '1' => (v :+ true, b :+ false, true)
              case x =>
                throwReturn[Either[String, (BitVector, BitVector)]](
                  Left(s"Found invalid binary character in binary mode: $x")
                )
        }
      if (binMode) Left(s"Missing closing braces of binary mode")
      else
        val actualWidth = valueBits.lengthOfValue.toInt
        explicitWidth match
          case Some(width) if width < actualWidth =>
            Left(
              s"Explicit given width ($width) is smaller than the actual width ($actualWidth)"
            )
          case Some(width) =>
            Right((valueBits.resize(width), bubbleBits.resize(width)))
          case None => Right((valueBits, bubbleBits))
    }

    object StrInterp:
      class BParts[P <: Tuple](parts: P):
        transparent inline def apply(inline args: Any*): Any =
          ${ applyMacro('{ "b" })('parts, 'args) }
        transparent inline def unapplySeq[T <: DFTypeAny](
            inline arg: DFValOf[T]
        )(using DFC): Option[Seq[Any]] =
          ${ unapplySeqMacro('{ "b" })('parts, 'arg) }

      class HParts[P <: Tuple](parts: P):
        transparent inline def apply(inline args: Any*): Any =
          ${ applyMacro('{ "h" })('parts, 'args) }
        transparent inline def unapplySeq[T <: DFTypeAny](
            inline arg: DFValOf[T]
        )(using DFC): Option[Seq[Any]] =
          ${ unapplySeqMacro('{ "h" })('parts, 'arg) }

      extension (inline sc: StringContext)
        /** Binary Bits Vector Token String Interpolator
          *
          * Interpolator Syntax: {{{b"width'bin"}}}
          *   - `bin` is a char sequence of '0', '1', and '?' (to indicate a bit bubble).
          *   - `bin` also allows separators of space ' ' or underscore '_' that are ignored.
          *   - `width` (with the following tag char `'`) is optional. If it's not specified, the
          *     width is determined by the length of the char sequence. Otherwise, the width is set
          *     as required. If the required width is longer than the char sequence length, then
          *     zeros are added as the MSBits. If the required width is shorter then the char
          *     sequence, it is accepted only if the MSBits it is truncating are zeros. Otherwise, a
          *     compilation error is generated.
          * @example
          *   {{{
          *   b"1"        //value = 1
          *   b"1000"     //value = 1000
          *   b"8'1000"   //value = 00001000
          *   b"3'0100"   //value = 100
          *   b"3'1100"   //error
          *   b"1?11"     //value = 1?11 (? is a bubble bit)
          *   b"11_00"    //value = 1100
          *   }}}
          * @note
          *   The string interpolator currently does not accept external arguments with `\${arg}`
          * @return
          *   Bits vector token.
          */
        transparent inline def b: Any = ${ SIParts.scMacro[BParts]('sc) }

        /** Hexadecimal Bits Vector Token String Interpolator
          *
          * Interpolator Syntax: {{{b"width'hex"}}}
          *   - `hex` is a char sequence of '0'-'9','A'-'F','a'-'f','?' (to indicate a 4-bit
          *     bubble). Each character is equivalent to a 4-bits nibble.
          *   - `hex` also allows separators of space ' ' or underscore '_' that are ignored.
          *   - `hex` also supports a binary mode within `{bin}`, where bin is equivalent to the
          *     char sequence of the binary string interpolator (see [[b]]). So between 4-bit hex
          *     nibbles, it is possible to insert a binary bit sequence of any length that is not
          *     necessarily dividable by 4.
          *   - `width` (with the following tag char `'`) is optional. If it's not specified, the
          *     width is determined by the length of the char sequence. Otherwise, the width is set
          *     as required. If the required width is longer than the char sequence length, then
          *     zeros are added as the MSBits. If the required width is shorter then the char
          *     sequence, it is accepted only if the MSBits it is truncating are zeros. Otherwise, a
          *     compilation error is generated.
          * @example
          *   {{{
          *   h"1"        //value = 0001
          *   h"27"       //value = 00100111
          *   h"6'27"     //value = 100111
          *   h"5'27"     //error
          *   h"2?"       //value = 0010????
          *   h"F{00}F"   //value = 1111001111
          *   h"3_3"      //value = 00110011
          *   }}}
          * @note
          *   The string interpolator currently does not accept external arguments with `\${arg}`
          * @return
          *   Bits vector token.
          */
        transparent inline def h: Any = ${ SIParts.scMacro[HParts]('sc) }
      end extension

      private def applyMacro[P <: Tuple](opExpr: Expr[String])(
          scParts: Expr[P],
          args: Expr[Seq[Any]]
      )(using Quotes, Type[P]): Expr[DFTokenAny] =
        scParts.scPartsWithArgs(args).interpolate(opExpr)

      extension (using Quotes)(fullTerm: quotes.reflect.Term)
        private def interpolate(
            opExpr: Expr[String]
        ): Expr[DFTokenAny] =
          import quotes.reflect.*
          val opStr = opExpr.value.get
          val widthTpe: TypeRepr = fullTerm match
            case Literal(StringConstant(t)) =>
              val res = opStr match
                case "b" => fromBinString(t)
                case "h" => fromHexString(t)
              res match
                case Right((valueBits, bubbleBits)) =>
                  ConstantType(IntConstant(valueBits.length.toInt))
                case Left(msg) =>
                  report.errorAndAbort(msg)
            case _ => TypeRepr.of[Int]
          val widthType = widthTpe.asType.asInstanceOf[Type[Int]]
          val fullExpr = opStr match
            case "b" => '{ fromBinString(${ fullTerm.asExprOf[String] }) }
            case "h" => '{ fromHexString(${ fullTerm.asExprOf[String] }) }
          '{
            val (valueBits, bubbleBits) = ${ fullExpr }.toOption.get
            val width =
              dfhdl.internals.Inlined
                .forced[widthType.Underlying](valueBits.length.toInt)
            Token[widthType.Underlying](width, valueBits, bubbleBits)
          }
      private def unapplySeqMacro[P <: Tuple, T <: DFTypeAny](
          opForcedExpr: Expr[String]
      )(
          scParts: Expr[P],
          arg: Expr[DFValOf[T]]
      )(using Quotes, Type[P], Type[T]): Expr[Option[Seq[Any]]] =
        import quotes.reflect.*
        val parts = TypeRepr.of[P].getTupleArgs
        val op = opForcedExpr.value.get
        if (TypeRepr.of[P].getTupleArgs.length > 1)
          val vArgs = Varargs(opForcedExpr :: parts.map {
            case ConstantType(StringConstant(part: String)) =>
              val partFiltered = part.filter {
                case '_' | ' ' | '?'        => false
                case isHex() if op == "h"   => true
                case '0' | '1' if op == "b" => true
                case x =>
                  report.errorAndAbort(
                    s"""Found invalid character: ${x}. 
                      |Note: string interpolation with value extraction does not support the `[w']` width extension syntax.""".stripMargin
                  )
              }
              Literal(StringConstant(partFiltered)).asExprOf[String]
          })
          '{
            Some(Seq(${ vArgs }*))
          }
        else
          val token =
            SIParts
              .tupleToExprs(scParts)
              .head
              .asTerm
              .interpolate(opForcedExpr)
          val tokenType = token.asTerm.tpe.asTypeOf[DFTokenAny]
          '{
            val tc = compiletime
              .summonInline[
                DFVal.Compare[T, tokenType.Underlying, FuncOp.===.type, false]
              ]
            Some(
              Seq(
                tc.conv(${ arg }.dfType, $token)
              )
            )
          }
        end if
      end unapplySeqMacro
    end StrInterp
  end Token

  object Val:
    trait Candidate[R]:
      type OutW <: Int
      def apply(value: R)(using DFC): DFBits[OutW] <> VAL
    object Candidate:
      given fromDFBits[W <: Int, R <: DFBits[W] <> VAL]: Candidate[R] with
        type OutW = W
        def apply(value: R)(using DFC): DFBits[W] <> VAL =
          value
      given fromDFUInt[W <: Int, R <: DFUInt[W] <> VAL]: Candidate[R] with
        type OutW = W
        def apply(value: R)(using DFC): DFBits[W] <> VAL = ???
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a bits variable."
        )
      inline given errDFSInt[W <: Int, R <: DFSInt[W] <> VAL]: Candidate[R] =
        compiletime.error(
          "Cannot apply a signed value to a bits variable.\nConsider applying `.bits` conversion to resolve this issue."
        )
      transparent inline given fromDFBitsTokenCandidate[R](using
          ic: Token.Candidate[R]
      ): Candidate[R] = new Candidate[R]:
        type OutW = ic.OutW
        def apply(arg: R)(using DFC): DFBits[OutW] <> VAL =
          DFVal.Const(ic(arg))

      private[Val] def valueToBits(value: Any)(using dfc: DFC): DFBits[Int] <> VAL = ???
      transparent inline given fromTuple[R <: NonEmptyTuple]: Candidate[R] = ${
        DFBitsMacro[R]
      }
      def DFBitsMacro[R](using
          Quotes,
          Type[R]
      ): Expr[Candidate[R]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[R]
        val wType = rTpe.calcValWidth(false).asTypeOf[Int]
        '{
          new Candidate[R]:
            type OutW = wType.Underlying
            def apply(value: R)(using DFC): DFValOf[DFBits[OutW]] =
              valueToBits(value).asIR.asValOf[DFBits[OutW]]
        }
      end DFBitsMacro
    end Candidate

    object TC:
      import DFVal.TC
      def apply(
          dfType: DFBits[Int],
          dfVal: DFBits[Int] <> VAL
      ): DFBits[Int] <> VAL =
        `LW == RW`(dfType.width, dfVal.width)
        dfVal
      protected object `LW == RW`
          extends Check2[
            Int,
            Int,
            [LW <: Int, RW <: Int] =>> LW == RW,
            [LW <: Int, RW <: Int] =>> "The argument width (" + ToString[RW] +
              ") is different than the receiver width (" + ToString[LW] +
              ").\nConsider applying `.resize` to resolve this issue."
          ]
      given DFBitsFromCandidate[
          LW <: Int,
          V
      ](using dfc: DFC, candidate: Candidate[V])(using
          check: `LW == RW`.Check[LW, candidate.OutW]
      ): TC[DFBits[LW], V] with
        def conv(dfType: DFBits[LW], value: V): DFValOf[DFBits[LW]] =
          val dfVal = candidate(value)
          check(dfType.width, dfVal.width.value)
          dfVal.asIR.asValOf[DFBits[LW]]
      given DFBitsFromSEV[LW <: Int, T <: BitOrBool, V <: SameElementsVector[T]](using
          dfc: DFC
      ): TC[DFBits[LW], V] with
        def conv(dfType: DFBits[LW], value: V): DFValOf[DFBits[LW]] =
          DFVal.Const(Token(dfType.width, value))
    end TC
  end Val
end CompanionsDFBits
