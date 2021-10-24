package DFiant.core
import DFiant.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import DFiant.internals.*

import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*

type DFBits[W <: Int] = OpaqueDFBits.DFBits[W]
val DFBits = OpaqueDFBits.DFBits
import CompanionsDFDecimal.Constraints.`LW == RW`

private object OpaqueDFBits:
  opaque type DFBits[W <: Int] <: DFType[ir.DFBits] = DFType[ir.DFBits]
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

    type Token[W <: Int] = CompanionsDFBits.Token[W]
    val Token = CompanionsDFBits.Token
    val Val = CompanionsDFBits.Val
  end DFBits
end OpaqueDFBits

private object CompanionsDFBits:
  protected object `AW == TW`
      extends Check2[
        Int,
        Int,
        [AW <: Int, TW <: Int] =>> AW == TW,
        [AW <: Int, TW <: Int] =>> "The alias width (" + AW +
          ") is different than the dataflow value width (" + TW + ")."
      ]
  protected object BitIndex
      extends Check2[
        Int,
        Int,
        [I <: Int, W <: Int] =>> (I < W) && (I >= 0),
        [I <: Int, W <: Int] =>> "Index " + I +
          " is out of range of width/length " + W
      ]
  protected object BitsHiLo
      extends Check2[
        Int,
        Int,
        [H <: Int, L <: Int] =>> H >= L,
        [H <: Int, L <: Int] =>> "Low index " + L +
          " is bigger than High bit index " + H
      ]
  trait CompareCheck[
      ValW <: Int,
      ArgW <: Int,
      Castle <: Boolean //castling of dfVal and arg
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
      ir.DFToken(dfType.asIR, data).asTokenOf[DFBits[W]]
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
    protected[core] def apply[W <: Int](
        width: Inlined[W],
        value: SameBitsVector
    ): Token[W] =
      val level = value match
        case SameBitsVector.b0s => false
        case SameBitsVector.b1s => true
      Token(
        width,
        BitVector.fill(width.value)(level),
        BitVector.low(width.value)
      )
    extension [W <: Int](token: DFBits.Token[W])
//      def width: Inlined[W] = Inlined.forced[W](token.asIR.width)
      def data: (BitVector, BitVector) =
        token.asIR.data.asInstanceOf[(BitVector, BitVector)]
      def valueBits: BitVector = token.data._1
      def bubbleBits: BitVector = token.data._2

    object Conversions:
      given DFBitsTokenConversionSing[W <: Int & Singleton, V](using
          tc: DFToken.TC[DFBits[W], V],
          w: ValueOf[W]
      ): Conversion[V, DFBits[W] <> TOKEN] = value =>
        tc(DFBits(valueOf[W]), value)
    end Conversions

    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBits token."
    )
    trait Candidate[-R]:
      type OutW <: Int
      def apply(arg: R): Token[OutW]
    protected trait CandidateLP:
      protected inline val intErrMsg =
        "An integer value cannot be a candidate for a DFBits type.\nTry explicitly using a decimal token via the `d\"<width>'<number>\"` string interpolation."
      transparent inline given errorOnSingInt[R <: Int]: Candidate[ValueOf[R]] =
        compiletime.error(intErrMsg)
      transparent inline given errorOnInt: Candidate[Int] =
        compiletime.error(intErrMsg)
    object Candidate extends CandidateLP:
      type Aux[-R, W <: Int] = Candidate[R] { type OutW = W }
      transparent inline given fromDFBitsToken[W <: Int]: Candidate[Token[W]] =
        new Candidate[Token[W]]:
          type OutW = W
          def apply(arg: Token[W]): Token[OutW] = arg
      transparent inline given fromDFUIntToken[W <: Int]
          : Candidate[DFUInt.Token[W]] = new Candidate[DFUInt.Token[W]]:
        type OutW = W
        def apply(arg: DFUInt.Token[W]): Token[OutW] =
          import DFToken.Ops.bits
          arg.bits
      transparent inline given fromDFBitCandidate[R, T <: DFBoolOrBit](using
          ic: DFBoolOrBit.Token.Candidate.Aux[R, T]
      )(using T =:= DFBit): Candidate[R] = new Candidate[R]:
        type OutW = 1
        def apply(arg: R): Token[1] =
          import DFToken.Ops.bits
          ic(arg).bits
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

      //TODO: minimize error when removing aux pattern
      given DFBitsTokenFromCandidate[W <: Int, R, VW <: Int](using
          ic: Candidate.Aux[R, VW]
      )(using check: `W == VW`.Check[W, VW]): TC[DFBits[W], R] with
        def apply(dfType: DFBits[W], value: R): Out =
          val tokenArg = ic(value)
          check(dfType.width, tokenArg.asIR.width)
          tokenArg.asInstanceOf[Out]

      given DFBitsTokenFromSBV[W <: Int]: TC[DFBits[W], SameBitsVector] with
        def apply(dfType: DFBits[W], value: SameBitsVector): Out =
          DFBits.Token[W](dfType.width, value)
    end TC

    private val widthExp = "([0-9]+)'(.*)".r
    def fromBinString(
        bin: String
    ): Either[String, (BitVector, BitVector)] =
      val (explicitWidth, word) = bin match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, bin)
      val (valueBits, bubbleBits) =
        word.foldLeft((BitVector.empty, BitVector.empty)) {
          case (t, '_') => t //ignoring underscore
          case ((v, b), c) =>
            c match //bin mode
              case '?' => (v :+ false, b :+ true)
              case '0' => (v :+ false, b :+ false)
              case '1' => (v :+ true, b :+ false)
              case x   => return Left(s"Found invalid binary character: $x")
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
    end fromBinString
    def fromHexString(
        hex: String
    ): Either[String, (BitVector, BitVector)] =
      val isHex = "[0-9a-fA-F]".r
      val (explicitWidth, word) = hex match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, hex)
      val (valueBits, bubbleBits, binMode) =
        word.foldLeft((BitVector.empty, BitVector.empty, false)) {
          case (t, '_' | ' ') => t //ignoring underscore or space
          case ((v, b, false), c) =>
            c match //hex mode
              case '{' => (v, b, true)
              case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
              case isHex() =>
                (
                  v ++ BitVector.fromHex(c.toString).get,
                  b ++ BitVector.low(4),
                  false
                )
              case x => return Left(s"Found invalid hex character: $x")
          case ((v, b, true), c) =>
            c match //bin mode
              case '}' => (v, b, false)
              case '?' => (v :+ false, b :+ true, true)
              case '0' => (v :+ false, b :+ false, true)
              case '1' => (v :+ true, b :+ false, true)
              case x =>
                return Left(
                  s"Found invalid binary character in binary mode: $x"
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
    end fromHexString

    object StrInterp:
      extension (inline sc: StringContext)
        transparent inline def b(inline args: Any*): DFTokenAny =
          ${
            interpMacro('{ "b" })('sc, 'args)
          }
        transparent inline def h(inline args: Any*): DFTokenAny =
          ${
            interpMacro('{ "h" })('sc, 'args)
          }

      private def interpMacro(op: Expr[String])(
          sc: Expr[StringContext],
          args: Expr[Seq[Any]]
      )(using Quotes): Expr[DFTokenAny] =
        import quotes.reflect.*
        val fullTerm = sc.termWithArgs(args)
        val opStr = op.value.get
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
            DFiant.internals.Inlined
              .forced[widthType.Underlying](valueBits.length.toInt)
          Token[widthType.Underlying](width, valueBits, bubbleBits)
        }
      end interpMacro
    end StrInterp

    object Compare:
      import DFToken.Compare
      given [LW <: Int, R, Op <: FuncOp, C <: Boolean](using
          ic: Candidate[R]
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], R, Op, C] with
        def apply(token: Token[LW], arg: R): DFBool <> TOKEN =
          val tokenArg = ic(arg)
          check(
            token.dfType.width,
            tokenArg.dfType.width
          )
          val outData =
            if (token.data._2.isZeros && tokenArg.data._2.isZeros)
              op.value match
                case FuncOp.=== => Some(token.data._1 === tokenArg.data._1)
                case FuncOp.=!= => Some(!(token.data._1 === tokenArg.data._1))
                case _ => throw new IllegalArgumentException("Unsupported Op")
            else None
          DFBoolOrBit.Token(DFBool, outData)
        end apply
      end given
      given [LW <: Int, Op <: FuncOp, C <: Boolean](using
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], SameBitsVector, Op, C] with
        def apply(token: Token[LW], arg: SameBitsVector): DFBool <> TOKEN =
          token == Token[LW](token.width, arg)
    end Compare

    object Ops:
      extension [LW <: Int](lhs: DFBits.Token[LW])
        def as[A](
            aliasType: A
        )(using
            tc: DFType.TC[A],
            aW: Width[A]
        )(using check: `AW == TW`.Check[aW.Out, LW]): DFToken[tc.Type] =
          val dfType = tc(aliasType).asIR
          check(dfType.width, lhs.width)
          lhs.asIR.asInstanceOf[ir.DFBits.Token].as(dfType).asTokenOf[tc.Type]
        def uint: DFUInt.Token[LW] = as(DFUInt(lhs.width))
        def sint: DFSInt.Token[LW] = as(DFSInt(lhs.width))
        def apply[I <: Int](
            relIdx: Inlined[I]
        )(using check: BitIndex.Check[I, LW]): DFBoolOrBit.Token =
          check(relIdx, lhs.width)
          val value = lhs.valueBits.bit(relIdx.toLong)
          val bubble = lhs.bubbleBits.bit(relIdx.toLong)
          val tokenData = if (bubble) None else Some(value)
          DFBoolOrBit.Token(DFBit, tokenData)
        def msbit: DFBoolOrBit.Token = apply(lhs.width - 1)
        def lsbit: DFBoolOrBit.Token = apply(0)

        def apply[H <: Int, L <: Int](
            relBitHigh: Inlined[H],
            relBitLow: Inlined[L]
        )(using
            checkHigh: BitIndex.Check[H, LW],
            checkLow: BitIndex.Check[L, LW],
            checkHiLo: BitsHiLo.Check[H, L]
        ): DFBits.Token[H - L + 1] =
          checkHigh(relBitHigh, lhs.width)
          checkLow(relBitLow, lhs.width)
          checkHiLo(relBitHigh, relBitLow)
          val valueBits =
            lhs.valueBits.bits(relBitHigh.toLong, relBitLow.toLong)
          val bubbleBits =
            lhs.bubbleBits.bits(relBitHigh.toLong, relBitLow.toLong)
          val width = relBitHigh - relBitLow + 1
          DFBits.Token(width, valueBits, bubbleBits)
        end apply

        @targetName("bitsResize")
        def resize[RW <: Int](updatedWidth: Inlined[RW])(using
            check: Arg.Width.Check[RW]
        ): Token[RW] =
          if (updatedWidth == lhs.width) lhs.asIR.asTokenOf[DFBits[RW]]
          else
            check(updatedWidth)
            val data = lhs.data
            import DFiant.internals.{resize => resizeBV}
            Token(
              updatedWidth,
              data._1.resizeBV(updatedWidth),
              data._2.resizeBV(updatedWidth)
            )

        @targetName("concat")
        def ++[RW <: Int](rhs: DFBits.Token[RW]): DFBits.Token[LW + RW] =
          val width = lhs.width + rhs.width
          val valueBits = lhs.valueBits ++ rhs.valueBits
          val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
          Token(width, valueBits, bubbleBits)

        @targetName("bitwiseAnd")
        def &[RW <: Int](rhs: DFBits.Token[RW])(using
            bb: Bubble.Behaviour
        ): DFBits.Token[LW] =
          assert(lhs.width == rhs.width)
          val width = lhs.width
          bb match
            case Bubble.Behaviour.Stall =>
              Token(
                width,
                lhs.valueBits & rhs.valueBits,
                lhs.bubbleBits | rhs.bubbleBits
              )
            case Bubble.Behaviour.DontCare =>
              val valueBits =
                (lhs.valueBits | lhs.bubbleBits) & (rhs.valueBits | rhs.bubbleBits)
              val bubbleBits =
                (lhs.bubbleBits & rhs.bubbleBits) | (lhs.bubbleBits & rhs.valueBits) |
                  (rhs.bubbleBits & lhs.valueBits)
              Token(width, valueBits, bubbleBits)
        end &
      end extension
    end Ops
  end Token

  object Val:
    trait Candidate[-R]:
      type OutW <: Int
      def apply(value: R)(using DFC): DFBits[OutW] <> VAL
    object Candidate:
      given fromDFBits[W <: Int]: Candidate[DFBits[W] <> VAL] with
        type OutW = W
        def apply(value: DFBits[W] <> VAL)(using DFC): DFBits[W] <> VAL =
          value
      given fromDFUInt[W <: Int]: Candidate[DFUInt[W] <> VAL] with
        type OutW = W
        def apply(value: DFUInt[W] <> VAL)(using DFC): DFBits[W] <> VAL =
          import DFVal.Ops.bits
          value.bits
      transparent inline given fromDFBitsTokenCandidate[R](using
          ic: Token.Candidate[R]
      ): Candidate[R] = new Candidate[R]:
        type OutW = ic.OutW
        def apply(arg: R)(using DFC): DFBits[OutW] <> VAL =
          DFVal.Const(ic(arg))

      private def valueToBits(value: Any)(using dfc: DFC): DFBits[Int] <> VAL =
        import DFBits.Val.Ops.concatBits
        given dfcAnon: DFC = dfc.anonymize
        value match
          case v: ValueOf[?] =>
            valueToBits(v.value)
          case x: NonEmptyTuple =>
            x.toList.map(valueToBits).concatBits
          case i: Int =>
            DFVal.Const(Token(1, BitVector.bit(i > 0), BitVector.zero))
//          case bool: Boolean =>
//            DFVal.Const(Token(1, BitVector.bit(bool), BitVector.zero))
          case token: DFToken[_] =>
            val tokenIR = token.asIR
            val tokenOut = tokenIR.dfType match
              case _: ir.DFBits => tokenIR.asTokenOf[DFBits[Int]]
              case _            => tokenIR.bits.asTokenOf[DFBits[Int]]
            DFVal.Const(tokenOut)
          case dfVal: DFVal[_, _] =>
            import DFVal.Ops.bits
            val dfValIR = dfVal.asIR
            dfValIR.dfType match
              case _: ir.DFBits => dfValIR.asValOf[DFBits[Int]]
              case _            => dfValIR.asValAny.bits
        end match
      end valueToBits
      transparent inline given fromTuple[R <: NonEmptyTuple]
          : Candidate[ValueOf[R]] = ${ DFBitsMacro[ValueOf[R]] }
      def DFBitsMacro[R](using
          Quotes,
          Type[R]
      ): Expr[Candidate[R]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[R]
        extension (tpe: TypeRepr)
          def calcValWidth: TypeRepr =
            tpe.dealias match
              case applied: AppliedType
                  if applied <:< TypeRepr.of[ValueOf[_]] =>
                applied.args.head.calcValWidth
              case AppliedType(tycon, tpe :: _)
                  if tycon <:< TypeRepr.of[DFVal] =>
                tpe.dealias.calcWidth
              case AppliedType(tycon, tpe :: _)
                  if tycon <:< TypeRepr.of[DFToken] =>
                tpe.calcWidth
              case AppliedType(tycon, args)
                  if tycon <:< TypeRepr.of[NonEmptyTuple] =>
                val widths = args.map(a => a.calcValWidth)
                widths.reduce(_ + _)
              case ConstantType(IntConstant(v)) if (v == 1 || v == 0) =>
                ConstantType(IntConstant(1))
//              case ConstantType(BooleanConstant(v)) =>
//                ConstantType(IntConstant(1))
              case ref: TermRef =>
                ref.widen.calcValWidth
              case x =>
                report.errorAndAbort(
                  s"Unsupported argument value ${x.show} for dataflow receiver type DFBits"
                )
            end match
        val wType = rTpe.calcValWidth.asTypeOf[Int]
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
            [LW <: Int, RW <: Int] =>> "The argument width (" +
              ToString[RW] +
              ") is different than the reciever width (" +
              ToString[LW] +
              ").\nConsider applying `.resize` to resolve this issue."
          ]
      given DFBitsFromCandidate[
          LW <: Int,
          R
      ](using dfc: DFC, candidate: Candidate[R])(using
          check: `LW == RW`.Check[LW, candidate.OutW]
      ): TC[DFBits[LW], R] with
        def apply(dfType: DFBits[LW], value: R): DFValOf[DFBits[LW]] =
          val dfVal = candidate(value)
          check(dfType.width, dfVal.width.value)
          dfVal.asIR.asValOf[DFBits[LW]]
      given DFBitsFromSBV[LW <: Int](using
          dfc: DFC
      ): TC[DFBits[LW], SameBitsVector] with
        def apply(
            dfType: DFBits[LW],
            value: SameBitsVector
        ): DFValOf[DFBits[LW]] =
          DFVal.Const(Token(dfType.width, value))
    end TC

    object Compare:
      import DFVal.Compare
      given DFBitsCompareCandidate[LW <: Int, R, Op <: FuncOp, C <: Boolean](
          using ic: Candidate[R]
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], R, Op, C] with
        def apply(dfVal: DFBits[LW] <> VAL, arg: R)(using
            dfc: DFC
        ): DFBool <> VAL =
          val dfValArg = ic(arg)(using dfc.anonymize)
          check(dfVal.dfType.width, dfValArg.dfType.width)
          func(dfVal, dfValArg)
      given DFBitsCompareSBV[LW <: Int, Op <: FuncOp, C <: Boolean](using
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], SameBitsVector, Op, C] with
        def apply(dfVal: DFBits[LW] <> VAL, arg: SameBitsVector)(using
            dfc: DFC
        ): DFBool <> VAL =
          func(dfVal, DFVal.Const(Token(dfVal.width, arg)))
    end Compare

    object Conversions:
      given DFBitsValConversionSing[LW <: Int & Singleton, R](using
          v: ValueOf[LW],
          tc: CompanionsDFVal.TC[DFBits[LW], R],
          dfc: DFC
      ): Conversion[R, DFValOf[DFBits[LW]]] = from =>
        tc(DFBits(valueOf[LW]), from)
      given DFBitsValConversion[R](using
          candidate: Candidate[R],
          dfc: DFC
      ): Conversion[R, DFValOf[DFBits[Int]]] = from =>
        candidate(from).asIR.asValOf[DFBits[Int]]

    object Ops:
      extension [T <: Int](iter: Iterable[DFBits[T] <> VAL])
        protected[core] def concatBits(using DFC): DFBits[Int] <> VAL =
          val width = Inlined.forced[Int](iter.map(_.width.value).sum)
          DFVal.Func(DFBits(width), FuncOp.++, iter.toList)
      extension [W <: Int, M <: ir.DFVal.Modifier](
          lhs: DFVal[DFBits[W], M]
      )
        def as[A](
            aliasType: A
        )(using
            tc: DFType.TC[A],
            aW: Width[A],
            dfc: DFC
        )(using check: `AW == TW`.Check[aW.Out, W]): DFValOf[tc.Type] =
          import Token.Ops.{as => asToken}
          val aliasDFType = tc(aliasType)
          check.apply(aliasDFType.asIR.width, lhs.width)
          DFVal.Alias.AsIs(aliasDFType, lhs, _.asToken(aliasType))
        def uint(using DFC): DFValOf[DFUInt[W]] =
          as(DFUInt(lhs.width))
        def sint(using DFC): DFValOf[DFSInt[W]] =
          as(DFSInt(lhs.width))

        def apply[I <: Int](
            relIdx: Inlined[I]
        )(using
            check: BitIndex.Check[I, W],
            dfc: DFC
        ): DFVal[DFBit, M] =
          check(relIdx, lhs.width)
          DFVal.Alias.ApplyIdx(lhs, relIdx)
        def apply[H <: Int, L <: Int](
            relBitHigh: Inlined[H],
            relBitLow: Inlined[L]
        )(using
            checkHigh: BitIndex.Check[H, W],
            checkLow: BitIndex.Check[L, W],
            checkHiLo: BitsHiLo.Check[H, L],
            dfc: DFC
        ): DFVal[DFBits[H - L + 1], M] =
          checkHigh(relBitHigh, lhs.width)
          checkLow(relBitLow, lhs.width)
          checkHiLo(relBitHigh, relBitLow)
          DFVal.Alias.ApplyRange(lhs, relBitHigh, relBitLow)
        def repeat[N <: Int](num: Inlined[N])(using
            check: Arg.Positive.Check[N],
            dfc: DFC
        ): DFValOf[DFBits[W * N]] =
          check(num)
          DFVal.Func(
            DFBits(lhs.dfType.width * num),
            FuncOp.++,
            List.fill(num)(lhs)
          )
        def resize[RW <: Int](updatedWidth: Inlined[RW])(using
            Arg.Width.Check[RW],
            DFC
        ): DFValOf[DFBits[RW]] =
          import Token.Ops.{resize => resizeToken}
          DFVal.Alias.AsIs(
            DFBits(updatedWidth),
            lhs,
            _.resizeToken(updatedWidth)
          )
      end extension
    end Ops
  end Val
end CompanionsDFBits
