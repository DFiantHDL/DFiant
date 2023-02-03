package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}

import scala.quoted.*
import scala.annotation.targetName
import DFDecimal.Constraints.*

type DFDecimal[S <: Boolean, W <: Int, F <: Int] =
  DFType[ir.DFDecimal, Args3[S, W, F]]
object DFDecimal:
  protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
      signed: Inlined[S],
      width: Inlined[W],
      fractionWidth: Inlined[F]
  )(using check: Width.Check[S, W]): DFDecimal[S, W, F] =
    check(signed, width)
    ir.DFDecimal(signed, width, fractionWidth).asFE[DFDecimal[S, W, F]]

  given [S <: Boolean, W <: Int, F <: Int](using
      ValueOf[S],
      ValueOf[W],
      ValueOf[F]
  )(using Width.Check[S, W]): DFDecimal[S, W, F] =
    DFDecimal(valueOf[S], valueOf[W], valueOf[F])
  object Extensions:
    extension [S <: Boolean, W <: Int, F <: Int](dfType: DFDecimal[S, W, F])
      def signed: Inlined[S] = Inlined.forced[S](dfType.asIR.signed)

  protected[core] object Constraints:
    object Width
        extends Check2[
          Boolean,
          Int,
          [s <: Boolean, w <: Int] =>> ITE[s, w > 1, w > 0],
          [s <: Boolean, w <: Int] =>> ITE[
            s,
            "Signed value width must be larger than 1, but found: " + w,
            "Unsigned value width must be positive, but found: " + w
          ]
        ]
    object Sign
        extends Check2[
          Boolean,
          Int,
          [s <: Boolean, n <: Int] =>> ITE[s, true, n >= 0],
          [s <: Boolean, n <: Int] =>> "Unsigned value must be natural, but found: " + n
        ]

    object `LW >= RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW >= RW,
          [LW <: Int, RW <: Int] =>> "The applied RHS value width (" + RW +
            ") is larger than the LHS variable width (" + LW + ")."
        ]
    object `LW == RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW == RW,
          [LW <: Int, RW <: Int] =>> "Cannot apply this operation between a value of " + LW +
            " bits width (LHS) to a value of " + RW +
            " bits width (RHS).\nAn explicit conversion must be applied."
        ]
    object `ValW >= ArgW`
        extends Check2[
          Int,
          Int,
          [ValW <: Int, ArgW <: Int] =>> ValW >= ArgW,
          [ValW <: Int, ArgW <: Int] =>> "Cannot compare a dataflow value (width = " + ValW +
            ") with a Scala `Int` argument that is wider (width = " + ArgW +
            ").\nAn explicit conversion must be applied."
        ]
    object `LS >= RS`
        extends Check2[
          Boolean,
          Boolean,
          [LS <: Boolean, RS <: Boolean] =>> LS || ![RS],
          [LS <: Boolean, RS <: Boolean] =>> "Cannot apply a signed value to an unsigned variable."
        ]
    type SignStr[S <: Boolean] = ITE[S, "a signed", "an unsigned"]
    object `LS == RS`
        extends Check2[
          Boolean,
          Boolean,
          [LS <: Boolean, RS <: Boolean] =>> LS == RS,
          [LS <: Boolean, RS <: Boolean] =>> "Cannot apply this operation between " +
            ITE[LS, "a signed", "an unsigned"] + " value (LHS) and " +
            ITE[RS, "a signed", "an unsigned"] +
            " value (RHS).\nAn explicit conversion must be applied."
        ]
    trait TCCheck[LS <: Boolean, LW <: Int, RS <: Boolean, RW <: Int]:
      def apply(
          leftSigned: Boolean,
          leftWidth: Int,
          rightSigned: Boolean,
          rightWidth: Int
      ): Unit
    given [LS <: Boolean, LW <: Int, RS <: Boolean, RW <: Int](using
        checkS: `LS >= RS`.Check[LS, RS],
        checkW: `LW >= RW`.Check[LW, ITE[LS != RS, RW + 1, RW]]
    ): TCCheck[LS, LW, RS, RW] with
      def apply(
          leftSigned: Boolean,
          leftWidth: Int,
          rightSigned: Boolean,
          rightWidth: Int
      ): Unit =
        checkS(leftSigned, rightSigned)
        checkW(
          leftWidth,
          if (leftSigned != rightSigned) rightWidth + 1 else rightWidth
        )
    end given
    trait CompareCheck[
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean, // argument is from a Scala Int
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValSigned: Boolean,
          dfValWidth: Int,
          argSigned: Boolean,
          argWidth: Int
      ): Unit
    given [
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        argWFix: Id[ITE[ArgIsInt && ValS && ![ArgS], ArgW + 1, ArgW]],
        skipChecks: Id[ArgIsInt && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipChecks.Out, ValS, ArgS]],
        lw: Id[ITE[Castle, argWFix.Out, ValW]],
        rw: Id[ITE[Castle ^ skipChecks.Out, ValW, argWFix.Out]]
    )(using
        checkS: `LS == RS`.Check[ls.Out, rs.Out],
        checkW: `LW == RW`.Check[lw.Out, rw.Out],
        checkVAW: `ValW >= ArgW`.Check[ValW, ITE[ArgIsInt, argWFix.Out, 0]],
        argIsInt: ValueOf[ArgIsInt],
        castle: ValueOf[Castle]
    ): CompareCheck[ValS, ValW, ArgS, ArgW, ArgIsInt, Castle] with
      def apply(
          dfValSigned: Boolean,
          dfValWidth: Int,
          argSigned: Boolean,
          argWidth: Int
      ): Unit =
        val skipChecks = argIsInt.value && (dfValSigned || !argSigned)
        val argWFix =
          if (argIsInt.value && dfValSigned && !argSigned) argWidth + 1
          else argWidth
        if (argIsInt) checkVAW(dfValWidth, argWFix)
        if (!skipChecks)
          val ls = if (castle) argSigned else dfValSigned
          val rs = if (castle) dfValSigned else argSigned
          checkS(ls, rs)
          val lw = if (castle) argWFix else dfValWidth
          val rw = if (castle) dfValWidth else argWFix
          checkW(lw, rw)
      end apply
    end given
    trait ArithCheck[
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean, // argument is from a Scala Int
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValDFType: DFXInt[ValS, ValW],
          argDFType: DFXInt[ArgS, ArgW]
      ): Unit
    given [
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        argWFix: Id[
          ITE[ArgIsInt && ![Castle] && ValS && ![ArgS], ArgW + 1, ArgW]
        ],
        skipSignChecks: Id[ArgIsInt && ![Castle] && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipSignChecks.Out, ValS, ArgS]],
        lw: Id[ITE[Castle, argWFix.Out, ValW]],
        rw: Id[ITE[Castle, ValW, argWFix.Out]]
    )(using
        checkS: `LS == RS`.Check[ls.Out, rs.Out],
        checkW: `LW >= RW`.Check[lw.Out, rw.Out],
        argIsInt: ValueOf[ArgIsInt],
        castle: ValueOf[Castle]
    ): ArithCheck[ValS, ValW, ArgS, ArgW, ArgIsInt, Castle] with
      def apply(
          dfValDFType: DFXInt[ValS, ValW],
          argDFType: DFXInt[ArgS, ArgW]
      ): Unit =
        val skipSignChecks: Boolean =
          argIsInt.value && !castle && (dfValDFType.signed || !argDFType.signed)
        val argWFix: Int =
          if (argIsInt.value && !castle && dfValDFType.signed && !argDFType.signed)
            argDFType.width + 1
          else argDFType.width
        if (!skipSignChecks)
          val ls: Boolean = if (castle) argDFType.signed else dfValDFType.signed
          val rs: Boolean = if (castle) dfValDFType.signed else argDFType.signed
          checkS(ls, rs)
        val lw: Int = if (castle) argWFix else dfValDFType.width
        val rw: Int = if (castle) dfValDFType.width else argWFix
        checkW(lw, rw)
      end apply
    end given
    trait SignCheck[
        ValS <: Boolean,
        ArgS <: Boolean,
        ArgIsInt <: Boolean, // argument is from a Scala Int
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValSigned: Boolean,
          argSigned: Boolean
      ): Unit
    given [
        ValS <: Boolean,
        ArgS <: Boolean,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        skipSignChecks: Id[ArgIsInt && ![Castle] && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipSignChecks.Out, ValS, ArgS]]
    )(using
        checkS: `LS == RS`.Check[ls.Out, rs.Out],
        argIsInt: ValueOf[ArgIsInt],
        castle: ValueOf[Castle]
    ): SignCheck[ValS, ArgS, ArgIsInt, Castle] with
      def apply(
          dfValSigned: Boolean,
          argSigned: Boolean
      ): Unit =
        val skipSignChecks: Boolean =
          argIsInt.value && !castle && (dfValSigned || !argSigned)
        if (!skipSignChecks)
          val ls: Boolean = if (castle) argSigned else dfValSigned
          val rs: Boolean = if (castle) dfValSigned else argSigned
          checkS(ls, rs)
      end apply
    end given
  end Constraints

  type Token[S <: Boolean, W <: Int, F <: Int] = DFToken[DFDecimal[S, W, F]]
  object Token:
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        dfType: DFDecimal[S, W, F],
        data: Option[BigInt]
    ): Token[S, W, F] =
      ir.DFToken(dfType.asIR)(data).asTokenOf[DFDecimal[S, W, F]]
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        fractionWidth: Inlined[F],
        value: BigInt
    ): Token[S, W, F] =
      require(
        value.bitsWidth(signed) <= width,
        s"\nThe init value $value width must be smaller or equal to $width"
      )
      Token(DFDecimal(signed, width, fractionWidth), Some(value))
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        fractionWidth: Inlined[F],
        value: Int
    ): Token[S, W, F] = Token(signed, width, fractionWidth, BigInt(value))

    private val widthIntExp = "(\\d+)'(-?\\d+)".r
    private val widthFixedExp = "(\\d+)\\.(\\d+)'(-?\\d+)\\.?(\\d*)".r
    private val intExp = "(-?\\d+)".r
    private def fromDecString(
        dec: String,
        signedForced: Boolean
    ): Either[String, (Boolean, Int, Int, BigInt)] =
      def fromValidString(numStr: String): (Boolean, Int, Int, BigInt) =
        val value = BigInt(numStr)
        val signed = value < 0 | signedForced
        val actualWidth = value.bitsWidth(signed)
        (signed, actualWidth, 0, value)
      dec.replace(",", "").replace("_", "") match
        case widthFixedExp(
              magnitudeWidthStr,
              fractionWidthStr,
              magnitudeStr,
              fractionStr
            ) =>
          val explicitMagnitudeWidth = magnitudeWidthStr.toInt
          val explicitFractionWidth = fractionWidthStr.toInt
          val magnitude = BigInt(magnitudeStr)
          val fraction =
            if (fractionStr.isEmpty) BigInt(0) else BigInt(fractionStr)
          Left("Fixed-point decimal literals are not yet supported")
        case widthIntExp(widthStr, numStr) =>
          val explicitWidth = widthStr.toInt
          val (signed, width, fractionWidth, value) = fromValidString(numStr)
          if (explicitWidth < width)
            Left(
              s"Explicit given width ($explicitWidth) is smaller than the actual width ($width)"
            )
          else
            Right((signed, explicitWidth, fractionWidth, value))
        case intExp(numStr) => Right(fromValidString(numStr))
        case _ =>
          Left(s"Invalid decimal pattern found: $dec")
      end match
    end fromDecString

    object TC:
      export DFXInt.Token.TC.given

  end Token

  object Val:
    object TC:
      export DFXInt.Val.TC.given
      def apply(
          dfType: DFDecimal[Boolean, Int, Int],
          dfVal: DFDecimal[Boolean, Int, Int] <> VAL
      ): DFDecimal[Boolean, Int, Int] <> VAL =
        `LW >= RW`(dfType.width, dfVal.width)
        `LS >= RS`(dfType.signed, dfVal.dfType.signed)
        dfVal
    end TC
  end Val
end DFDecimal

type DFXInt[S <: Boolean, W <: Int] = DFDecimal[S, W, 0]
object DFXInt:
  def apply[S <: Boolean, W <: Int](signed: Inlined[S], width: Inlined[W])(using
      Width.Check[S, W]
  ): DFXInt[S, W] = DFDecimal(signed, width, 0)

  type Token[S <: Boolean, W <: Int] = DFDecimal.Token[S, W, 0]
  object Token:
    protected[core] def apply[S <: Boolean, W <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        data: Option[BigInt]
    ): Token[S, W] = DFDecimal.Token(DFXInt(signed, width), data)

    trait Candidate[R]:
      type OutS <: Boolean
      type OutW <: Int
      type IsScalaInt <: Boolean
      def apply(arg: R): Token[OutS, OutW]
    object Candidate:
      // change to given...with after
      // https://github.com/lampepfl/dotty/issues/13580 is resolved
      transparent inline given fromInt[R <: Int](using
          info: IntInfo[R]
      ): Candidate[R] = new Candidate[R]:
        type OutS = info.OutS
        type OutW = info.OutW
        type IsScalaInt = true
        def apply(arg: R): Token[OutS, OutW] =
          Token(info.signed(arg), info.width(arg), Some(arg))
      transparent inline given fromDFXIntToken[W <: Int, S <: Boolean, R <: Token[S, W]]
          : Candidate[R] =
        new Candidate[R]:
          type OutS = S
          type OutW = W
          type IsScalaInt = false
          def apply(arg: R): Token[S, W] = arg
      transparent inline given fromDFBitsToken[W <: Int, R <: DFBits.Token[W]]: Candidate[R] =
        new Candidate[R]:
          type OutS = false
          type OutW = W
          type IsScalaInt = false
          def apply(arg: R): Token[false, W] = ???
    end Candidate

    object TC:
      import DFToken.TC
      given [LS <: Boolean, LW <: Int, R](using
          ic: Candidate[R]
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW]
      ): TC[DFXInt[LS, LW], R] with
        def conv(dfType: DFXInt[LS, LW], value: R): Out = ???
      end given
    end TC
  end Token

  object Val:
    trait Candidate[R]:
      type OutS <: Boolean
      type OutW <: Int
      type IsScalaInt <: Boolean
      def apply(arg: R)(using DFC): DFValOf[DFXInt[OutS, OutW]]
    object Candidate:
      transparent inline given fromTokenCandidate[R](using
          ic: Token.Candidate[R]
      ): Candidate[R] = new Candidate[R]:
        type OutS = ic.OutS
        type OutW = ic.OutW
        type IsScalaInt = ic.IsScalaInt
        def apply(arg: R)(using dfc: DFC): DFValOf[DFXInt[OutS, OutW]] =
          given DFC = dfc.anonymize
          DFVal.Const(ic(arg))
      given fromDFXIntVal[S <: Boolean, W <: Int, R <: DFValOf[DFXInt[S, W]]]: Candidate[R] with
        type OutS = S
        type OutW = W
        type IsScalaInt = false
        def apply(arg: R)(using DFC): DFValOf[DFXInt[S, W]] = arg
      given fromDFBitsVal[W <: Int, R <: DFValOf[DFBits[W]]]: Candidate[R] with
        type OutS = false
        type OutW = W
        type IsScalaInt = false
        def apply(arg: R)(using dfc: DFC): DFValOf[DFXInt[false, W]] = ???
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a dataflow decimal variable."
        )
    end Candidate

    object TC:
      import DFVal.TC
      given [LS <: Boolean, LW <: Int, R](using
          ic: Candidate[R],
          dfc: DFC
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW]
      ): TC[DFXInt[LS, LW], R] with
        def conv(dfType: DFXInt[LS, LW], value: R): Out = ???
      end given
    end TC
  end Val
end DFXInt

type DFUInt[W <: Int] = DFXInt[false, W]
object DFUInt:
  def apply[W <: Int](width: Inlined[W])(using
      Width.Check[false, W]
  ): DFUInt[W] = DFXInt(false, width)
  def apply[W <: Int](using dfType: DFUInt[W]): DFUInt[W] = dfType
  def until[V <: Int](sup: Inlined[V])(using
      check: Arg.LargerThan1.Check[V],
      info: IntInfo[V - 1]
  ): DFUInt[info.OutW] =
    check(sup)
    DFXInt(false, info.width(sup - 1))
  def max[V <: Int](max: Inlined[V])(using
      check: Arg.Positive.Check[V],
      info: IntInfo[V]
  ): DFUInt[info.OutW] =
    check(max)
    DFXInt(false, info.width(max))

  protected object Unsigned
      extends Check1[
        Boolean,
        [S <: Boolean] =>> ![S],
        [S <: Boolean] =>> "Argument must be unsigned"
      ]
  protected object `UB > R`
      extends Check2[
        Int,
        Int,
        [UB <: Int, R <: Int] =>> UB > R,
        [UB <: Int, R <: Int] =>> "The argument must be smaller than the upper-bound " + UB +
          " but found: " + R
      ]
  protected object `UBW == RW`
      extends Check2[
        Int,
        Int,
        [UBW <: Int, RW <: Int] =>> UBW == RW,
        [UBW <: Int, RW <: Int] =>> "Expected argument width " + UBW + " but found: " + RW
      ]

  type Token[W <: Int] = DFDecimal.Token[false, W, 0]
  object Token

  object Val:
    trait UBArg[UB <: Int, R]:
      type OutW <: Int
      def apply(ub: Inlined[UB], arg: R): DFValOf[DFUInt[OutW]]
    trait UBArgLP:
      transparent inline given errorDMZ[UB <: Int, R](using
          r: ShowType[R]
      ): UBArg[UB, R] =
        Error.call[
          (
              "Upper-bound argument cannot be constructed from the type `",
              r.Out,
              "`."
          )
        ]
    object UBArg extends UBArgLP:
      transparent inline given fromInt[UB <: Int, R <: Int](using
          dfc: DFC,
          ubInfo: IntInfo[UB - 1]
      )(using
          unsignedCheck: Unsigned.Check[R < 0],
          ubCheck: `UB > R`.Check[UB, R]
      ): UBArg[UB, R] = new UBArg[UB, R]:
        type OutW = ubInfo.OutW
        def apply(ub: Inlined[UB], arg: R): DFValOf[DFUInt[OutW]] =
          unsignedCheck(arg < 0)
          // TODO: https://github.com/lampepfl/dotty/issues/15798
          val fixme = (ub - 1).asInstanceOf[Inlined[Int]].value
          ubCheck(fixme, arg)
          val token =
            DFXInt.Token(false, ubInfo.width(fixme), Some(BigInt(arg)))
          DFVal.Const(token)
      transparent inline given fromR[UB <: Int, R](using
          dfc: DFC,
          c: DFXInt.Val.Candidate[R],
          ubInfo: IntInfo[UB - 1]
      )(using
          unsignedCheck: Unsigned.Check[c.OutS],
          widthCheck: `UBW == RW`.Check[ubInfo.OutW, c.OutW]
      ): UBArg[UB, R] = new UBArg[UB, R]:
        type OutW = ubInfo.OutW
        def apply(ub: Inlined[UB], arg: R): DFValOf[DFUInt[OutW]] =
          given DFC = dfc.anonymize
          val argVal = c(arg)
          unsignedCheck(argVal.dfType.signed)
          // TODO: https://github.com/lampepfl/dotty/issues/15798
          val fixme = (ub - 1).asInstanceOf[Inlined[Int]].value
          widthCheck(ubInfo.width(fixme), argVal.width)
          // for constant value we apply an explicit check for the bound
          argVal.asIR match
            case ir.DFVal.Const(ir.DFDecimal.Token(dfType, data), _, _, _) =>
              data match
                case Some(value) =>
                  summon[`UB > R`.Check[UB, Int]](ub, value.toInt)
                case _ => // no check
            case _ => // no check
          argVal.asIR.asValOf[DFUInt[OutW]]
        end apply
    end UBArg
  end Val

end DFUInt

type DFSInt[W <: Int] = DFXInt[true, W]
object DFSInt:
  def apply[W <: Int](width: Inlined[W])(using
      Width.Check[true, W]
  ): DFSInt[W] = DFXInt(true, width)
  def apply[W <: Int](using dfType: DFSInt[W]): DFSInt[W] = dfType

  type Token[W <: Int] = DFDecimal.Token[true, W, 0]
  object Token
  object Val
end DFSInt
