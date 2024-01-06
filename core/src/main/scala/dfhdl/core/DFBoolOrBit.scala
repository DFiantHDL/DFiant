package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import annotation.{implicitNotFound, targetName}

type BitNum = 0 | 1
type BitOrBool = BitNum | Boolean
type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  type Data = Option[Boolean]
  type Token = DFToken[DFBoolOrBit]
  given DFBool = DFBool
  given DFBit = DFBit
  object Token:
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        data: Option[Boolean]
    ): T <> TOKEN =
      ir.DFToken(dfType.asIR)(data).asTokenOf[T]
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Boolean
    ): T <> TOKEN =
      Token(dfType, Some(value))
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: BitNum
    ): T <> TOKEN =
      Token(dfType, value > 0)
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Bubble
    ): T <> TOKEN =
      Token(dfType, None)

    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit token."
    )
    trait Candidate[R]:
      type OutT <: DFBoolOrBit
      def apply(arg: R): DFToken[OutT]
    object Candidate:
      type Aux[R, T <: DFBoolOrBit] = Candidate[R] { type OutT = T }
      given fromBoolean[R <: Boolean]: Candidate[R] with
        type OutT = DFBool
        def apply(arg: R): DFToken[DFBool] = Token(DFBool, arg)
      given fromBit[R <: BitNum]: Candidate[R] with
        type OutT = DFBit
        def apply(arg: R): DFToken[DFBit] = Token(DFBit, arg)
      given fromDFBoolOrBitToken[T <: DFBoolOrBit, R <: DFToken[T]]: Candidate[R] with
        type OutT = T
        def apply(arg: R): DFToken[T] = arg
    end Candidate

    object TC:
      import DFToken.TC
      given DFBoolTokenFromCandidate[T <: DFBoolOrBit, R](using
          ic: Candidate[R]
      ): TC[T, R] with
        def conv(dfType: T, arg: R)(using Ctx): Out =
          val tokenArg = ic(arg)
          val tokenOut = (dfType, tokenArg.dfType) match
            case (DFBit, DFBool) => Token(DFBit, tokenArg.data)
            case (DFBool, DFBit) => Token(DFBool, tokenArg.data)
            case _               => tokenArg
          tokenOut.asTokenOf[T]
    end TC

    private def logicOp[O <: DFBoolOrBit, T <: DFBoolOrBit](
        dfType: O,
        token: DFToken[T],
        tokenArg: DFToken[DFBoolOrBit],
        op: FuncOp
    ): DFToken[O] =
      val dataOut = (token.data, tokenArg.data) match
        case (Some(l), Some(r)) =>
          op match
            case FuncOp.=== => Some(l == r)
            case FuncOp.=!= => Some(l != r)
            case FuncOp.|   => Some(l || r)
            case FuncOp.&   => Some(l && r)
            case FuncOp.^   => Some(l ^ r)
            case _          => throw new IllegalArgumentException("Unsupported Op")
        case _ => None
      Token(dfType, dataOut)
    end logicOp

    private def logicOp[T <: DFBoolOrBit](
        token: DFToken[T],
        tokenArg: DFToken[DFBoolOrBit],
        op: FuncOp
    ): DFToken[T] = logicOp[T, T](token.dfType, token, tokenArg, op)

    object Compare:
      import DFToken.Compare
      given DFBoolOrBitCompare[T <: DFBoolOrBit, R, Op <: FuncOp, C <: Boolean](using
          ic: Candidate[R],
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[T, R, Op, C] with
        def conv(dfType: T, arg: R)(using Ctx): DFToken[T] =
          val tokenArg = ic(arg)
          Token(dfType, tokenArg.data)
    end Compare

    object Ops:
      extension (lhs: DFBit <> TOKEN)
        def bool: DFBool <> TOKEN = Token(DFBool, lhs.data)
        @targetName("notOfDFBit")
        def unary_! : DFBit <> TOKEN = Token(DFBit, lhs.data.map(!_))
      extension (lhs: DFBool <> TOKEN)
        def bit: DFBit <> TOKEN = Token(DFBit, lhs.data)
        @targetName("notOfDFBool")
        def unary_! : DFBool <> TOKEN = Token(DFBool, lhs.data.map(!_))
      extension [T <: DFBoolOrBit](lhs: T <> TOKEN)
        def ||[R](rhs: Exact[R])(using ic: Candidate[R]): T <> TOKEN =
          logicOp(lhs, ic(rhs), FuncOp.|)
        def &&[R](rhs: Exact[R])(using ic: Candidate[R]): T <> TOKEN =
          logicOp(lhs, ic(rhs), FuncOp.&)
        def ^[R](rhs: Exact[R])(using ic: Candidate[R]): T <> TOKEN =
          logicOp(lhs, ic(rhs), FuncOp.^)
      extension [L](lhs: L)
        def ||[RT <: DFBoolOrBit](
            rhs: DFToken[RT]
        )(using es: Exact.Summon[L, lhs.type])(using
            ic: Candidate[es.Out]
        ): RT <> TOKEN = logicOp(rhs, ic(es(lhs)), FuncOp.|)
        def &&[RT <: DFBoolOrBit](
            rhs: DFToken[RT]
        )(using es: Exact.Summon[L, lhs.type])(using
            ic: Candidate[es.Out]
        ): RT <> TOKEN = logicOp(rhs, ic(es(lhs)), FuncOp.&)
        def ^[RT <: DFBoolOrBit](
            rhs: DFToken[RT]
        )(using es: Exact.Summon[L, lhs.type])(using
            ic: Candidate[es.Out]
        ): RT <> TOKEN = logicOp(rhs, ic(es(lhs)), FuncOp.^)
      end extension
    end Ops
  end Token

  object Val:
    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit DFHDL value."
    )
    trait Candidate[R]:
      type OutT <: DFBoolOrBit
      type OutP
      def apply(arg: R)(using DFC): DFValOf[OutT]
    object Candidate:
      given fromTokenCandidate[R, IC <: Token.Candidate[R]](using ic: IC): Candidate[R] with
        type OutT = ic.OutT
        type OutP = CONST
        def apply(arg: R)(using DFC): DFValOf[OutT] = DFVal.Const(ic(arg))
      given fromDFBoolOrBitVal[T <: DFBoolOrBit, R <: DFValOf[T]]: Candidate[R] with
        type OutT = T
        def apply(arg: R)(using DFC): DFValOf[T] = arg

    private def b2b[T <: DFBoolOrBit, R](dfType: T, arg: R)(using
        ic: Candidate[R],
        dfc: DFC
    ): DFValTP[T, ic.OutP] =
      val dfcAnon = dfc.anonymize
      import Ops.{bit, bool}
      val dfValArg = ic(arg)(using dfcAnon)
      val dfValOut = (dfType, dfValArg.dfType) match
        case (DFBit, DFBool) => dfValArg.asValOf[DFBool].bit(using dfcAnon)
        case (DFBool, DFBit) => dfValArg.asValOf[DFBit].bool(using dfcAnon)
        case _               => dfValArg
      dfValOut.asValTP[T, ic.OutP]

    object TC:
      import DFVal.TC
      given DFBoolOrBitFromCandidate[T <: DFBoolOrBit, R, IC <: Candidate[R]](using
          ic: IC
      ): TC[T, R] with
        type OutP = ic.OutP
        def conv(dfType: T, arg: R)(using Ctx): Out = b2b(dfType, arg)
    end TC

    object Compare:
      import DFVal.Compare
      given DFBoolOrBitCompare[T <: DFBoolOrBit, R, Op <: FuncOp, C <: Boolean](using
          Candidate[R],
          ValueOf[Op],
          ValueOf[C]
      ): Compare[T, R, Op, C] with
        def conv(dfType: T, arg: R)(using Ctx): DFValOf[T] =
          b2b(dfType, arg)

    object Ops:
      extension [P](lhs: DFValTP[DFBit, P])
        def rising(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.rising, List(lhs))
        }
        def falling(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.falling, List(lhs))
        }
        def bool(using DFC): DFValTP[DFBool, P] = trydf {
          import Token.Ops.{bool => boolToken}
          DFVal.Alias.AsIs(DFBool, lhs, _.boolToken)
        }
        @targetName("notOfDFBit")
        def unary_!(using DFC): DFValTP[DFBit, P] = trydf {
          DFVal.Func(DFBit, FuncOp.unary_!, List(lhs))
        }
      end extension
      extension [P](lhs: DFValTP[DFBool, P])
        def bit(using DFC): DFValTP[DFBit, P] = trydf {
          import Token.Ops.{bit => bitToken}
          DFVal.Alias.AsIs(DFBit, lhs, _.bitToken)
        }
        @targetName("notOfDFBool")
        def unary_!(using DFC): DFValTP[DFBool, P] = trydf {
          DFVal.Func(DFBool, FuncOp.unary_!, List(lhs))
        }

      private def logicOp[T <: DFBoolOrBit, P, R](
          dfVal: DFValTP[T, P],
          arg: R,
          op: FuncOp,
          castle: Boolean
      )(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
        val dfValArg = b2b(dfVal.dfType, arg)
        val (lhs, rhs) = if (castle) (dfValArg, dfVal) else (dfVal, dfValArg)
        DFVal.Func(lhs.dfType.asFE[T], op, List(lhs, rhs))
      extension [T <: DFBoolOrBit, P](lhs: DFValTP[T, P])
        def ||[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
          trydf { logicOp[T, P, R](lhs, rhs, FuncOp.|, false) }
        def &&[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
          trydf { logicOp[T, P, R](lhs, rhs, FuncOp.&, false) }
        def ^[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
          trydf { logicOp[T, P, R](lhs, rhs, FuncOp.^, false) }
      extension [L](lhs: L)
        def ||[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): DFValTP[RT, RP | ic.OutP] = trydf {
          logicOp(rhs, ic(es(lhs)), FuncOp.|, true).asValTP[RT, RP | ic.OutP]
        }
        def &&[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): DFValTP[RT, RP | ic.OutP] = trydf {
          logicOp(rhs, ic(es(lhs)), FuncOp.&, true).asValTP[RT, RP | ic.OutP]
        }
        def ^[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): DFValTP[RT, RP | ic.OutP] = trydf {
          logicOp(rhs, ic(es(lhs)), FuncOp.^, true).asValTP[RT, RP | ic.OutP]
        }
      end extension
    end Ops
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type, NoArgs]
final lazy val DFBit = ir.DFBit.asFE[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived
