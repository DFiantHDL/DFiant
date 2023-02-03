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
      transparent inline given fromBoolean[R <: Boolean]: Candidate[R] = new Candidate[R]:
        type OutT = DFBool
        def apply(arg: R): DFToken[DFBool] = Token(DFBool, arg)
      transparent inline given fromBit[R <: BitNum]: Candidate[R] = new Candidate[R]:
        type OutT = DFBit
        def apply(arg: R): DFToken[DFBit] = Token(DFBit, arg)
      transparent inline given fromDFBoolOrBitToken[
          T <: DFBoolOrBit,
          R <: DFToken[T]
      ]: Candidate[R] = new Candidate[R]:
        type OutT = T
        def apply(arg: R): DFToken[T] = arg
    end Candidate

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
  end Token

  object Val:
    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit dataflow value."
    )
    trait Candidate[R]:
      type OutT <: DFBoolOrBit
      def apply(arg: R)(using DFC): DFValOf[OutT]
    object Candidate:
      transparent inline given fromTokenCandidate[R](using
          ic: Token.Candidate[R]
      ): Candidate[R] = new Candidate[R]:
        type OutT = ic.OutT
        def apply(arg: R)(using DFC): DFValOf[OutT] = DFVal.Const(ic(arg))
      transparent inline given fromDFBoolOrBitVal[T <: DFBoolOrBit, R <: T <> VAL]: Candidate[R] =
        new Candidate[R]:
          type OutT = T
          def apply(arg: R)(using DFC): T <> VAL = arg

    private def b2b[T <: DFBoolOrBit, R](dfType: T, arg: R)(using
        ic: Candidate[R],
        dfc: DFC
    ): T <> VAL = ???
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type, NoArgs]
final lazy val DFBit = ir.DFBit.asFE[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived
