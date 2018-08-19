package DFiant

import DFiant.basiclib._
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import shapeless.<:!<

trait DFSInt[W] extends DFSInt.Unbounded {
  type Width = W
}

object DFSInt extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFSInt.type] {
    type LW = Width
    type TVal = DFSInt[LW]
    type TVar = DFSInt.Var[LW]
    type TToken = DFSInt.Token
    type Extendable
    def +  [R](right: Op.Able[R])(implicit op: `Op+`.Builder[TVal, Extendable, R]) = op(left, right)
    def -  [R](right: Op.Able[R])(implicit op: `Op-`.Builder[TVal, Extendable, R]) = op(left, right)
    def *  [R](right: Op.Able[R])(implicit op: `Op*`.Builder[TVal, Extendable, R]) = op(left, right)
    //  def /  (right : DFSInt)         : DFSInt = ???

    def <  [R](right: Op.Able[R])(implicit op: `Op<`.Builder[TVal, R]) = op(left, right)
    def >  [R](right: Op.Able[R])(implicit op: `Op>`.Builder[TVal, R]) = op(left, right)
    def <= [R](right: Op.Able[R])(implicit op: `Op<=`.Builder[TVal, R]) = op(left, right)
    def >= [R](right: Op.Able[R])(implicit op: `Op>=`.Builder[TVal, R]) = op(left, right)

    def == [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) = op(left, right)
    def == [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) = op(left, right)
    def == (that : BigInt)(implicit op: `Op==`.Builder[TVal, BigInt]) = op(left, that)
    def != [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[TVal, R]) = op(left, right)
    def != [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[TVal, R]) = op(left, right)
    def != (that : BigInt)(implicit op: `Op!=`.Builder[TVal, BigInt]) = op(left, that)


    def extendBy[N](numOfBits : Natural.Int.Checked[N])(
      implicit tfs : TwoFace.Int.Shell2[+, LW, Int, N, Int], ctx : DFAny.Alias.Context
    ) : DFSInt.Var[tfs.Out] = ??? //new DFSInt.NewVar(tfs(width, numOfBits), getInit).assign(left, blk)

    def isZero(implicit ctx : DFAny.Op.Context) = left == 0
    def isNonZero(implicit ctx : DFAny.Op.Context) = left != 0
    //  def toDFSInt[SW](implicit tfs : TwoFace.Int.)
    def extendable(implicit ctx : DFAny.Alias.Context) : DFSInt[LW] with DFSInt.Extendable = DFSInt.extendable[LW](left)

    //    def within[Start, End](right : XRange[Start, End])(implicit op : OpWithin.Builder[TVal, XRange[Start, End]]) = op(left, right)
    //    trait matchdf extends super.matchdf {
    //      def casedf[R <: Unbounded](right : R)(block : => Unit)(implicit op: `Op==`.Builder[TVal, right.TVal]) : Unit = {}
    //      def casedf[R](that : Int)(block : => Unit)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : Unit = {}
    //      def casedf[R](that : Long)(block : => Unit)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : Unit = {}
    //      def casedf(that : BigInt)(block : => Unit)(implicit op: `Op==`.Builder[TVal, BigInt]) : Unit = {}
    //
    //    }
    override lazy val typeName: String = s"DFSInt[$width]"
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFSInt[W] with DFAny.Var {}

  trait Extendable {
    type Extendable = true
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit ctx : DFAny.NewVar.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : NewVar[W] = new NewVar(checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W])(
    implicit ctx : DFAny.NewVar.Context
  ) : NewVar[W] = new NewVar(checkedWidth.unsafeCheck())
  //  def rangeUntil(supLimit : Int)    : Var = rangeUntil(intToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : Long)   : Var = rangeUntil(longToBigIntBits(supLimit))
  //  def rangeUntil(supLimit : BigInt) : Var = apply(bigIntRepWidth(supLimit-1))
  //  def rangeTo(maxLimit : Int)       : Var = rangeTo(intToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : Long)      : Var = rangeTo(longToBigIntBits(maxLimit))
  //  def rangeTo(maxLimit : BigInt)    : Var = apply(bigIntRepWidth(maxLimit))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final class NewVar[W](width : TwoFace.Int[W])(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar(width, s"DFSInt($width)") with Var[W] {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
  }

  final class Alias[W](aliasedVars : List[DFAny], reference : AliasReference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias(aliasedVars, reference) with Var[W] {
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toSInt
  }

  protected[DFiant] def extendable[W](extendedVar : DFSInt[W])(implicit ctx : DFAny.Alias.Context)
  : Var[W] with Extendable = new DFAny.Alias(List(extendedVar), AliasReference.AsIs(".extendable")) with Var[W] with Extendable {
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toSInt
    override def toString : String = s"DFSInt[$width] & Extendable"
  }

  protected[DFiant] def const[W](token : DFSInt.Token)(implicit ctx : DFAny.Const.Context) : DFSInt[W] =
    new DFAny.Const(token) with DFSInt[W]

  protected[DFiant] def port[W, Dir <: DFDir](dfVar : DFSInt[W], dir : Dir)(implicit ctx : DFAny.Port.Context) : DFSInt[W] <> Dir =
    new DFAny.Port[DFSInt[W], Dir](dfVar, dir) with DFSInt[W] { }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (val width : Int, val valueSInt : BigInt, val bubble : Boolean) extends DFAny.Token {
    lazy val valueBits : BitVector = valueSInt.toBitVector(width)
    lazy val bubbleMask: BitVector = bubble.toBitVector(width)
    def mkTokenU(that : Token, result : BigInt, resultWidth : Int) : Token = {
      if (this.isBubble || that.isBubble) Token(resultWidth, Bubble)
      else Token(resultWidth, result.asUnsigned(resultWidth))
    }

    final def + (that : Token) : Token = mkTokenU(that, this.valueSInt + that.valueSInt, scala.math.max(this.width, that.width) + 1)
    final def - (that : Token) : Token = mkTokenU(that, this.valueSInt - that.valueSInt, scala.math.max(this.width, that.width) + 1)
    final def * (that : Token) : Token = mkTokenU(that, this.valueSInt * that.valueSInt, this.width + that.width)
    final def / (that : Token) : Token = mkTokenU(that, this.valueSInt / that.valueSInt, this.width)
    final def % (that : Token) : Token = mkTokenU(that, this.valueSInt % that.valueSInt, that.width)
    final def <  (that : Token) : DFBool.Token = DFBool.Token(this.valueSInt < that.valueSInt, this.isBubble || that.isBubble)
    final def >  (that : Token) : DFBool.Token = DFBool.Token(this.valueSInt > that.valueSInt, this.isBubble || that.isBubble)
    final def <= (that : Token) : DFBool.Token = DFBool.Token(this.valueSInt <= that.valueSInt, this.isBubble || that.isBubble)
    final def >= (that : Token) : DFBool.Token = DFBool.Token(this.valueSInt >= that.valueSInt, this.isBubble || that.isBubble)
    final def == (that : Token) : DFBool.Token = DFBool.Token(this.valueSInt == that.valueSInt, this.isBubble || that.isBubble)
    final def != (that : Token) : DFBool.Token = DFBool.Token(this.valueSInt != that.valueSInt, this.isBubble || that.isBubble)

    override def codeString: String = if (isBubble) "Φ" else valueSInt.codeString
    override def valueString : String = valueSInt.toString()
  }

  object Token {
    import DFAny.TokenSeq
    def +  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l + r)
    def -  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l - r)
    def *  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l * r)
    def /  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l / r)
    def %  (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l % r)
    def <  (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l < r)
    def >  (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l > r)
    def <= (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l <= r)
    def >= (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l >= r)
    def == (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l == r)
    def != (left : Seq[Token], right : Seq[Token]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l != r)

    def apply(width : Int, value : Int) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : Long) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : BigInt) : Token = {
      if (value < 0 ) throw new IllegalArgumentException(s"Unsigned token value must not be negative. Found $value")
      new Token(width, value, false)
    }
    def apply(width : Int, value : Bubble) : Token = new Token(width, 0, true)
    def apply(width : Int, token : Token) : Token = {
      //TODO: Boundary checks
      new Token(width, token.valueSInt, token.bubble)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends Port {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[LW, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFSInt[LW], Dir] = (right, dir) => port[LW, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.Width](List(left), AliasReference.AsIs(s".as(DFSInt(${mold.width}))"))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends Init {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      private type IntWithinWidth[LW] = CompileTime[Natural.Int.Cond[GetArg0] && (BitsWidthOf.CalcInt[GetArg0] <= LW)]
      private type LongWithinWidth[LW] = CompileTime[Natural.Long.Cond[GetArg0] && (BitsWidthOf.CalcLong[GetArg0] <= LW)]
      implicit class DFSIntBubble[LW](val right : Bubble) extends Able[DFSInt[LW]]
      implicit class DFSIntToken[LW](val right : DFSInt.Token) extends Able[DFSInt[LW]]
      implicit class DFSIntTokenSeq[LW](val right : Seq[DFSInt.Token]) extends Able[DFSInt[LW]]
      implicit class DFSIntInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFSInt[LW]]
      implicit class DFSIntLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFSInt[LW]]
      implicit class DFSIntBigInt[LW](val right : BigInt) extends Able[DFSInt[LW]]

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFSInt[LW]]]) : Seq[Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => DFSInt.Token(width, t)
          case (t : DFSInt.Token) => DFSInt.Token(width, t)
          case (t : Int) => DFSInt.Token(width, t)
          case (t : Long) => DFSInt.Token(width, t)
          case (t : BigInt) => DFSInt.Token(width, t)
        })

    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFSInt[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends Prev {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev[LW](implicit ctx : DFAny.Alias.Context) : Builder[DFSInt[LW]] = new Builder[DFSInt[LW]] {
        def apply[P](left : DFSInt[LW], right : Natural.Int.Checked[P]) : DFSInt[LW] =
          new Alias(List(left), AliasReference.Prev(right))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      val left = value
      def +  [RW](right : DFSInt[RW])(implicit op: `Op+`.Builder[L, Extendable, DFSInt[RW]]) = op(left, right)
      def -  [RW](right : DFSInt[RW])(implicit op: `Op-`.Builder[L, Extendable, DFSInt[RW]]) = op(left, right)
      def <  [RW](right : DFSInt[RW])(implicit op: `Op<`.Builder[L, DFSInt[RW]]) = op(left, right)
      def >  [RW](right : DFSInt[RW])(implicit op: `Op>`.Builder[L, DFSInt[RW]]) = op(left, right)
      def <= [RW](right : DFSInt[RW])(implicit op: `Op<=`.Builder[L, DFSInt[RW]]) = op(left, right)
      def >= [RW](right : DFSInt[RW])(implicit op: `Op>=`.Builder[L, DFSInt[RW]]) = op(left, right)
      def <> [RW, RDIR <: DFDir](port : DFSInt[RW] <> RDIR)(
        implicit op: `Op<>`.Builder[DFSInt[RW], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
      def toDFSInt(implicit op : Const.PosOnly[Const.PosOnly[_,_],L]) = op(left)
    }
    trait Implicits {
      sealed class DFSIntFromInt[L <: Int](left : L) extends Able[L](left)
      final implicit def DFSIntFromInt[L <: Int](left: L): DFSIntFromInt[L] = new DFSIntFromInt(left)
      sealed class DFSIntFromXInt[L <: XInt](left : L) extends Able[L](left)
      final implicit def DFSIntFromXInt[L <: XInt](left: L): DFSIntFromXInt[L] = new DFSIntFromXInt(left)
      sealed class DFSIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFSIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): DFSIntFromLong[L] = new DFSIntFromLong(left)
      sealed class DFSIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFSIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): DFSIntFromXLong[L] = new DFSIntFromXLong(left)
      sealed class DFSIntFromBigInt[L <: BigInt](left : L) extends Able[L](left)
      final implicit def DFSIntFromBigInt[L <: BigInt](left: L): DFSIntFromBigInt[L] = new DFSIntFromBigInt[L](left)
      final implicit def ofDFSInt[R <: DFSInt.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Implicit Evidence of DFSInt
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait PosNeg[N] {
      type W
      def apply(value : N) : (DFSInt[W], Boolean)
    }
    object PosNeg {
      type Aux[N, W0] = PosNeg[N]{type W = W0}
      import singleton.ops.math.Abs
      implicit def fromInt[N <: Int](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Int[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFSInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromLong[N <: Long](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Long[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFSInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromBigInt[N <: BigInt](implicit ctx : DFAny.Const.Context)
      : Aux[N, Int] = new PosNeg[N] {
        type W = Int
        def apply(value : N) : (DFSInt[W], Boolean) = {
          val absValue = value.abs
          (const[W](Token(absValue.bitsWidth, absValue)), value < 0)
        }
      }
    }
    trait PosOnly[Sym, N] {
      type W
      def apply(value : N) : DFSInt[W]
    }
    object PosOnly {
      type Aux[Sym, N, W0] = PosOnly[Sym, N]{type W = W0}
      object `N >= 0` extends `N >= 0` {
        type MsgCommon[N] = "Operation or assignment do not permit a negative number. Found literal: " + ToString[N]
      }
      implicit def fromInt[Sym, N <: Int](
        implicit
        ctx : DFAny.Const.Context,
        checkPos : `N >= 0`.Int.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Int[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFSInt[W] = {
          checkPos.unsafeCheck(value)
          const[W](Token(w(value), value))
        }
      }
      implicit def fromLong[Sym, N <: Long](
        implicit
        ctx : DFAny.Const.Context,
        checkPos : `N >= 0`.Long.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Long[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFSInt[W] = {
          checkPos.unsafeCheck(value)
          const[W](Token(w(value), value))
        }
      }
      implicit def fromBigInt[Sym, N <: BigInt](implicit ctx : DFAny.Const.Context)
      : Aux[Sym, N, Int] = new PosOnly[Sym, N] {
        type W = Int
        def apply(value : N) : DFSInt[W] = {
          `N >= 0`.BigInt.unsafeCheck(value)
          const[W](Token(value.bitsWidth, value))
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>`[Ctx, SkipLengthCheck] extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment/connect operation with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = SkipLengthCheck || (LW >= RW)
        type Msg[LW, RW] = "An assignment operation does not permit a wider RHS expression. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, R, RW](properR : (L, R) => DFSInt[RW]) : Aux[L, R, DFSInt[RW]] =
        new Builder[L, R] {
          type Comp = DFSInt[RW]
          def apply(leftL : L, rightR : R) : Comp =  properR(leftL, rightR)
        }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
        implicit
        ctx : Ctx,
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFSInt[LW], DFSInt[RW], DFSInt[RW]] =
        create[DFSInt[LW], DFSInt[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          right
        })

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](
        implicit
        ctx : Ctx,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFSInt[LW], R, DFSInt[RW]] = create[DFSInt[LW], R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right
      })
    }
  }
  object `Op:=` extends `Ops:=,<>`[DFAny.Op.Context, false]
  object `Op<>` extends `Ops:=,<>`[DFAny.Connector.Context, true]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`[K <: `Ops+Or-`.Kind](kind : K) {
    //NCW = No-carry width
    //WCW = With-carry width
    class Component[NCW, WCW](val wc : DFSInt[WCW])(implicit ctx : DFAny.Alias.Context) extends
      DFAny.Alias(List(wc), AliasReference.BitsWL(wc.width-1, 0, s".bits(${wc.width-2}, 0).uint")) with DFSInt[NCW] {
      lazy val c = new DFBool.Alias(List(wc), AliasReference.BitsWL(1, wc.width-1, s".bit(${wc.width-1})")).setAutoName(s"${ctx.n.value}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toSInt
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[L, LE, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcWCW[LW, RW] = Max[LW, RW] + 1
        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, LE, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[NCW, WCW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Comp = Component[NCW, WCW]
            def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, LE, R, Comp] =
              new Builder[L, LE, R] {
                type Comp = Component[NCW, WCW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import ctx.basicLib.DFSIntOps._
                  val (creationKind, left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = wcW(left.width, right.width)
                  val opInst = creationKind match {
                    case `Ops+Or-`.+ => new DFiant.basiclib.DFSIntOps.`Comp+`(left.width, right.width, opWidth)
                    case `Ops+Or-`.- => new DFiant.basiclib.DFSIntOps.`Comp-`(left.width, right.width, opWidth)
                  }
                  opInst.inLeft <> left
                  opInst.inRight <> right
                  val wc = new DFSInt.Alias[WCW](List(opInst.outResult), AliasReference.AsIs("")).setAutoName(s"${ctx.n.value}WC")
                  // Creating extended component aliasing the op
                  new Component[NCW, WCW](wc)
                }
              }
          }
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, LE, R <: DFSInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, DFSInt[RW], RW]
      ) = detailedBuilder((left, right) => (kind, left, right))

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, LE, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosNeg.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => {
        val (right, negative) = rConst(rightNum)
        val creationKind = if (negative) -kind else kind
        (creationKind, left, right)
      })

      implicit def evConst_op_DFSInt[L, LW, LE, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFSInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => {
        (kind, lConst(leftNum), right)
      })
    }
  }
  protected object `Ops+Or-` {
    abstract class Kind(val opString : String) {
      def unary_- : Kind
    }
    case object + extends Kind("+") {def unary_- : Kind = `Ops+Or-`.-}
    case object - extends Kind("-") {def unary_- : Kind = `Ops+Or-`.+}
  }
  object `Op+` extends `Ops+Or-`(`Ops+Or-`.+)
  object `Op-` extends `Ops+Or-`(`Ops+Or-`.-)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // * operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op*` {
    //NCW = No-carry width
    //WCW = With-carry width
    //CW = Carry width
    class Component[NCW, WCW, CW](val wc : DFSInt[WCW], ncW : TwoFace.Int[NCW], cW : TwoFace.Int[CW])(
      implicit ctx : DFAny.Alias.Context
    ) extends DFAny.Alias(List(wc), AliasReference.BitsWL(ncW, 0, s".bits(${wc.width-cW-1}, 0).uint")) with DFSInt[NCW] {
      lazy val c = new DFBits.Alias[CW](List(wc), AliasReference.BitsWL(cW, wc.width - cW, s".bits(${wc.width-1}, ${wc.width-cW})")).setAutoName(s"${ctx.n.value}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toSInt
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Op `*` with the type ${R}")
    trait Builder[L, LE, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcWCW[LW, RW] = LW + RW
        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
        type CalcCW[LW, RW] = CalcWCW[LW, RW] - CalcNCW[LW, RW]
        type CW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, LE, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, CW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
          cW : Inference.CW[LW, RW, CW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[NCW, WCW, CW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Comp = Component[NCW, WCW, CW]
            def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, LE, R, Comp] =
              new Builder[L, LE, R] {
                type Comp = Component[NCW, WCW, CW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import ctx.basicLib.DFSIntOps._
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val wcWidth = wcW(left.width, right.width)
                  val ncWidth = ncW(left.width, right.width)
                  val cWidth = cW(left.width, right.width)

                  val opInst = new DFiant.basiclib.DFSIntOps.`Comp*`(left.width, right.width, wcWidth)
                  opInst.inLeft <> left
                  opInst.inRight <> right
                  val wc = new DFSInt.Alias[WCW](List(opInst.outResult), AliasReference.AsIs("")).setAutoName(s"${ctx.n.value}WC")

                  // Creating extended component aliasing the op
                  new Component[NCW, WCW, CW](wc, ncWidth, cWidth)
                }
              }
          }
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, LE, R <: DFSInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, DFSInt[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, LE, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_,_], R, RW],
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFSInt[L, LW, LE, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFSInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare(opKind : DiSoOp.Kind)(opFunc : (Seq[DFSInt.Token], Seq[DFSInt.Token]) => Seq[DFBool.Token]) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      object `VecW >= ConstW` extends Checked1Param.Int { //Needs to be mitigated to a warning
      type Cond[VW, CW] = VW >= CW
        type Msg[VW, CW] = "A static boolean result detected, due to an unsigned comparison between a DF variable and a larger number. Found: DFVar-width = "+ ToString[VW] + " and Num-width = " + ToString[CW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFSInt[LW], DFSInt[RW]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import ctx.basicLib.DFSIntOps._
        val (left, right) = properLR(leftL, rightR)
        val opInst = opKind match {
          case DiSoOp.Kind.== => new DFiant.basiclib.DFSIntOps.`Comp==`(left.width, right.width)
          case DiSoOp.Kind.!= => new DFiant.basiclib.DFSIntOps.`Comp!=`(left.width, right.width)
          case DiSoOp.Kind.<  => new DFiant.basiclib.DFSIntOps.`Comp<`(left.width, right.width)
          case DiSoOp.Kind.>  => new DFiant.basiclib.DFSIntOps.`Comp>`(left.width, right.width)
          case DiSoOp.Kind.<= => new DFiant.basiclib.DFSIntOps.`Comp<=`(left.width, right.width)
          case DiSoOp.Kind.>= => new DFiant.basiclib.DFSIntOps.`Comp>=`(left.width, right.width)
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }
        opInst.inLeft <> left
        opInst.inRight <> right
        opInst.outResult
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFSInt[LW], DFSInt[RW]] = create[DFSInt[LW], LW, DFSInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFSInt[LW], R] = create[DFSInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFSInt[L, LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_], L, LW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFSInt[RW]] = create[L, LW, DFSInt[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(right.width, left.width)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.Kind.==)(DFSInt.Token.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=)(DFSInt.Token.!=) with `Op!=`
  object `Op<`  extends OpsCompare(DiSoOp.Kind.< )(DFSInt.Token.< )
  object `Op>`  extends OpsCompare(DiSoOp.Kind.> )(DFSInt.Token.> )
  object `Op<=` extends OpsCompare(DiSoOp.Kind.<=)(DFSInt.Token.<=)
  object `Op>=` extends OpsCompare(DiSoOp.Kind.>=)(DFSInt.Token.>=)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}