package DFiant

import DFiant.internals._
import singleton.ops._
import singleton.twoface._

trait DFBool extends DFBool.Unbounded

object DFBool extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFBool.type] {
    type TVal = DFBool
    type TVar = DFBool.Var
    type TToken = DFBool.Token
    type Width = 1
    def unary_!(implicit ctx : DFAny.Op.Context)               : DFBool = ??? //DFBool.op("!", DFBool.Token.unary_!(getInit), this)

    def || [R](right: Op.Able[R])(implicit op: `Op||`.Builder[TVal, R]) : DFBool = op(left, right)
    def && [R](right: Op.Able[R])(implicit op: `Op&&`.Builder[TVal, R]) : DFBool = op(left, right)
    //  def ^^ (that : DFBool) : DFBool = AlmanacEntryOpXor(this, that)
    //  def ## (that : DFBits.Unsafe)    : DFBits.Unsafe = this.bits() ## that
    //  def ## (that : DFBool)    : DFBits.Unsafe = this.bits() ## that.bits()
    def rising (implicit ctx : DFAny.Op.Context) : DFBool = left && !left.prev(1)
    def falling (implicit ctx : DFAny.Op.Context) : DFBool = !left && left.prev(1)

    def newEmptyDFVar(implicit ctx : DFAny.NewVar.Context) = ??? //new DFBool.NewVar(Seq(DFBool.Token(false)))

    override lazy val typeName : String = s"DFBool"

    //  protected[DFiant] def __!= (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0!=arg1
    //  protected[DFiant] def __== (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0==arg1
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var extends DFAny.Var with DFBool {
//    final def set() : DFBool = this := true
//    final def clear() : DFBool = this := false
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply()(implicit ctx : DFAny.NewVar.Context) : NewVar = new NewVar()
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final class NewVar()(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar(1, "DFBool()") with Var {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
  }

  protected[DFiant] def alias(aliasedVar : DFAny, relBit : Int, deltaStep : Int = 0, aliasCodeString : String)(implicit ctx : DFAny.Alias.Context) : Var =
    new DFAny.Alias(aliasedVar, 1, relBit, deltaStep, aliasCodeString) with Var {
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = DFBool.Token(token.valueBits(0))
    }

  protected[DFiant] def const(token : DFBool.Token)(implicit ctx : DFAny.Const.Context) : DFBool =
    new DFAny.Const(token) with DFBool

  protected[DFiant] def port[Dir <: DFDir](dfVar : DFBool, dir : Dir)(implicit ctx : DFAny.Port.Context) : DFBool <> Dir =
    new DFAny.Port[DFBool, Dir](dfVar, dir) with DFBool {}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (val valueBool : Boolean, val bubble : Boolean) extends DFAny.Token {
    val width : Int = 1
    lazy val valueBits : BitVector = BitVector.bit(valueBool)
    lazy val bubbleMask: BitVector = BitVector.bit(bubble)

    final def && (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.valueBool && that.valueBool)
    }
    final def || (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.valueBool || that.valueBool)
    }
    final def unary_! : Token = {
      if (this.isBubble) Token(Bubble)
      else Token(!this.valueBool)
    }
    final def == (that : Token) : Token = DFBool.Token(this.valueBool == that.valueBool, this.isBubble || that.isBubble)
    final def != (that : Token) : Token = DFBool.Token(this.valueBool != that.valueBool, this.isBubble || that.isBubble)
    override def valueString : String = valueBool.toString()
  }

  object Token {
    import DFAny.TokenSeq
    def || (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l || r)
    def && (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l && r)
    def == (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l == r)
    def != (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l != r)
    def unary_! (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => !t)

    def apply(value : Int) : Token = value match {
      case 0 => Token(false)
      case 1 => Token(true)
    }
    def apply(valueBool : Boolean, bubble : Boolean) : Token = new Token(valueBool, bubble)
    def apply(value : Boolean) : Token = new Token(value, false)
    def apply(value : Bubble) : Token = new Token(false, true)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends Port {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFBool, Dir] = (right, dir) => port[Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends Init {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      private type IntIsBoolean = CompileTime[(GetArg0 == 0) || (GetArg0 == 1)]
      implicit class DFBoolBubble(val right : Bubble) extends Able[DFBool]
      implicit class DFBoolToken(val right : DFBool.Token) extends Able[DFBool]
      implicit class DFBoolTokenSeq(val right : Seq[DFBool.Token]) extends Able[DFBool]
      implicit class DFBoolXInt[T <: XInt](val right : T)(implicit chk : IntIsBoolean) extends Able[DFBool]
      implicit class DFBoolBoolean(val right : Boolean) extends Able[DFBool]

      def toTokenSeq(right : Seq[Able[DFBool]]) : Seq[DFBool.Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => DFBool.Token(t)
          case (t : DFBool.Token) => t
          case (t : Int) => DFBool.Token(t)
          case (t : Boolean) => DFBool.Token(t)
        })
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev : Builder[DFBool, Token] = (left, right) => Able.toTokenSeq(right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends Prev {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev(implicit ctx : DFAny.Alias.Context) : Builder[DFBool] = new Builder[DFBool] {
        def apply[P](left : DFBool, right : Natural.Int.Checked[P]) : DFBool =
          DFBool.alias(left, 0, -right, "")
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
      def ||  (right : DFBool)(implicit op: `Op||`.Builder[L, DFBool]) = op(left, right)
      def &&  (right : DFBool)(implicit op: `Op&&`.Builder[L, DFBool]) = op(left, right)
      def <> [RDIR <: DFDir](port : DFBool <> RDIR)(
        implicit op: `Op<>`.Builder[DFBool, L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
    }
    trait Implicits {
      implicit class DFBoolFromXInt[L <: XInt](left : L) extends Able[L](left)
      implicit class DFBoolFromBoolean[L <: Boolean](left : L) extends Able[L](left)
      implicit def ofDFBool[R <: DFBool.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Implicit Evidence of DFUInt
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Const[Sym, N] {
    def apply(value : N) : DFBool
  }
  object Const {
    implicit def fromInt[Sym, N <: Int](implicit ctx : DFAny.Const.Context, checkBin : BinaryInt.CheckedShellSym[Sym, N])
    : Const[Sym, N] = value => {
      checkBin.unsafeCheck(value)
      const(Token(value))
    }
    implicit def fromBoolean[Sym, N <: Boolean](implicit ctx : DFAny.Const.Context)
    : Const[Sym, N] = value => const(Token(value))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment/connect operation with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      def create[L, R](properR : (L, R) => DFBool)
      : Aux[L, R, DFBool] = new Builder[L, R] {
        type Comp = DFBool
        def apply(leftL : L, rightR : R) : Comp = properR(leftL, rightR)
      }

      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](implicit ctx : DFAny.Op.Context)
      : Aux[DFBool, DFBool, DFBool] = create[DFBool, DFBool]((left, right) => right)

      implicit def evDFBool_op_Const[L <: DFBool, R](implicit ctx : DFAny.Op.Context, rConst : Const[Builder[_,_], R])
      : Aux[DFBool, R, DFBool] = create[DFBool, R]((left, rightNum) => {
        val right = rConst(rightNum)
        right
      })
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class BoolOps(kind : BoolOps.Kind) {
    @scala.annotation.implicitNotFound("Operation is not supported between type ${L} and type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

    object Builder {
      def create[L, R](properLR : (L, R) => (DFBool, DFBool))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = new Builder[L, R] {
        def apply(leftL : L, rightR : R) : Comp = {
          val (left, right) = properLR(leftL, rightR)
          ??? //DFBool.op(kind.opString, kind.opFunc(left.getInit, right.getInit), left, right)
        }
      }

      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](
        implicit
        ctx : DFAny.Op.Context,
      ) : Builder[DFBool, DFBool] =  create[DFBool, DFBool]((left, right) => (left, right))

      implicit def evDFBool_op_Const[L <: DFBool, R](implicit ctx : DFAny.Op.Context, rConst : Const[Builder[_,_], R])
      : Builder[DFBool, R] = create[DFBool, R]((left, rightNum) => {
        val right = rConst(rightNum)
        (left, right)
      })

      implicit def evConst_op_DFBool[L, R <: DFBool](implicit ctx : DFAny.Op.Context, lConst : Const[Builder[_,_], L])
      : Builder[L, DFBool] = create[L, DFBool]((leftNum, right) => {
        val left = lConst(leftNum)
        (left, right)
      })
    }
  }
  protected object BoolOps {
    class Kind(val opString : String, val opFunc : (Seq[DFBool.Token], Seq[DFBool.Token]) => Seq[DFBool.Token])
    case object == extends Kind("==", DFBool.Token.==)
    case object != extends Kind("!=", DFBool.Token.!=)
    case object || extends Kind("||", DFBool.Token.||)
    case object && extends Kind("&&", DFBool.Token.&&)
  }
  object `Op==` extends BoolOps(BoolOps.==) with `Op==`
  object `Op!=` extends BoolOps(BoolOps.!=) with `Op!=`
  object `Op||` extends BoolOps(BoolOps.||)
  object `Op&&` extends BoolOps(BoolOps.&&)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
