package ZFiant

import singleton.ops._
import singleton.twoface._
trait DFAnyMember
trait DFType {
  type TToken <: DFAny.Token
  type Width
  val width : TwoFace.Int[Width]
}
object DFType {
  implicit def ev[T <: DFAny](t : T) : t.TType = t.dfType
}

trait DFAny extends DFAnyMember {
  type TType <: DFType
  type TVar
  val dfType : TType
  final protected val left : this.type = this
  protected type AsVal = DFAny.Aux[TType, false]
  protected type AsVar = DFAny.Aux[TType, true]
  protected type AsType[T <: DFType] = DFAny.Aux[T, TVar]
  final def bits : AsType[DFBits[dfType.Width]] = DFAny.Alias.BitsWL[dfType.Width, 0, this.type](left, dfType.width, 0)
  final def as[AT <: DFType](aliasType : AT) : AsType[AT] = DFAny.Alias.AsIs[AT, this.type](aliasType, left)
  final def prev : AsVal = DFAny.Alias.Prev[this.type](left, 1)
}

object DFAny {
  protected type Aux[Type <: DFType, Var] = DFAny {
    type TType = Type
    type TVar = Var
  }

  trait `Op:=`[To <: DFAny, From] {
    def apply(left : To, right : From) : Unit = {}
  }

  implicit class VarOps[L <: DFAny](left : L)(implicit isVar : L#TVar =:= true) {
    def := [R](right : R)(implicit op : `Op:=`[L, R]) : Unit = op(left, right)
  }

  trait Token

  sealed trait Constructor[Type <: DFType, Var] extends DFAny {
    type TType = Type
    type TVar = Var
  }

  final case class Const[Type <: DFType](dfType : Type, token : Type#TToken) extends Constructor[Type, false]

  sealed trait Initializable[Type <: DFType, Var] extends Constructor[Type, Var] {
    val externalInit : Seq[TType#TToken]
  }

  sealed trait Port[Type <: DFType, Var] extends Initializable[Type, Var] {
    val dir : Port.Dir
  }
  object Port {
    sealed trait Dir
    object Dir {
      case object IN extends Dir
      case object OUT extends Dir
    }
    final case class In[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken]) extends Port[Type, false] {
      val dir : Port.Dir = Dir.IN
    }
    final case class Out[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken]) extends Port[Type, true] {
      val dir : Port.Dir = Dir.OUT
    }
  }

  case class NewVar[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken]) extends Initializable[Type, true]

  trait Alias[Type <: DFType, RefVal <: DFAny, Var] extends Constructor[Type, Var] {
    val refVal : RefVal
  }
  object Alias {
    final case class AsIs[Type <: DFType, RefVal <: DFAny](dfType : Type, refVal : RefVal) extends Alias[Type, RefVal, RefVal#TVar]
    final case class BitsWL[W, L, RefVal <: DFAny](refVal : RefVal, relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L]) extends Alias[DFBits[W], RefVal, RefVal#TVar]{
      val dfType : TType = DFBits.dfType(relWidth)
    }
    final case class Prev[RefVal <: DFAny](refVal : RefVal, step : Int) extends Alias[RefVal#TType, RefVal, false] {
      val dfType : TType = refVal.dfType
    }
  }

  sealed abstract class Func[Type <: DFType](dfType : Type) extends Constructor[Type, false]
  final case class Func2[Type <: DFType, L <: DFAny, R <: DFAny](dfType: Type)(left : L, right : R) extends Func(dfType)


}



object Test {
  val a = DFUInt(8)
  val aa = a.bits.as(DFUInt(8)).bits

//  a := a
//  implicitly[aa.Var =:= true]
}