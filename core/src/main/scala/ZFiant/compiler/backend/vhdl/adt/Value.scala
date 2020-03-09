package ZFiant.compiler.backend.vhdl.adt
import ZFiant.EnumType
import DFiant.internals.StringExtras

sealed trait Value extends HasName with Product with Serializable {
  val rtType : Value.Type
  val name : Name
  def refString : String
}

object Value {
  final case class Const(valueStr : String, rtType : Type) extends Value {
    val name : Name = Name.anonymous
    override def refString: String = valueStr
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Declaration
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Dcl[+Mod <: Dcl.Modifier](modifier : Mod, name : Name, rtType : Type, initStrOption : Option[String]) extends Value with Declaration {
    private val initStr = initStrOption match {
      case Some(i) => s" := $i"
      case None => ""
    }
    def refString : String = name.toString
    override def toString: String =
      s"${modifier.preModStr}$name : ${modifier.portDirStr}$rtType$initStr${modifier.postModStr}"
  }
  object Dcl {
    sealed trait Modifier extends Product with Serializable {
      val preModStr : String
      val postModStr : String
      val portDirStr : String
    }
    object Modifier {
      case object Signal extends Modifier {
        val preModStr : String = "signal "
        val postModStr : String = ";"
        val portDirStr : String = ""
      }
      case object Variable extends Modifier {
        val preModStr : String = "variable "
        val postModStr : String = ";"
        val portDirStr : String = ""
      }
      sealed trait Port extends Modifier {
        val preModStr : String = ""
        val postModStr : String = ""
      }
      object Port {
        case object In extends Port {
          val portDirStr : String = "in  "
        }
        case object Out extends Port {
          val portDirStr : String = "out "
        }
      }
    }
    object Port {
      object In {
        def unapply(arg: Dcl[_]): Boolean = arg.modifier match {
          case Modifier.Port.In => true
          case _ => false
        }
      }
      object Out {
        def unapply(arg: Dcl[_]): Boolean = arg.modifier match {
          case Modifier.Port.Out => true
          case _ => false
        }
      }
    }
    object Signal {
      def unapply(arg: Dcl[_]): Boolean = arg.modifier match {
        case Modifier.Signal => true
        case _ => false
      }
    }
    object Variable {
      def unapply(arg: Dcl[_]): Boolean = arg.modifier match {
        case Modifier.Variable => true
        case _ => false
      }
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Type extends Product with Serializable {
    val width : Int
  }
  object Type {
    ///////////////////////////////////////////////////////////
    // Capabilities
    ///////////////////////////////////////////////////////////
    sealed trait Resizeable extends Type {
      def resize(width : Int) : Type
    }
    sealed trait Invertable extends Type
    sealed trait Reverseable extends Type
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // std_logic_vector
    ///////////////////////////////////////////////////////////
    final case class std_logic_vector(width : Int) extends Resizeable with Invertable with Reverseable {
      def resize(width : Int) : Type = std_logic_vector(width)
      override def toString: String = s"std_logic_vector(${width-1} downto 0)"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // unsigned
    ///////////////////////////////////////////////////////////
    final case class unsigned(width : Int) extends Resizeable {
      def resize(width : Int) : Type = unsigned(width)
      override def toString: String = s"unsigned(${width-1} downto 0)"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // signed
    ///////////////////////////////////////////////////////////
    final case class signed(width : Int) extends Resizeable {
      def resize(width : Int) : Type = signed(width)
      override def toString: String = s"signed(${width-1} downto 0)"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // std_logic
    ///////////////////////////////////////////////////////////
    case object std_logic extends Invertable {
      val width = 1
      override def toString: String = s"std_logic"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // boolean
    ///////////////////////////////////////////////////////////
    case object boolean extends Invertable {
      val width = 1
      override def toString: String = s"boolean"
    }
    ///////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////
    // enumeration
    ///////////////////////////////////////////////////////////
    case class enumeration(name: Name, width : Int, entries : List[enumeration.entry]) extends Type {
      override def toString: String = ???
    }
    object enumeration {
      final case class entry(name : Name) {
        override def toString: String = s"$name"
      }
    }
    ///////////////////////////////////////////////////////////
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Active extends Value {
    def active : Value
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Reference
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Reference extends Value {
    override def refString: String = if (name.isAnonymous) toString else name.toString
  }
  object Reference {
    final case class Resize(name : Name, rtType : Type, refVal : Value) extends Reference
    object Resize {
//      def apply(refVal : Value, width : Int) : resize = refVal.rtType match {
//        case t : Type.Resizeable => resize(t.resize(width), refVal)
//        case _ => ???
//      }
    }
    final case class Invert(name : Name, refVal : Value) extends Reference {
      val rtType: Type = refVal.rtType
    }
//    final case class EdgeCheck(clock : Clock) extends Reference {
//      clock.rtType match {
//        case Type.std_logic => //OK
//        case _ => ??? //Bad
//      }
//      val rtType: Type = Type.boolean
//      override val name: Name = Name.anonymous
//      override def toString: String = s"${clock.edge}(${clock.refString})"
//    }
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Infix function between two arguments
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Func2(name : Name, rtType : Type, leftArg : Value, opStr : String, rightArg : Value) extends Reference {
    override def toString: String = s"${leftArg.refString.applyBrackets()} $opStr ${rightArg.refString.applyBrackets()}"
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
}

//final case class Clock(
//  name : Name, modifier : Value.Dcl.Modifier, edge : Clock.Edge
//) extends Value.Dcl[Value.Dcl.Modifier] with Value.Active {
//  val rtType: Value.Type = Value.Type.std_logic
//  val initStrOption: Option[String] = None
//  def active : Value = Value.Reference.EdgeCheck(this)
//}
//object Clock {
//  sealed trait Edge extends Product with Serializable
//  object Edge {
//    case object Rising extends Edge {
//      override def toString: String = "rising_edge"
//    }
//    case object Falling extends Edge {
//      override def toString: String = "falling_edge"
//    }
//  }
//}
//
//final case class Reset(
//  name : Name, modifier : Value.Dcl.Modifier, polarity : Reset.Polarity
//) extends Value.Dcl[Value.Dcl.Modifier] with Value.Active {
//  val rtType: Value.Type = Value.Type.std_logic
//  val initStrOption: Option[String] = None
//  def active : Value = Value.Func2(Name.anonymous, Value.Type.boolean, this, "=", polarity.compareVal)
//}
//object Reset {
//  sealed trait Polarity extends Product with Serializable {
//    def compareVal : Value
//  }
//  object Polarity {
//    case object Low extends Polarity {
//      override def compareVal: Value = Value.Const("'0'", Value.Type.std_logic)
//    }
//    case object High extends Polarity {
//      override def compareVal: Value = Value.Const("'1'", Value.Type.std_logic)
//    }
//  }
//}