package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import scodec.bits._
import singleton.ops._

class DFEnumAutoTest extends Properties("DFEnumAutoTest") {
  object Foo extends Enum.Auto {
    sealed class Entry extends Enum.Auto.Entry {val value = bin"1"}
    case object Bar0 extends Entry
    case object Bar1 extends Entry
    case object Bar2 extends Entry
  }

  trait MyDesign extends DFDesign {
    val e = Foo.DFEnum()
    implicitly[Require[e.Width == 2]]
    e := Foo.Bar1
//    e == Foo.Bar2
    e.bits(1,0)
    illTyped("""e.bits(3,0)""", "Bit index 3 is out of range of width 2")
  }
}


class DFEnumManualTest extends Properties("DFEnumManualTest") {
  object Foo extends Enum.Manual[2] {
    sealed class Entry(value : BitVector) extends Enum.Manual.Entry(value)
    case object Bar1 extends Entry(bin"01")
    case object Bar2 extends Entry(bin"10")
  }


  trait MyDesign extends DFDesign {
    val e = Foo.DFEnum()
    val f = e.init(Foo.Bar1, Foo.Bar2)
    implicitly[Require[f.Width == 2]]
    f := Foo.Bar1
//    f == Foo.Bar2
    f.prev.bits(1,0)
    //f.prev(2).prev(2).bits(1,0) TODO: causes compiler crash
    illTyped("""f.bits(3,0)""", "Bit index 3 is out of range of width 2")
  }

}
