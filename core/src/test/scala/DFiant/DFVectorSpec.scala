package DFiant

class DFVectorSpec extends DFSpec {
  val one = 1
  val two = 3
  val three = 3
  val four = 4
  val negOne = -1
  val a = Bits(4)

  val boolArr = Bool.X(5) <> VAR init Vector(0, 1, 0, 1, 1)
  val uintArr = UInt(8).X(5) <> VAR init Vector(0, 1, 0, 1, 1)
  val bitsArr = Bits(8).X(5) <> VAR init Vector[Bits.Token](h"21",h"92",h"77",h"33",h"11")

}
