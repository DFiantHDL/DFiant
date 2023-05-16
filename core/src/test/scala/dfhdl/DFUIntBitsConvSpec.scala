package dfhdl

import dfhdl.TestUtils._

class UIntBitsConvSpec extends DFTopSpec {
  @df class Top extends DFDesign {
    val xu = UInt(8) <> IN
    val yb = Bits(8) <> OUT
    val xb = Bits(8) <> IN
    val yu = UInt(8) <> OUT
    yb := xu
    yb := d"8'25"
    yu := xb
    yu := h"19"
  }

  val top = new Top

  val expectedCodeString : String =
    """|@df final class Top extends DFDesign {
       |  val xu = UInt(8) <> IN
       |  val yb = Bits(8) <> OUT
       |  val xb = Bits(8) <> IN
       |  val yu = UInt(8) <> OUT
       |  yb     := xu.bits
       |  yb     := h"8'19"
       |  yu     := xb.uint
       |  yu     := 25
       |}""".stripMargin

  test("codeString generation") {
    assert(top.codeString =@= expectedCodeString)
  }
}

