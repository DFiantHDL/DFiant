package CoreSpec
import dfhdl.*
import munit.*
import internals.Inlined

class DFBoolOrBitSpec extends DFSpec:
  test("Inlined width") {
    Bit.width.verifyInlined(1)
    Boolean.width.verifyInlined(1)
  }
end DFBoolOrBitSpec
