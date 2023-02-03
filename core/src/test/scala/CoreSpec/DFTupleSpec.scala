package CoreSpec
import dfhdl.*
import munit.*

class DFTupleSpec extends DFSpec:
  val tplA = (UInt(8), Bit, Bits(3))
  val tokenA = tplA token (22, 1, b"101")
end DFTupleSpec
