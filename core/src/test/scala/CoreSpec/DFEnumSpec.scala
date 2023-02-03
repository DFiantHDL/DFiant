package CoreSpec
import dfhdl.*
import munit.*
import internals.Inlined
import collection.immutable.ListMap

class DFEnumSpec extends DFSpec:
  enum MyEnum5(val value: UInt[8] <> TOKEN) extends Encode.Manual(8):
    case Foo extends MyEnum5(200)
end DFEnumSpec
