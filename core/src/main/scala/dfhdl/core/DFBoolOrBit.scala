package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import annotation.{implicitNotFound, targetName}

type BitNum = 0 | 1
type BitOrBool = BitNum | Boolean
type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  type Data = Option[Boolean]
  type Token = DFToken[DFBoolOrBit]
  given DFBool = DFBool
  given DFBit = DFBit
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type, NoArgs]
final lazy val DFBit = ir.DFBit.asFE[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived
