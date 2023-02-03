package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.quoted.*
import collection.immutable.ListMap
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.unchecked.uncheckedVariance

type FieldsOrTuple = DFStruct.Fields | NonEmptyTuple
type DFStruct[+F <: FieldsOrTuple] =
  DFType[ir.DFStruct, Args1[F @uncheckedVariance]]
object DFStruct:
  abstract class Fields extends Product with Serializable
  type Token[+F <: FieldsOrTuple] = DFToken[DFStruct[F]]
end DFStruct
