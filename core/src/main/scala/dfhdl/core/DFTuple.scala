package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance
import scala.quoted.*

type DFTuple[+T <: NonEmptyTuple] = DFStruct[T @uncheckedVariance]
object DFTuple:
  private[core] def apply[T <: NonEmptyTuple](t: NonEmptyTuple): DFTuple[T] = ???
  private[core] def apply[T <: NonEmptyTuple](
      fieldList: List[DFTypeAny]
  ): DFTuple[T] = ???

  type Token[+T <: NonEmptyTuple] = DFToken[DFTuple[T]]
  object Token:
    protected[core] def apply[T <: NonEmptyTuple](
        dfType: DFTuple[T],
        data: List[Any]
    ): Token[T] =
      ir.DFToken(dfType.asIR)(data).asTokenOf[DFTuple[T]]
  end Token
end DFTuple
