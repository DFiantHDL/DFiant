package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance

type DFVector[+T <: DFTypeAny, +D <: NonEmptyTuple] =
  DFType[ir.DFVector, Args2[T @uncheckedVariance, D @uncheckedVariance]]

object DFVector:
  def apply[T <: DFTypeAny, D <: NonEmptyTuple](
      cellType: T,
      cellDims: D
  ): DFVector[T, D] =
    ir.DFVector(cellType.asIR, cellDims.toList.asInstanceOf[List[Int]])
      .asFE[DFVector[T, D]]
  @targetName("givenApply")
  given apply[T <: DFTypeAny, D <: NonEmptyTuple](using
      cellType: T,
      cellDims: ValueOfTuple[D]
  ): DFVector[T, D] = DFVector(cellType, cellDims.value)

  extension [T <: DFTypeAny, D <: NonEmptyTuple](dfType: DFVector[T, D])
    def cellType: T = dfType.asIR.cellType.asFE[T]
    def cellDims: List[Int] = dfType.asIR.cellDims

  type Token[+T <: DFTypeAny, +D <: NonEmptyTuple] = DFToken[DFVector[T, D]]
  object Token:
    def apply[T <: DFTypeAny, D <: NonEmptyTuple](
        dfType: DFVector[T, D],
        data: Vector[Any]
    ): Token[T, D] =
      val dim = dfType.asIR.cellDims.head
      require(
        data.length == dim,
        s"The length of the Scala vector (${data.length}) does not match the dataflow vector dimension ($dim)"
      )
      ir.DFVector.Token(dfType.asIR, data).asTokenOf[DFVector[T, D]]
  end Token
end DFVector
