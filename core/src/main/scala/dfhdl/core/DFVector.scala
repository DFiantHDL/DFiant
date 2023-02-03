package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import dfhdl.core.CompanionsDFBits.BitIndex

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

  protected[core] object IndexWidth
      extends Check2[
        Int,
        Int,
        [IW <: Int, W <: Int] =>> IW == W,
        [IW <: Int, W <: Int] =>> "The index width " + IW +
          " is different than the expected width of the vector address " + W
      ]
  protected object `LL == RL`
      extends Check2[
        Int,
        Int,
        [LL <: Int, RL <: Int] =>> LL == RL,
        [LL <: Int, RL <: Int] =>> "The argument vector length (" + RL +
          ") is different than the receiver vector length (" + LL + ")."
      ]

  sealed class ComposedModifier[D <: Int, M <: ModifierAny](val cellDim: D, val modifier: M)

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

    object TC:
      import DFToken.TC
      given DFVectorTokenFromVector[T <: DFTypeAny, D1 <: Int, E, R <: Iterable[E]](using
          tc: TC[T, E]
      ): TC[DFVector[T, Tuple1[D1]], R] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R): Out =
          Token(dfType, arg.map(tc(dfType.cellType, _).asIR.data).toVector)
      given DFVectorTokenFromSEV[T <: DFTypeAny, D1 <: Int, E, R <: SameElementsVector[E]](using
          tc: TC[T, E]
      ): TC[DFVector[T, Tuple1[D1]], R] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R): Out =
          Token(
            dfType,
            Vector.fill(dfType.cellDims.head)(
              tc(dfType.cellType, arg.value).asIR.data
            )
          )
    end TC

    object Compare:
      import DFToken.Compare
      given DFVectorCompare[
          T <: DFTypeAny,
          D1 <: Int,
          E,
          R <: Iterable[E],
          Op <: FuncOp,
          C <: Boolean
      ](using
          tc: Compare[T, E, Op, C],
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[DFVector[T, Tuple1[D1]], R, Op, C] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R): Out =
          Token(dfType, arg.map(tc.conv(dfType.cellType, _).asIR.data).toVector)
    end Compare
  end Token

  object Val:
    object TC:
      import DFVal.TC
      given DFVectorValFromDFVectorVal[
          T <: DFTypeAny,
          D1 <: Int,
          RD1 <: Int,
          R <: DFVector[T, Tuple1[RD1]] <> VAL
      ](using
          dfc: DFC,
          check: `LL == RL`.Check[D1, RD1]
      ): TC[DFVector[T, Tuple1[D1]], R] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R): Out =
          check(dfType.asIR.cellDims.head, arg.dfType.asIR.cellDims.head)
          arg.asIR.asValOf[DFVector[T, Tuple1[D1]]]
      given DFVectorValFromDFValVector[
          T <: DFTypeAny,
          D1 <: Int,
          E,
          R <: Iterable[E]
      ](using
          tc: TC[T, E],
          dfc: DFC
      ): TC[DFVector[T, Tuple1[D1]], R] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)).toList
          DFVal.Func(dfType, FuncOp.++, dfVals)
      given DFVectorValFromSEV[
          T <: DFTypeAny,
          D1 <: Int,
          E,
          R <: SameElementsVector[E]
      ](using
          tc: TC[T, E],
          dfc: DFC
      ): TC[DFVector[T, Tuple1[D1]], R] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R): Out =
          val dfVals =
            List.fill(dfType.cellDims.head)(tc.conv(dfType.cellType, arg.value))
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
      end DFVectorValFromSEV
    end TC
    object Compare:
      import DFVal.Compare
      given DFVectorCompareDFValVector[
          T <: DFTypeAny,
          D1 <: Int,
          E,
          R <: Iterable[E],
          Op <: FuncOp,
          C <: Boolean
      ](using
          dfc: DFC,
          tc: Compare[T, E, Op, C],
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[DFVector[T, Tuple1[D1]], R, Op, C] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)).toList
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
      end DFVectorCompareDFValVector
    end Compare
  end Val
end DFVector
