package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import dfhdl.core.DFBits.BitIndex

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance

type DFVector[+T <: DFTypeAny, +D <: NonEmptyTuple] =
  DFType[ir.DFVector, Args2[T @uncheckedVariance, D @uncheckedVariance]]

object DFVector:
  def apply[T <: DFTypeAny, D <: NonEmptyTuple](
      cellType: T,
      cellDims: List[IntParam[Int]]
  )(using DFC): DFVector[T, D] =
    ir.DFVector(cellType.asIR, cellDims.map(_.ref))
      .asFE[DFVector[T, D]]
  @targetName("givenApply")
  given apply[T <: DFTypeAny, D <: NonEmptyTuple](using
      dfc: DFC,
      cellType: T,
      cellDims: ValueOfTuple[D]
  ): DFVector[T, D] =
    DFVector(
      cellType,
      cellDims.value.toList.asInstanceOf[List[IntP]].map(x => IntParam.fromValue(x))
    )

  extension [T <: DFTypeAny, D <: NonEmptyTuple](dfType: DFVector[T, D])
    def cellType: T = dfType.asIR.cellType.asFE[T]
  extension [T <: DFTypeAny, D1 <: IntP](dfType: DFVector[T, Tuple1[D1]])
    def lengthInt(using dfc: DFC): Int =
      import dfc.getSet
      dfType.asIR.cellDimParamRefs.head.getInt
    def lengthIntParam(using dfc: DFC): IntParam[D1] =
      dfType.asIR.cellDimParamRefs.head.get.asInstanceOf[IntParam[D1]]

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

  sealed class ComposedModifier[D <: IntP, M](val cellDim: D, val modifier: M)
  object Ops:
    extension [T <: DFType.Supported, D <: IntP](t: T)(using tc: DFType.TC[T])
      // transparent inline def X(inline cellDim: Int*): DFType =
      //   x(dfType, cellDim*)
      inline infix def X(
          cellDim: IntParam[D]
      )(using DFC): DFVector[tc.Type, Tuple1[D]] =
        DFVector[tc.Type, Tuple1[D]](tc(t), List(cellDim))
    extension [T <: DFType.Supported, D <: IntP, M <: ModifierAny](t: T)(using tc: DFType.TC[T])
      infix def X(
          composedModifier: ComposedModifier[D, M]
      )(using DFC): DFVal[DFVector[tc.Type, Tuple1[D]], M] =
        DFVal.Dcl(
          DFVector(tc(t), List(IntParam.fromValue(composedModifier.cellDim))),
          composedModifier.modifier
        )
//      inline def X(
//          inline cellDim0: Int,
//          inline cellDim1: Int
//      ): DFVector[tc.Type, Tuple2[cellDim0.type, cellDim1.type]] =
//        DFVector(tc(t), Tuple2(cellDim0, cellDim1))
//      inline def X(
//          inline cellDim0: Int,
//          inline cellDim1: Int,
//          inline cellDim2: Int
//      ): DFVector[tc.Type, Tuple3[
//        cellDim0.type,
//        cellDim1.type,
//        cellDim2.type
//      ]] =
//        DFVector(tc(t), Tuple3(cellDim0, cellDim1, cellDim2))
    end extension
  end Ops

  object Val:
    object TC:
      import DFVal.TC
      given DFVectorValFromDFVectorVal[
          T <: DFTypeAny,
          D1 <: IntP,
          RD1 <: IntP,
          RP,
          R <: DFValTP[DFVector[T, Tuple1[RD1]], RP]
      ](using
          check: `LL == RL`.CheckNUB[D1, RD1]
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = RP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          check(dfType.lengthInt, arg.dfType.lengthInt)
          arg.asValTP[DFVector[T, Tuple1[D1]], RP]
      given DFVectorValFromDFValVector[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: Iterable[E],
          TCE <: TC[T, E]
      ](using
          tc: TCE
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = tc.OutP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)).toList
          DFVal.Func(dfType, FuncOp.++, dfVals)
      given DFVectorValFromSEV[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: SameElementsVector[E],
          TCE <: TC[T, E]
      ](using
          tc: TCE
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = tc.OutP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val dfVals =
            List.fill(dfType.lengthInt)(tc.conv(dfType.cellType, arg.value))
          DFVal.Func(dfType, FuncOp.++, dfVals)
      end DFVectorValFromSEV
    end TC
    object Compare:
      import DFVal.Compare
      given DFVectorCompareDFValVector[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: Iterable[E],
          Op <: FuncOp,
          C <: Boolean,
          TC <: Compare[T, E, Op, C]
      ](using
          tc: TC,
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[DFVector[T, Tuple1[D1]], R, Op, C] with
        type OutP = tc.OutP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)).toList
          DFVal.Func(dfType, FuncOp.++, dfVals)
      end DFVectorCompareDFValVector
    end Compare
    object Ops:
      extension [T <: DFTypeAny, D1 <: IntP, M <: ModifierAny](
          lhs: DFVal[DFVector[T, Tuple1[D1]], M]
      )
        @targetName("applyDFVector")
        def apply[I](
            idx: Exact[I]
        )(using
            c: DFUInt.Val.UBArg[D1, I],
            dfc: DFC
        ): DFVal[T, M] = trydf {
          val idxVal = c(lhs.dfType.lengthIntParam, idx)(using dfc.anonymize)
          DFVal.Alias.ApplyIdx(lhs.dfType.cellType, lhs, idxVal)
        }
        def elements(using DFC): Vector[DFValOf[T]] =
          import DFDecimal.StrInterpOps.d
          val elementType = lhs.dfType.cellType
          Vector.tabulate(lhs.dfType.lengthInt)(i =>
            val idxVal = DFVal.Const(DFInt32, Some(BigInt(i)))
            DFVal.Alias.ApplyIdx(elementType, lhs, idxVal)(using dfc.anonymize)
          )
      end extension
    end Ops
  end Val
end DFVector
