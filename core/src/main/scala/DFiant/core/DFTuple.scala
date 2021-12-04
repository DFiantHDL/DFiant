package DFiant.core
import DFiant.compiler.ir
import DFiant.compiler.ir.DFVal.Modifier
import ir.DFVal.Func.Op as FuncOp
import DFiant.internals.*

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance
import scala.quoted.*

type DFTuple[+T] = DFStruct[DFTuple.Fields[T] @uncheckedVariance]
object DFTuple:
  def apply[T <: AnyRef](t: T): DFTuple[T] =
    val fieldList: List[DFTypeAny] =
      t.asInstanceOf[NonEmptyTuple]
        .toList
        // TODO: Hack due to https://github.com/lampepfl/dotty/issues/12721
        .asInstanceOf[List[AnyRef]]
        .map(x => DFType(x))
    DFStruct(Fields[T](fieldList))

  final case class Fields[+T](fieldList: List[DFTypeAny]) extends DFFields:
    override lazy val typeName: String = ir.DFStruct.ReservedTupleName
    fieldList.zipWithIndex.foreach((f, i) => createField(f, (i + 1).toString))

  extension [T](dfType: DFTuple[T])
    def fieldList: List[DFTypeAny] =
      dfType.asIR.fieldMap.values.map(_.asFE[DFTypeAny]).toList

  trait TCZipper[
      T <: NonEmptyTuple,
      V <: NonEmptyTuple,
      O,
      TC[T <: DFTypeAny, V] <: TCConv[T, V, O]
  ]:
    def apply(
        fieldList: List[DFTypeAny],
        tokenTupleValues: List[Any]
    ): List[O]
  object TCZipper:
    transparent inline given [
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFTypeAny, V] <: TCConv[T, V, O]
    ]: TCZipper[T, V, O, TC] = ${
      zipperMacro[T, V, O, TC]
    }
    import DFType.TC.MacroOps.*
    def zipperMacro[
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFTypeAny, V] <: TCConv[T, V, O]
    ](using
        Quotes,
        Type[T],
        Type[V],
        Type[O],
        Type[TC]
    ): Expr[TCZipper[T, V, O, TC]] =
      def applyExpr[T, V](
          fieldListExpr: Expr[List[DFTypeAny]],
          tokenTupleValuesExpr: Expr[List[Any]]
      )(using Quotes, Type[T], Type[V], Type[O], Type[TC]): Expr[List[O]] =
        import quotes.reflect.*
        val AppliedType(fun, tArgs) = TypeRepr.of[T]
        val AppliedType(_, vArgs) = TypeRepr.of[V]
        if (tArgs.length == vArgs.length)
          val exprs =
            tArgs.zipWithIndex.lazyZip(vArgs).map { case ((t, i), v) =>
              val vTpe = v.asTypeOf[Any]
              val dfTypeTpe = t.dfTypeTpe.get.asTypeOf[DFTypeAny]
              val iExpr = Literal(IntConstant(i)).asExprOf[Int]
              '{
                val tc = compiletime
                  .summonInline[
                    TC[dfTypeTpe.Underlying, vTpe.Underlying]
                  ]
                val dfType =
                  $fieldListExpr
                    .apply($iExpr)
                    .asInstanceOf[dfTypeTpe.Underlying]
                val value =
                  $tokenTupleValuesExpr
                    .apply($iExpr)
                    .asInstanceOf[vTpe.Underlying]
                tc.conv(dfType, value)
              }
            }
          '{ List(${ Varargs(exprs) }*) }
        else
          errorExpr(
            s"DFType tuple length (${tArgs.length}) and value tuple length (${vArgs.length}) do not match."
          )
        end if
      end applyExpr
      import quotes.reflect.*
      '{
        new TCZipper[T, V, O, TC]:
          def apply(
              fieldList: List[DFTypeAny],
              tokenTupleValues: List[Any]
          ): List[O] = ${
            applyExpr[T, V]('fieldList, 'tokenTupleValues)
          }
      }
    end zipperMacro
  end TCZipper

  type Token[+T] = DFToken[DFTuple[T]]
  object Token:
    protected[core] def apply[T](
        dfType: DFTuple[T],
        data: List[Any]
    ): Token[T] =
      ir.DFToken(dfType.asIR)(data).asTokenOf[DFTuple[T]]

    object TC:
      import DFToken.TC
      given DFTupleTokenFromTuple[
          T <: NonEmptyTuple,
          V <: NonEmptyTuple
      ](using
          zipper: TCZipper[T, V, DFTokenAny, TC]
      ): TC[DFTuple[T], ValueOf[V]] with
        def conv(dfType: DFTuple[T], value: ValueOf[V]): Out =
          DFTuple.Token[T](
            dfType,
            zipper(dfType.fieldList, value.value.toList).map(_.asIR.data)
          )
    end TC

    object Compare:
      import DFToken.Compare
      given DFTupleTokenFromTuple[
          T <: NonEmptyTuple,
          V <: NonEmptyTuple,
          Op <: FuncOp,
          C <: Boolean
      ](using
          zipper: TCZipper[T, V, DFTokenAny, [T <: DFTypeAny,
          R] =>> Compare[T, R, Op, C]]
      ): Compare[DFTuple[T], ValueOf[V], Op, C] with
        def conv(dfType: DFTuple[T], value: ValueOf[V]): Out =
          DFTuple.Token[T](
            dfType,
            zipper(dfType.fieldList, value.value.toList).map(_.asIR.data)
          )
    end Compare

    object Ops:
      import CompanionsDFBits.BitIndex
      extension [T <: NonEmptyTuple](t: DFToken[DFTuple[T]])
        def apply[I <: Int](i: Inlined[I])(using
            check: BitIndex.Check[I, Tuple.Size[T]],
            size: ValueOf[Tuple.Size[T]],
            tc: DFType.TC[Tuple.Elem[T, I]]
        ): DFToken[tc.Type] =
          check(i, size)
          selectRuntime[tc.Type](t.wide, i)
      private def selectRuntime[T <: DFTypeAny](
          token: Token[NonEmptyTuple],
          idx: Int
      ): DFToken[T] =
        val dfType = token.dfType.fieldList(idx).asIR
        val data = token.data(idx)
        ir.DFToken.forced(dfType, data).asTokenOf[T]
      extension [T <: NonEmptyTuple](t: DFToken[DFTuple[T]])
        // TODO: workaround compiler issue that inline does not obey covariance
        private def wide: Token[NonEmptyTuple] =
          t.asInstanceOf[Token[NonEmptyTuple]]

      extension [T1](t: Token[Tuple1[T1]])
        inline def _1(using tc: DFType.TC[T1]): DFToken[tc.Type] =
          selectRuntime[tc.Type](t.wide, 0)
      extension [T1, T2](t: Token[(T1, T2)])
        inline def _1(using tc: DFType.TC[T1]): DFToken[tc.Type] =
          selectRuntime[tc.Type](t.wide, 0)
        inline def _2(using tc: DFType.TC[T2]): DFToken[tc.Type] =
          selectRuntime[tc.Type](t.wide, 1)
      extension [T1, T2, T3](t: Token[(T1, T2, T3)])
        inline def _1(using tc: DFType.TC[T1]): DFToken[tc.Type] =
          selectRuntime[tc.Type](t.wide, 0)
        inline def _2(using tc: DFType.TC[T2]): DFToken[tc.Type] =
          selectRuntime[tc.Type](t.wide, 1)
        inline def _3(using tc: DFType.TC[T3]): DFToken[tc.Type] =
          selectRuntime[tc.Type](t.wide, 2)
    end Ops

  end Token

  object Val:
    object TC:
      import DFVal.TC
      given DFTupleArg[
          T <: NonEmptyTuple,
          R <: NonEmptyTuple
      ](using
          zipper: TCZipper[T, R, DFValAny, TC],
          dfc: DFC
      ): TC[DFTuple[T], ValueOf[R]] =
        new TC[DFTuple[T], ValueOf[R]]:
          type TType = DFTuple[T]
          def conv(dfType: DFTuple[T], value: ValueOf[R]): DFValOf[TType] =
            val dfVals =
              zipper(dfType.fieldList, value.value.toList)
            DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)

    end TC

    object Compare:
      import DFVal.Compare
      given DFTupleArg[
          T <: NonEmptyTuple,
          R <: NonEmptyTuple,
          Op <: FuncOp,
          C <: Boolean
      ](using
          zipper: TCZipper[T, R, DFValAny, [T <: DFTypeAny,
          R] =>> Compare[T, R, Op, C]],
          dfc: DFC
      ): Compare[DFTuple[T], ValueOf[R], Op, C] with
        def conv(dfType: DFTuple[T], value: ValueOf[R]): Out =
          val dfVals =
            zipper(dfType.fieldList, value.value.toList)
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
    end Compare

    object Ops:
      import CompanionsDFBits.BitIndex
      extension [T <: NonEmptyTuple, M <: Modifier](t: DFVal[DFTuple[T], M])
        def apply[I <: Int](i: Inlined[I])(using
            dfc: DFC,
            check: BitIndex.Check[I, Tuple.Size[T]],
            size: ValueOf[Tuple.Size[T]],
            tc: DFType.TC[Tuple.Elem[T, I]]
        ): DFVal[tc.Type, M] =
          check(i, size)
          applyForced[tc.Type](i)
        private def applyForced[OT <: DFTypeAny](i: Int)(using
            dfc: DFC
        ): DFVal[OT, M] =
          DFVal.Alias
            .SelectField(t.dfType.fieldList(i), t, i.toString())
            .asIR
            .asVal[OT, M]
      end extension
      extension [T1, M <: Modifier](t: DFVal[DFTuple[Tuple1[T1]], M])
        inline def _1(using tc: DFType.TC[T1], dfc: DFC): DFVal[tc.Type, M] =
          t.applyForced[tc.Type](0)
      extension [T1, T2, M <: Modifier](t: DFVal[DFTuple[(T1, T2)], M])
        inline def _1(using tc: DFType.TC[T1], dfc: DFC): DFVal[tc.Type, M] =
          t.applyForced[tc.Type](0)
        inline def _2(using tc: DFType.TC[T2], dfc: DFC): DFVal[tc.Type, M] =
          t.applyForced[tc.Type](1)
      extension [T1, T2, T3, M <: Modifier](t: DFVal[DFTuple[(T1, T2, T3)], M])
        inline def _1(using tc: DFType.TC[T1], dfc: DFC): DFVal[tc.Type, M] =
          t.applyForced[tc.Type](0)
        inline def _2(using tc: DFType.TC[T2], dfc: DFC): DFVal[tc.Type, M] =
          t.applyForced[tc.Type](1)
        inline def _3(using tc: DFType.TC[T3], dfc: DFC): DFVal[tc.Type, M] =
          t.applyForced[tc.Type](2)
    end Ops
  end Val
end DFTuple
