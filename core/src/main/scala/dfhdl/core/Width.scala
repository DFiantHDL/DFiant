package dfhdl
package core
import internals.*
import scala.quoted.*
import compiler.ir
import annotation.targetName

trait Width[T]:
  type Out <: Int
object Width:
  val wide: Width[DFTypeAny] = new Width[DFTypeAny]:
    type Out = Int
  given fromDFBoolOrBit[T <: DFBoolOrBit]: Width[T] with
    type Out = 1
  given fromBooleanCompanion: Width[Boolean.type] with
    type Out = 1
  given fromDFBits[W <: Int]: Width[DFBits[W]] with
    type Out = W
  transparent inline given [T]: Width[T] = ${ getWidthMacro[T] }
  extension (using quotes: Quotes)(dfTpe: quotes.reflect.TypeRepr)
    def +(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      (dfTpe, rhs) match
        case (ConstantType(IntConstant(l)), ConstantType(IntConstant(r))) =>
          ConstantType(IntConstant(l + r))
        case (l, r) if l =:= TypeRepr.of[Int] || r =:= TypeRepr.of[Int] =>
          TypeRepr.of[Int]
        case _ =>
          TypeRepr.of[scala.compiletime.ops.int.+].appliedTo(List(dfTpe, rhs))
    def *(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      (dfTpe, rhs) match
        case (ConstantType(IntConstant(l)), ConstantType(IntConstant(r))) =>
          ConstantType(IntConstant(l * r))
        case (l, r) if l =:= TypeRepr.of[Int] || r =:= TypeRepr.of[Int] =>
          TypeRepr.of[Int]
        case _ =>
          TypeRepr.of[scala.compiletime.ops.int.`*`].appliedTo(List(dfTpe, rhs))
    def max(rhs: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      (dfTpe, rhs) match
        case (ConstantType(IntConstant(l)), ConstantType(IntConstant(r))) =>
          ConstantType(IntConstant(l max r))
        case (l, r) if l =:= TypeRepr.of[Int] || r =:= TypeRepr.of[Int] =>
          TypeRepr.of[Int]
        case _ =>
          TypeRepr.of[scala.compiletime.ops.int.Max].appliedTo(List(dfTpe, rhs))
    def simplify: quotes.reflect.TypeRepr =
      import quotes.reflect.*
      dfTpe match
        case _: TermRef => TypeRepr.of[Int]
        case _          => dfTpe
    def calcWidth: quotes.reflect.TypeRepr =
      import quotes.reflect.*
      dfTpe.asTypeOf[Any] match
        case '[DFTypeAny] =>
          dfTpe.asTypeOf[DFTypeAny] match
            case '[DFBoolOrBit] =>
              ConstantType(IntConstant(1))
            case '[DFBits[w]] =>
              TypeRepr.of[w].calcWidth
            case _ =>
              TypeRepr.of[Int]
          end match
        case '[Int] =>
          dfTpe
        case '[Boolean.type] => ConstantType(IntConstant(1))
        case '[Byte.type]    => ConstantType(IntConstant(8))
        case '[Int.type]     => ConstantType(IntConstant(32))
        case '[Long.type]    => ConstantType(IntConstant(64))
        case '[NonEmptyTuple] =>
          val widths =
            dfTpe.getTupleArgs.map(a => a.calcWidth)
          widths.reduce(_ + _)
        case _ =>
          dfTpe match
            case OrType(left, right) =>
              left.calcWidth max right.calcWidth
            case compObjTpe =>
              val compPrefix = compObjTpe match
                case TermRef(pre, _) => pre
                case _ =>
                  report.errorAndAbort("Case class companion must be a term ref")
              val clsSym = compObjTpe.typeSymbol.companionClass
              if !clsSym.paramSymss.forall(_.headOption.forall(_.isTerm)) then
                report.errorAndAbort(
                  "Case class with type parameters are not supported"
                )
              compPrefix.select(clsSym).calcWidth

          end match
      end match
    end calcWidth
    def calcValWidth(onlyTokens: Boolean): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      dfTpe.asType match
        case '[DFToken[t]] =>
          TypeRepr.of[t].calcWidth
        case '[NonEmptyTuple] =>
          val args = dfTpe.getTupleArgs
          val widths = args.map(a => a.calcValWidth(onlyTokens))
          widths.reduce(_ + _)
        case _ =>
          dfTpe.dealias match
            case ConstantType(IntConstant(v)) if (v == 1 || v == 0) =>
              ConstantType(IntConstant(1))
            case ref: TermRef =>
              ref.widen.calcValWidth(onlyTokens)
            case x =>
              report.errorAndAbort(
                s"Unsupported argument value ${x.showType} for dataflow receiver type DFBits"
              )
      end match
    end calcValWidth
  end extension
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
//    println(tTpe.show)
    val widthTpe = tTpe.calcWidth.asTypeOf[Int]
    '{
      new Width[T]:
        type Out = widthTpe.Underlying
    }
end Width

extension [T <: DFTypeAny](token: DFToken[T])
  @targetName("tokenWidth")
  def width(using w: Width[T]): Inlined[w.Out] =
    Inlined.forced[w.Out](token.asIR.width)

extension [T](t: T)(using tc: DFType.TC[T])
  @targetName("tWidth")
  def width(using w: Width[tc.Type]): Inlined[w.Out] =
    Inlined.forced[w.Out](tc(t).asIR.width)
