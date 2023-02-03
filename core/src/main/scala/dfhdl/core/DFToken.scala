package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.quoted.*
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

final class DFToken[+T <: DFTypeAny](val value: ir.DFTokenAny | DFError)
    extends AnyVal
    with Selectable:
  override def toString: String = value.toString
end DFToken

type DFTokenAny = DFToken[DFTypeAny]
extension (tokenIR: ir.DFTokenAny) def asTokenOf[T <: DFTypeAny]: DFToken[T] = DFToken[T](tokenIR)

object DFToken:
  trait Refiner[T <: FieldsOrTuple]:
    type Out <: DFToken[DFStruct[T]]
  object Refiner:
    transparent inline given [T <: FieldsOrTuple]: Refiner[T] = ${
      refineMacro[T]
    }
    def refineMacro[T <: FieldsOrTuple](using
        Quotes,
        Type[T]
    ): Expr[Refiner[T]] =
      import quotes.reflect.*
      val tokenTpe = TypeRepr.of[DFToken[DFStruct[T]]]
      val tTpe = TypeRepr.of[T]
      val fields: List[(String, TypeRepr)] = tTpe.asTypeOf[Any] match
        case '[NonEmptyTuple] =>
          tTpe.getTupleArgs.zipWithIndex.map((f, i) =>
            f.asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (s"_${i + 1}", TypeRepr.of[DFToken[t]])
          )
        case _ => ???

      val refined = fields.foldLeft(tokenTpe) { case (r, (n, t)) =>
        Refinement(r, n, t)
      }
      val refinedType = refined.asTypeOf[DFToken[DFStruct[T]]]
      '{
        new Refiner[T]:
          type Out = refinedType.Underlying
      }
    end refineMacro
  end Refiner

  // implicit conversion to allow a boolean/bit token to be a Scala Boolean
  implicit def fromDFBoolOrBitToken(from: DFToken[DFBoolOrBit]): Boolean =
    from.asIR.data.asInstanceOf[ir.DFBool.Data].get

  // Enabling equality with Int, Boolean, and Tuples.
  // just to give a better error message via the compiler plugin.
  // See the method `rejectBadPrimitiveOps` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny]: CanEqual[Int, DFToken[T]] =
    CanEqual.derived
  given [T <: DFTypeAny]: CanEqual[Boolean, DFToken[T]] =
    CanEqual.derived
  given [T <: DFTypeAny]: CanEqual[Tuple, DFToken[T]] =
    CanEqual.derived

  protected[core] def bubble[T <: DFTypeAny](dfType: T): DFToken[T] =
    ir.DFToken.bubble(dfType.asIR).asTokenOf[T]
  extension (token: DFTokenAny)
    def asIR: ir.DFTokenAny = token.value match
      case tokenIR: ir.DFTokenAny => tokenIR
      case err: DFError           => throw DFError.Derived(err)
  extension [T <: ir.DFType, Data](
      token: DFToken[DFType[ir.DFType.Aux[T, Data], Args]]
  ) def data: Data = token.asIR.data.asInstanceOf[Data]
end DFToken
