package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import DFOpaque.Abstract as DFOpaqueA
import dfhdl.compiler.ir.MemberGetSet
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import scala.annotation.tailrec

import scala.reflect.ClassTag
class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal | DFError)
    extends // AnyVal with
    DFMember[ir.DFVal]
    with Selectable:

  transparent inline def ==[R](
      inline that: R
  )(using DFC): DFBool <> VAL = ???
  transparent inline def !=[R](
      inline that: R
  )(using DFC): DFBool <> VAL = ???
end DFVal

type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFVarAny = DFVal[DFTypeAny, Modifier[Modifier.Assignable, Modifier.Connectable, Any]]
type DFValOf[+T <: DFTypeAny] = DFVal[T, ModifierAny]
type DFVarOf[+T <: DFTypeAny] = DFVal[T, Modifier[Modifier.Assignable, Any, Any]]
type DFPortOf[+T <: DFTypeAny] = DFVal[T, Modifier.Port]

sealed trait TOKEN
type <>[T <: DFType.Supported | Int, M] = T match
  case DFType.Supported =>
    M match
      case VAL   => DFValOf[DFType.Of[T]]
      case TOKEN => DFToken[DFType.Of[T]]
  // Int is also special cased by the compiler plugin
  case Int => DFVector.ComposedModifier[T, M]

type X[T <: DFType.Supported, M] = M match
  case DFVector.ComposedModifier[d, m] => <>[DFVector[DFType.Of[T], Tuple1[d]], m]
  case Int                             => DFVector[DFType.Of[T], Tuple1[M]]
type JUSTVAL[T <: DFType.Supported] = <>[T, VAL]

extension (dfVal: ir.DFVal)
  inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] =
    DFVal[T, M](dfVal)
  inline def asValOf[T <: DFTypeAny]: DFValOf[T] =
    DFVal[T, ModifierAny](dfVal)
  inline def asValAny: DFValAny =
    DFVal[DFTypeAny, ModifierAny](dfVal)
  inline def asVarOf[T <: DFTypeAny]: DFVarOf[T] =
    DFVal[T, Modifier.VAR](dfVal)
  inline def asVarAny: DFVarAny =
    DFVal[DFTypeAny, Modifier.VAR](dfVal)
  inline def asPortOf[T <: DFTypeAny]: DFPortOf[T] =
    DFVal[T, Modifier.Port](dfVal)

object DFVal:
  inline def unapply(arg: DFValAny): Option[ir.DFVal] = Some(arg.asIR)
  object OrTupleOrStruct:
    def unapply(arg: Any)(using DFC): Option[DFValAny] =
      arg match
        case dfVal: DFValAny => Some(dfVal)
        case _               => None

  trait Refiner[T <: FieldsOrTuple, A, I]:
    type Out <: DFVal[DFStruct[T], Modifier[A, Any, I]]
  object Refiner:
    transparent inline given [T <: FieldsOrTuple, A, I]: Refiner[T, A, I] = ${
      refineMacro[T, A, I]
    }
    def refineMacro[T <: FieldsOrTuple, A, I](using
        Quotes,
        Type[T],
        Type[A],
        Type[I]
    ): Expr[Refiner[T, A, I]] =
      import quotes.reflect.*
      val dfValTpe = TypeRepr.of[DFVal[DFStruct[T], Modifier[A, Any, I]]]
      val tTpe = TypeRepr.of[T]
      val fields: List[(String, TypeRepr)] = tTpe.asTypeOf[Any] match
        case '[NonEmptyTuple] =>
          tTpe.getTupleArgs.zipWithIndex.map((f, i) =>
            f.asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (s"_${i + 1}", TypeRepr.of[DFVal[t, Modifier[A, Any, I]]])
          )
        case _ =>
          val clsSym = tTpe.classSymbol.get
          clsSym.caseFields.map(m =>
            tTpe.memberType(m).asTypeOf[Any] match
              case '[DFValOf[t]] =>
                (m.name.toString, TypeRepr.of[DFVal[t, Modifier[A, Any, I]]])
          )

      val refined = fields.foldLeft(dfValTpe) { case (r, (n, t)) =>
        Refinement(r, n, t)
      }
      val refinedType = refined.asTypeOf[DFVal[DFStruct[T], Modifier[A, Any, I]]]
      '{
        new Refiner[T, A, I]:
          type Out = refinedType.Underlying
      }
    end refineMacro
  end Refiner

  // Enabling equality with Int, Boolean, and Tuples.
  // just to give a better error message via the compiler plugin.
  // See the method `rejectBadPrimitiveOps` in `MetaContextGenPhase.scala`
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Int, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Boolean, DFVal[T, M]] =
    CanEqual.derived
  given [T <: DFTypeAny, M <: ModifierAny]: CanEqual[Tuple, DFVal[T, M]] =
    CanEqual.derived

  trait InitCheck[I]
  given [I](using
      initializableOnly: AssertGiven[
        I =:= Modifier.Initializable,
        "Can only initialize a dataflow port or variable that are not already initialized."
      ]
  ): InitCheck[I] with {}

  implicit def BooleanHack(from: DFValOf[DFBoolOrBit])(using DFC): Boolean =
    ???

  // opaque values need special conversion that does not try to summon the opaque dftype
  // because it can be abstract in extension methods that are applied generically on an abstract
  // opaque super-type. E.g.:
  // ```
  // abstract class MyAbsOpaque extends Opaque
  // case class MyOpaque extends MyAbsOpaque
  // extension (a : MyAbsOpaque <> VAL) def foo : Unit = {}
  // val a = MyOpaque <> VAR
  // a.foo //here we currently access `foo` through conversion to MyAbsOpaque
  //       //because DFOpaque is not completely covariant due to bug
  //       //https://github.com/lampepfl/dotty/issues/15704
  implicit def DFOpaqueValConversion[T <: DFOpaque.Abstract, R <: DFOpaque.Abstract](
      from: DFValOf[DFOpaque[R]]
  )(using DFC, R <:< T): DFValOf[DFOpaque[T]] = from.asInstanceOf[DFValOf[DFOpaque[T]]]

end DFVal
