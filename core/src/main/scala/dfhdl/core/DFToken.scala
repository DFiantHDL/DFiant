package dfhdl.core
import dfhdl.compiler.ir
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

  protected[core] def bubble[T <: DFTypeAny](dfType: T): DFToken[T] = ???
  extension (token: DFTokenAny)
    def asIR: ir.DFTokenAny = token.value match
      case tokenIR: ir.DFTokenAny => tokenIR
      case err: DFError           => throw DFError.Derived(err)
  extension [T <: ir.DFType, Data](
      token: DFToken[DFType[ir.DFType.Aux[T, Data], Args]]
  ) def data: Data = token.asIR.data.asInstanceOf[Data]
end DFToken
