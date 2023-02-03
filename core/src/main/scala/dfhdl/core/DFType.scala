package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args2[T1, T2] extends Args
sealed trait Args3[T1, T2, T3] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T | DFError) extends AnyVal:
  override def toString: String = value.toString
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:

  extension [T <: ir.DFType, A <: Args](dfType: DFType[T, A])
    def asIR: T = dfType.value match
      case dfTypeIR: T @unchecked => dfTypeIR
      case err: DFError           => throw DFError.Derived(err)
  extension (dfType: ir.DFType) def asFE[T <: DFTypeAny]: T = new DFType(dfType).asInstanceOf[T]
  export DFBoolOrBit.given

  trait TC[T]:
    type Type <: DFTypeAny
    def apply(t: T): Type
  object TC:
    transparent inline given ofDFType[T <: DFTypeAny]: TC[T] = new TC[T]:
      type Type = T
      def apply(t: T): Type = t
  end TC
end DFType
