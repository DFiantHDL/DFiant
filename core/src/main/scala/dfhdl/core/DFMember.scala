package dfhdl.core
import dfhdl.compiler.ir
import scala.reflect.ClassTag
import dfhdl.internals.*

// TODO: return AnyVal workaround after https://github.com/lampepfl/dotty/issues/14340
trait DFMember[+T <: ir.DFMember]: // extends Any:
  val irValue: T | DFError
  override def toString: String = irValue.toString

type DFMemberAny = DFMember[ir.DFMember]
object DFMember:
  extension [T <: ir.DFMember](member: DFMember[T])
    def asIR: T = member.irValue match
      case memberIR: T @unchecked => memberIR
      case err: DFError           => throw DFError.Derived(err)
  extension [M <: DFMemberAny](member: M)
    @metaContextDelegate
    def anonymize: M = ???
