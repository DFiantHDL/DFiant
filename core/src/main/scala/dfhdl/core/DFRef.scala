package dfhdl.core
import dfhdl.compiler.ir

import scala.annotation.targetName
import scala.reflect.{ClassTag, classTag}
extension [M <: ir.DFMember](member: M)
  def ref(using ClassTag[M]): ir.DFRef.OneWay[M] = ???
  def refTW[O <: ir.DFMember](
      originMember: => O
  )(using ClassTag[M], ClassTag[O]): ir.DFRef.TwoWay[M, O] = ???

extension [T <: ir.DFOwner](owner: DFOwner[T]) def ref(using ClassTag[T]): ir.DFRef.OneWay[T] = ???
