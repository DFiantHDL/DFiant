package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.quoted.*
import scala.annotation.targetName

type DFDecimal[S <: Boolean, W <: Int, F <: Int] =
  DFType[ir.DFDecimal, Args3[S, W, F]]
object DFDecimal:
  protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
      signed: Inlined[S],
      width: Inlined[W],
      fractionWidth: Inlined[F]
  ): DFDecimal[S, W, F] = ???

  type Token[S <: Boolean, W <: Int, F <: Int] = DFToken[DFDecimal[S, W, F]]
end DFDecimal
