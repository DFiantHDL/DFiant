package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
type DFBits[W <: Int] = DFType[ir.DFBits, Args1[W]]

object DFBits:
  def apply[W <: Int](width: Inlined[W])(using
      check: Arg.Width.Check[W]
  ): DFBits[W] =
    check(width)
    ir.DFBits(width).asFE[DFBits[W]]
  @targetName("applyNoArg")
  def apply[W <: Int with Singleton](using ValueOf[W])(using
      Arg.Width.Check[W]
  ): DFBits[W] =
    DFBits[W](Inlined.forced[W](valueOf[W]))

  type Token[W <: Int] = DFToken[DFBits[W]]
end DFBits
