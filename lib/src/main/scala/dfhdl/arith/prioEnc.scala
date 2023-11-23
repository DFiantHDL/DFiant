package dfhdl.lib.arith

import dfhdl.*
import core.asValOf
import internals.IntInfo

def prioEncRecur(value: Bits[Int] <> VAL): Bits[Int] <> DFRET =
  val width = value.width
  if (width == 1) value
  else
    val lPrio = prioEncRecur(value.msbits(width / 2))
    val rPrio = prioEncRecur(value.lsbits(width - width / 2))
    val selPrio = if (value.msbit) lPrio else rPrio
    (value.msbit, selPrio)

@inline def prioEnc[W <: Int](value: Bits[W] <> VAL)(using
    info: IntInfo[W]
): Bits[info.OutW] <> DFRET =
  val ret = prioEncRecur(value)
  ret.asValOf[Bits[info.OutW]]
