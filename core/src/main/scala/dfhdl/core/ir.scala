package dfhdl.core

object ir:
  sealed trait DFType
  sealed trait DFBoolOrBit extends DFType
  case object DFBit extends DFBoolOrBit
