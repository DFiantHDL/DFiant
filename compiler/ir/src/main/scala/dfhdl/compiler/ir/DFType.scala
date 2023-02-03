package dfhdl.compiler
package ir

sealed trait DFType
sealed trait DFBoolOrBit extends DFType
case object DFBit extends DFBoolOrBit
