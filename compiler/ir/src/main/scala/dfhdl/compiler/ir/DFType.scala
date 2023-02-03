package dfhdl.compiler
package ir
import dfhdl.internals.*

import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag

sealed trait DFType extends Product, Serializable derives CanEqual
object DFType

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType

object DFBoolOrBit

case object DFBit extends DFBoolOrBit
/////////////////////////////////////////////////////////////////////////////
