package dfhdl.compiler
package ir
import dfhdl.internals.*

import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag

sealed trait DFType extends Product, Serializable derives CanEqual:
  type Data
  val width: Int
  def createBubbleData: Data
  def isDataBubble(data: Data): Boolean
  def dataToBitsData(data: Data): (BitVector, BitVector)
  def bitsDataToData(data: (BitVector, BitVector)): Data

object DFType:
  type Aux[T <: DFType, Data0] = DFType { type Data = Data0 }

  protected[ir] abstract class Companion[T <: DFType, D](using ClassTag[T]):
  end Companion
end DFType

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  type Data = Option[Boolean]
  final val width = 1
  def createBubbleData: Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data): (BitVector, BitVector) = data match
    case Some(value) => (BitVector.bit(value), BitVector.low(1))
    case None        => (BitVector.low(1), BitVector.high(1))
  def bitsDataToData(data: (BitVector, BitVector)): Data =
    if (data._2.isZeros) Some(!data._1.isZeros)
    else None

object DFBoolOrBit extends DFType.Companion[DFBoolOrBit, Option[Boolean]]

case object DFBit extends DFBoolOrBit
/////////////////////////////////////////////////////////////////////////////
