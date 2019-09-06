/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import internals._

import scala.annotation.tailrec
import scala.collection.immutable

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Source Aggregator
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
case class SourceVersion()
private[DFiant] case class PropTag(init : Seq[DFAny.Token], const : DFAny.Token, latency : Option[Int])
private[DFiant] case class AliasTag(dfVal : DFAny, context : DFBlock, dfNet : Option[DFNet], version : Option[Int], prevStep : Int, inverted : Boolean, latency : Option[Int], pipeStep : Int) {
  def invert : AliasTag = copy(inverted = !inverted)
  def prev(step : Int) : AliasTag = copy(prevStep = prevStep + step)
  private def addPipeToLatency(p : Int) : Option[Int] = latency match {
    case Some(lat) => Some(lat + p)
    case None => None
  }
  @tailrec private def versioned(currentContext : DFBlock) : AliasTag =
    currentContext.netsTo.get(dfVal) match {
      case Some(x) => copy(version = Some(x.length), context = currentContext)
      case None => currentContext match {
        case x : ConditionalBlock[_] => versioned(x.owner)
        case _ => copy(version = Some(0), context = currentContext)
      }
    }
  final def versioned : AliasTag = if (dfVal.isAssignable) versioned(dfVal.owner.asInstanceOf[DFBlock]) else this
  def atContext(newContext : DFBlock) : AliasTag = copy(context = newContext)
  def atVersion(num : Int) : AliasTag = copy(version = Some(num))
  def via(viaNet : DFNet) : AliasTag = copy(dfNet = Some(viaNet))
  def pipe(step : Int) : AliasTag = copy(latency = addPipeToLatency(step), pipeStep = pipeStep + step)
  def balanceTo(maxLatency : Option[Int]) : AliasTag = (maxLatency, latency) match {
    case (Some(maxLat), Some(lat)) => pipe(maxLat - lat)
    case _ => this
  }
}
private[DFiant] object AliasTag {
  def apply(dfVal : DFAny, context : DFBlock) : AliasTag =
    AliasTag(dfVal = dfVal, context = context, dfNet = None, version = None, prevStep = 0, inverted = false, latency = None, pipeStep = 0)
  def withLatency(dfVal : DFAny, latency : Option[Int]) : AliasTag =
    AliasTag(dfVal = dfVal, context = dfVal.owner.asInstanceOf[DFBlock], dfNet = None, version = None, prevStep = 0, inverted = false, latency = latency, pipeStep = 0)
}
private[DFiant] case class SourceElement(relBitHigh: Int, relBitLow : Int, reverseBits : Boolean, aliasTag : Option[AliasTag]) {
  val relWidth : Int = relBitHigh - relBitLow + 1
  def range : Range = if (reverseBits) relBitLow to relBitHigh else relBitHigh to relBitLow by -1
  def reverse : SourceElement = copy(reverseBits = !reverseBits)
  def invert : SourceElement = copy(aliasTag = aliasTag.map(t => t.invert))
  def prev(step : Int) : SourceElement = copy(aliasTag = aliasTag.map(t => t.prev(step)))
  def pipe(step : Int) : SourceElement = copy(aliasTag = aliasTag.map(t => t.pipe(step)))
  def via(viaNet : DFNet) : SourceElement = copy(aliasTag = aliasTag.map(t => t.via(viaNet)))
  def connectionsOnly : SourceElement = aliasTag match {
    case Some(AliasTag(_,_,Some(DFNet.Connection(_,_)),_,_,_,_,_)) => this
    case _ => copy(aliasTag = None)
  }
  def assignmentsOnly : SourceElement = aliasTag match {
    case Some(AliasTag(_,_,Some(DFNet.Assignment(_,_)),_,_,_,_,_)) => this
    case _ => copy(aliasTag = None)
  }
  def balanceTo(maxLatency : Option[Int]) : SourceElement = copy(aliasTag = aliasTag.map(t => t.balanceTo(maxLatency)))
  def versioned : SourceElement = {
    val versionedTag = aliasTag.map(t => t.versioned)
    versionedTag.foreach {
      case v if v.version.isDefined => v.dfVal.consumeAt(relWidth, relBitLow, v.version.get, v.context)
      case _ =>
    }
    copy(aliasTag = versionedTag)
  }
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = aliasTag match {
    case Some(t) =>
      val reverseStr = if (reverseBits) ".reverse" else ""
      val invertStr = if (t.inverted) "~" else ""
      val prevStr = if (t.prevStep == 1) s".prev" else if (t.prevStep > 0) s".prev(${t.prevStep})" else ""
      val selStr = if (t.dfVal.width.getValue != relWidth) s"($relBitHigh, $relBitLow)" else ""
      val pipeStr = if (t.pipeStep == 1) s".pipe" else if (t.pipeStep > 0) s".pipe(${t.pipeStep})" else ""
      s"$invertStr${t.dfVal.refCodeString}$selStr$prevStr$pipeStr$reverseStr"
    case None => "NA"
  }
  def latencyString : String = aliasTag match {
    case Some(AliasTag(_,_,_,_,_,_,Some(lat),_)) => lat.toString
    case _ => s"NA"
  }


  override def toString: String = aliasTag match {
    case Some(t) =>
      val ref = t.dfVal match {
        case x : DFAny.Const[_] => s"CONST_${x.token}"
        case _ => t.dfVal.fullName
      }
      val reverseStr = if (reverseBits) ".reverse" else ""
      val invertStr = if (t.inverted) ".invert" else ""
      val prevStr = if (t.prevStep > 0) s".prev(${t.prevStep})" else ""
      val versionStr = if (t.prevStep == 0) t.version.map(i => s"@V$i").getOrElse("") else ""
      s"$ref($relBitHigh, $relBitLow)$prevStr$reverseStr$invertStr$versionStr"
    case None => s"None($relBitHigh, $relBitLow)"
  }
}

private[DFiant] case class Source(elements : List[SourceElement]) {
  val width : Int = elements.map(v => v.relWidth).sum
  def coalesce : Source = Source(elements.foldLeft(List[SourceElement]()) {
    case (ls, e) if ls.isEmpty || (ls.last.aliasTag != e.aliasTag)=> ls :+ e
    case (ls, right) =>
      val left = ls.last
      val coupled : List[SourceElement] =
        if (left.aliasTag.isEmpty && right.aliasTag.isEmpty)
          List(SourceElement(left.relWidth + right.relWidth - 1, 0, reverseBits = false, None))
        else if (left.relBitLow == right.relBitHigh + 1 && ((!left.reverseBits && !right.reverseBits) || right.relWidth == 1))
          List(SourceElement(left.relBitHigh, right.relBitLow, left.reverseBits, left.aliasTag))
        else if (left.relBitHigh == right.relBitLow - 1 && ((left.reverseBits && right.reverseBits) || right.relWidth == 1))
          List(SourceElement(right.relBitHigh, left.relBitLow, left.reverseBits, left.aliasTag))
        else List(left, right)
      ls.dropRight(1) ++ coupled
  })
  def separate : Source = Source(elements.foldLeft(List[SourceElement]()) {
    case (ls, e) => ls ++ e.range.toList.map(i => SourceElement(i, i, e.reverseBits, e.aliasTag))
  })
  private def reverseIndex(idx : Int) : Int = width-1-idx
  def bitsWL(relWidth : Int, relBitLow : Int) : Source =
    if (relWidth == width) {assert(relBitLow == 0); this}
    else Source(separate.elements.slice(reverseIndex(relBitLow + relWidth-1), reverseIndex(relBitLow-1))).coalesce
  def bitsHL(relBitHigh : Int, relBitLow : Int) : Source = bitsWL(relBitHigh - relBitLow + 1, relBitLow)
  def replaceWL(relWidth : Int, relBitLow : Int, thatSource : Source) : Source =
    if (relWidth == width) {assert(relBitLow == 0); thatSource}
    else {
      val elms = separate.elements
      val left = elms.take(reverseIndex(relBitLow + relWidth-1))
      val right = elms.takeRight(relBitLow)
      assert(width - left.length - right.length == thatSource.width, s"$width - ${left.length} - ${right.length} != ${thatSource.width}")
      Source(left ++ thatSource.elements ++ right).coalesce
    }
  def replaceHL(relBitHigh : Int, relBitLow : Int, thatSource : Source) : Source =
    replaceWL(relBitHigh - relBitLow + 1, relBitLow, thatSource)
  def reverse : Source = Source(elements.reverse.map(e => e.reverse))
  def reverse(cond : Boolean) : Source = if (cond) reverse else this
  def invert : Source = Source(elements.map(e => e.invert))
  def invert(cond : Boolean) : Source = if (cond) invert else this
  def prev(step : Int) : Source = Source(elements.map(e => e.prev(step)))
  def pipe(step : Int) : Source = Source(elements.map(e => e.pipe(step)))
  def via(viaNet : DFNet) : Source = Source(elements.map(e => e.via(viaNet)))
  def connectionsOnly : Source = Source(elements.map(e => e.connectionsOnly)).coalesce
  def assignmentsOnly : Source = Source(elements.map(e => e.assignmentsOnly)).coalesce
  def resize(toWidth : Int) : Source =
    if (toWidth > width) {
      Source(List.fill(toWidth - width)(bitsWL(1, width-1).elements.head) ++ elements)
    } else if (toWidth < width) {
      bitsWL(toWidth, 0)
    } else {
      this
    }
  def versioned : Source = Source(elements.map(e => e.versioned))

  def getMaxLatency : Option[Int] = {
    val list = elements.flatMap(e => e.aliasTag).flatMap(t => t.latency)
    if (list.isEmpty) None else Some(list.max)
  }
  def balanceTo(maxLatency : Option[Int]) : Source = Source(elements.map(e => e.balanceTo(maxLatency))).coalesce
  def balance : Source = balanceTo(getMaxLatency)
  def ## (that : Source) : Source = Source(this.elements ++ that.elements).coalesce
  def copyWithNewDFVal(thatDFVal : DFAny) : Source = {
    assert(thatDFVal.width.getValue == width)
    var pos = width - 1
    Source(elements.map(e => {
      val thatTag : Option[AliasTag] = e.aliasTag match {
        case Some(t) => Some(AliasTag.withLatency(thatDFVal, t.latency))
        case None => None
      }
      val se = SourceElement(pos, pos-e.relWidth+1, false, thatTag)
      pos -= e.relWidth
      se
    })).coalesce
  }

  def orElse (that : Source) : Source =
    Source(this.separate.elements.zip(that.separate.elements).collect {
      case (left, right) => if (left.aliasTag.isDefined) left else right
    }).coalesce
  def isEmpty : Boolean = elements.length == 1 && elements.head.aliasTag.isEmpty
  def nonEmptyAtWL(relWidth : Int, relBitLow : Int) : Boolean = !bitsWL(relWidth, relBitLow).isEmpty
  def nonEmptyAtHL(relBitHigh : Int, relBitLow : Int) : Boolean = nonEmptyAtWL(relBitHigh - relBitLow + 1, relBitLow)
  def isCompletelyAllocated : Boolean = !elements.map(e => e.aliasTag.isEmpty).reduce((l, r) => l | r)
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String =
    if (elements.length > 1) elements.map(e => e.refCodeString).mkString("(", ", ", ")") else elements.head.refCodeString
  def latencyString : String = {
    val cf = elements.collectFirst{case SourceElement(_,_,_,Some(t)) => t.dfVal}
    if (cf.isEmpty) "NA"
    else {
      val coalesedLatency = Source(separate.elements.zipWithIndex.collect { case (e, i) =>
        SourceElement(reverseIndex(i), reverseIndex(i), false, Some(AliasTag.withLatency(cf.get, if (e.aliasTag.isDefined) e.aliasTag.get.latency else None)))
      }).coalesce
      var pos = width - 1
      coalesedLatency.elements.map(e => {
        val high = pos
        pos -= e.relWidth
        val low = pos + 1
        if (high - low + 1 == width) e.latencyString else s"${e.latencyString}@($high, $low)"
      }).mkString(", ")
    }
  }
  def toUsedBitSet : immutable.BitSet = {
    var bitHi = width-1
    elements.foldLeft(immutable.BitSet()){
      case (usedBits, e) =>
        val extra = e.aliasTag match {
          case Some(t) => usedBits ++ ((bitHi-e.relWidth+1) to bitHi)
          case None => usedBits
        }
        bitHi = bitHi - e.relWidth
        extra
    }
  }
  override def toString: String = elements.mkString(" ## ")
}
object Source {
  def apply(value : DFAny, context : DFBlock) : Source = Source(List(SourceElement(value.width-1, 0, reverseBits = false, Some(AliasTag(value, context)))))
  def withLatency(value : DFAny, latency : Option[Int]) : Source = Source(List(SourceElement(value.width-1, 0, reverseBits = false, Some(AliasTag.withLatency(value, latency)))))
  def zeroLatency(value : DFAny) : Source = withLatency(value, Some(0))
  def none(width : Int) : Source = Source(List(SourceElement(width-1, 0, reverseBits = false, None)))
//  def selfPrevAssignment(dfVal : DFAny) : Source =
//    Source(List(SourceElement(dfVal.width-1, 0, reverseBits = false,
//      Some(AliasTag(dfVal = dfVal, context = dfVal.owner.asInstanceOf[DFBlock], version = Some(0), prevStep = 1, inverted = false, latency = None, pipeStep = 0)))))
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////



