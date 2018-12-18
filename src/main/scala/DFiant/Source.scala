package DFiant

import internals._

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Source Aggregator
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
private[DFiant] case class SourceTag(dfVal : DFAny, prevStep : Int, inverted : Boolean, latency : Option[Int], pipeStep : Int) {
  def invert : SourceTag = SourceTag(dfVal, prevStep, !inverted, latency, pipeStep)
  def prev(step : Int) : SourceTag = SourceTag(dfVal, prevStep + step, inverted, latency, pipeStep)
  private def addPipeToLatency(p : Int) : Option[Int] = latency match {
    case Some(lat) => Some(lat + p)
    case None => None
  }
  def pipe(step : Int) : SourceTag = SourceTag(dfVal, prevStep, inverted, addPipeToLatency(step), pipeStep + step)
  def balanceTo(maxLatency : Option[Int]) : SourceTag = (maxLatency, latency) match {
    case (Some(maxLat), Some(lat)) => pipe(maxLat - lat)
    case _ => this
  }

  override def equals(that: Any): Boolean = that match {
    case SourceTag(dfVal2, prevStep2, inverted2, latency2, pipeStep2) =>
      dfVal.fullName == dfVal2.fullName && prevStep == prevStep2 && inverted == inverted2 &&
        latency == latency2 && pipeStep == pipeStep2
    case _ =>
      false
  }
}
private[DFiant] object SourceTag {
  def apply(dfVal : DFAny) : SourceTag = SourceTag(dfVal, prevStep = 0, inverted = false, latency = None, pipeStep = 0)
  def withLatency(dfVal : DFAny, latency : Option[Int]) : SourceTag = SourceTag(dfVal, prevStep = 0, inverted = false, latency = latency, pipeStep = 0)
}
private[DFiant] case class SourceElement(relBitHigh: Int, relBitLow : Int, reverseBits : Boolean, tag : Option[SourceTag]) {
  val relWidth : Int = relBitHigh - relBitLow + 1
  def range : Range = if (reverseBits) relBitLow to relBitHigh else relBitHigh to relBitLow by -1
  def reverse : SourceElement = SourceElement(relBitHigh, relBitLow, !reverseBits, tag)
  def invert : SourceElement = SourceElement(relBitHigh, relBitLow, reverseBits, tag.map(t => t.invert))
  def prev(step : Int) : SourceElement = SourceElement(relBitHigh, relBitLow, reverseBits, tag.map(t => t.prev(step)))
  def pipe(step : Int) : SourceElement = SourceElement(relBitHigh, relBitLow, reverseBits, tag.map(t => t.pipe(step)))
  def balanceTo(maxLatency : Option[Int]) : SourceElement =
    SourceElement(relBitHigh, relBitLow, reverseBits, tag.map(t => t.balanceTo(maxLatency)))

  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = tag match {
    case Some(t) =>
      val reverseStr = if (reverseBits) ".reverse" else ""
      val invertStr = if (t.inverted) "~" else ""
      val prevStr = if (t.prevStep == 1) s".prev" else if (t.prevStep > 0) s".prev(${t.prevStep})" else ""
      val selStr = if (t.dfVal.width.getValue != relWidth) s"($relBitHigh, $relBitLow)" else ""
      val pipeStr = if (t.pipeStep == 1) s".pipe" else if (t.pipeStep > 0) s".pipe(${t.pipeStep})" else ""
      s"$invertStr${t.dfVal.refCodeString}$selStr$prevStr$pipeStr$reverseStr"
    case None => "NA"
  }
  def latencyString : String = tag match {
    case Some(SourceTag(_,_,_,Some(lat),_)) => lat.toString
    case _ => s"NA"
  }


  override def toString: String = tag match {
    case Some(t) =>
      val reverseStr = if (reverseBits) ".reverse" else ""
      val invertStr = if (t.inverted) ".invert" else ""
      val prevStr = if (t.prevStep > 0) s".prev(${t.prevStep})" else ""
      s"${t.dfVal.fullName}($relBitHigh, $relBitLow)$prevStr$reverseStr$invertStr"
    case None => s"None($relBitHigh, $relBitLow)"
  }
}

private[DFiant] case class Source(elements : List[SourceElement]) {
  val width : Int = elements.map(v => v.relWidth).sum
  def coalesce : Source = Source(elements.foldLeft(List[SourceElement]()) {
    case (ls, e) if ls.isEmpty || (ls.last.tag != e.tag)=> ls :+ e
    case (ls, right) =>
      val left = ls.last
      val coupled : List[SourceElement] =
        if (left.relBitLow == right.relBitHigh + 1 && ((!left.reverseBits && !right.reverseBits) || right.relWidth == 1))
          List(SourceElement(left.relBitHigh, right.relBitLow, left.reverseBits, left.tag))
        else if (left.relBitHigh == right.relBitLow - 1 && ((left.reverseBits && right.reverseBits) || right.relWidth == 1))
          List(SourceElement(right.relBitHigh, left.relBitLow, left.reverseBits, left.tag))
        else List(left, right)
      ls.dropRight(1) ++ coupled
  })
  def separate : Source = Source(elements.foldLeft(List[SourceElement]()) {
    case (ls, e) => ls ++ e.range.toList.map(i => SourceElement(i, i, e.reverseBits, e.tag))
  })
  private def reverseIndex(idx : Int) : Int = width-1-idx
  def bitsWL(relWidth : Int, relBitLow : Int) : Source =
    Source(separate.elements.slice(reverseIndex(relBitLow + relWidth-1), reverseIndex(relBitLow-1))).coalesce
  def replaceWL(relWidth : Int, relBitLow : Int, thatSource : Source) : Source = {
    val elms = separate.elements
    val left = elms.take(reverseIndex(relBitLow + relWidth-1))
    val right = elms.takeRight(relBitLow)
    assert(width - left.length - right.length == thatSource.width, s"$width - ${left.length} - ${right.length} != ${thatSource.width}")
    Source(left ++ thatSource.elements ++ right).coalesce
  }
  def reverse : Source = Source(elements.reverse.map(e => e.reverse))
  def invert : Source = Source(elements.map(e => e.invert))
  def prev(step : Int) : Source = Source(elements.map(e => e.prev(step)))
  def pipe(step : Int) : Source = Source(elements.map(e => e.pipe(step)))
  def getMaxLatency : Option[Int] = {
    val list = elements.flatMap(e => e.tag).flatMap(t => t.latency)
    if (list.isEmpty) None else Some(list.max)
  }
  def balanceTo(maxLatency : Option[Int]) : Source = Source(elements.map(e => e.balanceTo(maxLatency))).coalesce
  def balance : Source = balanceTo(getMaxLatency)
  def ## (that : Source) : Source = Source(this.elements ++ that.elements).coalesce
  def copyWithNewDFVal(thatDFVal : DFAny) : Source = {
    assert(thatDFVal.width.getValue == width)
    var pos = width - 1
    Source(elements.map(e => {
      val thatTag : Option[SourceTag] = e.tag match {
        case Some(t) =>
          if (t.pipeStep > 0) t.dfVal.maxPrevUse = scala.math.max(t.dfVal.maxPrevUse, t.pipeStep)
          Some(SourceTag.withLatency(thatDFVal, t.latency))
        case None => None
      }
      val se = SourceElement(pos, pos-e.relWidth+1, false, thatTag)
      pos -= e.relWidth
      se
    })).coalesce
  }

  def orElse (that : Source) : Source =
    Source(this.separate.elements.zip(that.separate.elements).collect {
      case (left, right) => if (left.tag.isDefined) left else right
    }).coalesce
  def isEmpty : Boolean = elements.length == 1 && elements.head.tag.isEmpty
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String =
    if (elements.length > 1) elements.map(e => e.refCodeString).mkString("(", ", ", ")") else elements.head.refCodeString
  def latencyString : String = {
    val cf = elements.collectFirst{case SourceElement(_,_,_,Some(SourceTag(d, _,_,_,_))) => d}
    if (cf.isEmpty) "NA"
    else {
      val coalesedLatency = Source(separate.elements.zipWithIndex.collect { case (e, i) =>
        SourceElement(reverseIndex(i), reverseIndex(i), false, Some(SourceTag.withLatency(cf.get, if (e.tag.isDefined) e.tag.get.latency else None)))
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
  override def toString: String = elements.mkString(" ## ")
}
object Source {
  def apply(value : DFAny) : Source = Source(List(SourceElement(value.width-1, 0, reverseBits = false, Some(SourceTag(value)))))
  def withLatency(value : DFAny, latency : Option[Int]) : Source = Source(List(SourceElement(value.width-1, 0, reverseBits = false, Some(SourceTag.withLatency(value, latency)))))
  def zeroLatency(value : DFAny) : Source = withLatency(value, Some(0))
  def none(width : Int) : Source = Source(List(SourceElement(width-1, 0, reverseBits = false, None)))
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////



