package DFiant.internals

import scala.annotation.tailrec
import scala.collection.mutable

sealed class CacheBoxRO[+T](updateFunc : => T) {
  private val deps : mutable.HashSet[CacheBoxRO[_]] = mutable.HashSet()
  protected[this] var value : Option[T] = None
  @inline final protected def valueIsEmpty : Boolean = value.isEmpty
  @inline final protected def valueClear() : Unit = value = None
  @inline final protected def valueUpdate() : Unit = value = Some(updateFunc)
  @inline protected def emptyValueUpdate() : Unit = if (value.isEmpty) updateSrcValues()
  @tailrec private def dirty(current : CacheBoxRO[_], remainingDeps : List[CacheBoxRO[_]]) : Unit = {
    val updatedDeps = if (!current.valueIsEmpty) {
      current.valueClear()
      remainingDeps ++ current.deps
    } else remainingDeps
    updatedDeps match {
      case Nil =>
      case x :: xs => dirty(x, xs)
    }
  }
  @tailrec private def updateSrcValues(curDeps : List[CacheBoxRO[_]], curSrcs : List[CacheBoxRO[_]]) : Unit = curSrcs match {
    case Nil => curDeps.foreach(x => x.valueUpdate())
    case x :: xs =>
      if (x.valueIsEmpty) {
        val updatedDeps = x +: curDeps //adding in reverse to updated empty values in topological dependency order
        x match {
          case sd : CacheDerivedRO[_] => updateSrcValues(updatedDeps, xs ++ sd.sources)
          case sd : CacheDerivedROList[_] => updateSrcValues(updatedDeps, xs ++ sd.sources)
          case _ => updateSrcValues(updatedDeps, xs)
        }
      } else updateSrcValues(curDeps, xs)
  }
  @inline final protected def updateSrcValues() : Unit = updateSrcValues(List(), List(this))
  @inline final def get : T = {
    emptyValueUpdate()
    value.get
  }
  @inline final override def toString: String = get.toString
  @inline final protected def dirty() : Unit = dirty(this, List())
  @inline final protected def dirtyDeps() : Unit = deps.foreach{d => d.dirty()}
  @inline final def addDependency(st : CacheBoxRO[_]) : Unit = deps += st
  @inline final def removeDependency(st : CacheBoxRO[_]) : Unit = deps -= st
}
object CacheBoxRO {
  @inline def apply[T](updateFunc : => T) : CacheBoxRO[T] = new CacheBoxRO[T](updateFunc)
  @inline implicit def toValue[T](sf : CacheBoxRO[T]) : T = sf.get
}

final class CacheBoxRW[T](default : T) extends CacheBoxRO[T](default) {
  @inline def set(newValue : T) : Unit = {
    value = Some(newValue)
    dirtyDeps()
  }
}
object CacheBoxRW {
  def apply[T](t : T) : CacheBoxRW[T] = new CacheBoxRW[T](t)
}

//case class StateDerivedRW[T, R](st : StateBoxRW[T])(t2r : T => R)(r2t : (T, R) => T) extends StateBoxRW[R](t2r(st)) {
//  private var oldT : T = st
//  @inline override def set(newValue : R) : Unit = {
//    val oldR = super.get
//    if (newValue != oldR) {
//      super.set(newValue)
//      val newT = r2t(st.get, newValue)
//      st.set(newT)
//      oldT = newT
//    }
//  }
//  @inline override def get : R = {
//    val newT = st.get
//    if (oldT == newT) super.get
//    else {
//      oldT = newT
//      val newR = t2r(newT)
//      super.set(newR)
//      newR
//    }
//  }
//}

final class CacheDerivedRO[+T](val sources : List[CacheBoxRO[_]])(updateFunc : => T) extends CacheBoxRO[T](updateFunc) {
  sources.foreach{s => s.addDependency(this)}
}

final class CacheDerivedROList[+T](stBoxList : CacheBoxRO[List[CacheBoxRO[_]]])(updateFunc : => T) extends CacheBoxRO[T](updateFunc) {
  var stList : Option[List[CacheBoxRO[_]]] = None
  def sources : List[CacheBoxRO[_]] = stBoxList.get
  @inline override def emptyValueUpdate() : Unit = if (value.isEmpty) {
    val updateList = Some(stBoxList.get)
    if (updateList != stList) {
      stList.foreach{t => t.foreach {x => x.removeDependency(this)}}
      updateList.foreach{t => t.foreach {x => x.addDependency(this)}}
      dirty()
    }
    updateSrcValues()
  }
  stBoxList.addDependency(this)
}

object CacheDerivedRO {
  def apply[T](cb : CacheBoxRO[_]*)(updateFunc : => T) : CacheBoxRO[T] = new CacheDerivedRO(cb.toList)(updateFunc)
  def apply[T](cbList : CacheBoxRO[List[CacheBoxRO[_]]])(updateFunc : => T) : CacheBoxRO[T] = new CacheDerivedROList(cbList)(updateFunc)
}

