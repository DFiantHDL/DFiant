package ZFiant
package compiler

import DFiant.internals._
import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.collection.immutable

final class SingleStepPrevOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.explicitPrev.fixAnonymous.db
  import designDB.getset

  def singleStepPrev = {
    designDB.members.collect {
      case p @ DFAny.Alias.Prev(_, relValRef, step, _, tags) =>
        val relVal = relValRef.get
//        if (p.isAnonymous)
    }
//    val explicitPrevSet = getImplicitPrevVars(designDB.members.drop(1), designDB.top, Map(), Set())
//    val patchList = explicitPrevSet.toList.map(e => e -> Patch.Add(new MetaDesign() {
//      DFNet.Assignment(e, DFAny.Alias.Prev(e, 1))
//    }, Patch.Add.Config.After))
//
//          println(explicitPrevSet.map(e => e.getFullName).mkString(", "))
//    c.newStage[SingleStepPrev](designDB.patch(patchList), Seq())
    ???
  }
}

trait SingleStepPrev extends Compilable.Stage
