package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.internals.IntExtras

import collection.immutable
import scala.annotation.tailrec

private final class UniqueBlock(val block : DFDesign.Block.Internal, val members : List[DFMember])(implicit getSet: MemberGetSet) {
  override def equals(obj : Any): Boolean = obj match {
    case that : UniqueBlock if this.block.designType == that.block.designType =>
      (this.members lazyZip that.members).forall {
        case (l, r) if l.hasLateConstruction && r.hasLateConstruction => true
        case (l, r) => l =~ r
      }
    case _ => false
  }
  override def hashCode(): Int = block.designType.hashCode
}

final class UniqueDesigns[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def uniqueDesigns : IRCompilation[D] = {
    val uniqueBlockMap : Map[UniqueBlock, List[DFDesign.Block.Internal]] = designDB.members.collect {
      case block : DFDesign.Block.Internal if block.inlinedRep.isEmpty => block
    }.groupBy(b => new UniqueBlock(b, designDB.designMemberTable(b)))
    val uniqueTypeMap : Map[String, List[UniqueBlock]] = uniqueBlockMap.keys.toList.groupBy(ub => ub.block.designType)
    val patchList = uniqueTypeMap.flatMap {
      case (designType, list) if list.size > 1 => list.zipWithIndex.flatMap{case (ub, i) =>
        val updatedDesignType = s"${designType}_${i.toPaddedString(list.size)}"
        uniqueBlockMap(ub).map(block => block -> Patch.Replace(block.copy(designType = updatedDesignType), Patch.Replace.Config.FullReplacement))
      }
      case _ => Nil
    }.toList
    c.newStage(designDB.patch(patchList))
  }
}