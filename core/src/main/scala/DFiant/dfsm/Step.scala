package DFiant
package dfsm

import scala.collection.immutable
import internals._
/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Step
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[DFiant] sealed abstract class Step(implicit ctx : DFBlock.Context) extends Product with Serializable {
  val meta : Meta = ctx.meta
  protected def outIfs(fsm : FSM, list : List[Edge]) : Unit = list match {
    case Edge(Some(cond), block, dest) :: Nil => ifdf(cond()){
      block()
      fsm.goto(dest)
    }
    case Edge(None, block, dest) :: Nil =>
      block()
      fsm.goto(dest)
    case _ =>
  }
  def elaborateAt(fsm : FSM) : Unit = {}
}
protected[DFiant] object Step {
  implicit def fsmFromStep(implicit ctx : DFBlock.Context) : FSM.TC[Step] = s => FSM(immutable.ListMap(s -> List()), s, s)
  final case class Basic(alwaysBlock : () => Unit)(implicit ctx : DFBlock.Context) extends Step {
    override def elaborateAt(fsm : FSM) : Unit = {
      val edgeList = fsm.edges(this)
      outIfs(fsm, edgeList)
    }
  }
  final case class DoWhile(cond : () => DFBool, alwaysBlock : () => Unit)(implicit ctx : DFBlock.Context) extends Step

  final case class Owner(
    ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFOwner.NameFlattenOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    val nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(_, tags) => this.tags =~ tags
      case _ => false
    }
    private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(container : DFOwner.Container)(
      implicit ctx : DFBlock.Context
    ) : Owner = ctx.db.addContainerOwner(container)(Owner(ctx.owner, ctx.meta))
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Edge
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[dfsm] final case class Edge(condOption : Option[() => DFBool], block : () => Unit, dest : Step)
/////////////////////////////////////////////////////////////////////////////////////////////////////////

