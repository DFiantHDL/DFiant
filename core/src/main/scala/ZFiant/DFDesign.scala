package ZFiant
import DFiant.internals._

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.mutable

abstract class DFDesign(implicit ctx : DFDesign.Context) extends HasTypeName with Implicits {
  private[ZFiant] lazy val inlinedRep : Option[MemberGetSet => String] = None
  final val block : DFDesign.Block = DFDesign.Block.Internal(typeName, inlinedRep)(ctx)
  private[DFDesign] val __db: DFDesign.DB.Mutable = ctx.db
  private[ZFiant] val ownerInjector : DFMember.OwnerInjector = new DFMember.OwnerInjector(block)
  protected implicit val __getset : MemberGetSet = ctx.db.getset

  ///////////////////////////////////////////////////////////////////
  // Context implicits
  ///////////////////////////////////////////////////////////////////
  final protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context =
    new DFAny.Context(meta, ownerInjector, ctx.db)
  final protected implicit def __blockContext(implicit meta : Meta) : DFBlock.Context =
    new DFBlock.Context(meta, ownerInjector, ctx.db)
  final protected implicit def __designContextOf[T <: DFDesign](implicit meta : Meta) : ContextOf[T] =
    new ContextOf[T](meta, ownerInjector, ctx.db)
  ///////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////
  // Conditional Constructs
  ///////////////////////////////////////////////////////////////////
  final protected def ifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
    implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
  ) : ConditionalBlock.NoRetVal.IfBlock = ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(),cond))(block)(ctx)
  final protected def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
    implicit ctx : DFBlock.Context
  ): ConditionalBlock.NoRetVal.MatchHeader[MVType] = ConditionalBlock.NoRetVal.MatchHeader[MVType](matchValue, matchConfig)(ctx)
  ///////////////////////////////////////////////////////////////////
}

@implicitNotFound(ContextOf.MissingError.msg)
final class ContextOf[T <: DFDesign](val meta : Meta, val ownerInjector : DFMember.OwnerInjector, val db: DFDesign.DB.Mutable) extends DFMember.Context {
  lazy val owner : DFBlock = ownerInjector.get
}
object ContextOf {
  final object MissingError extends ErrorMsg (
    "Missing an implicit DFDesign Context.",
    "missing-context"
  ) {final val msg = getMsg}
  implicit def evCtx[T1 <: DFDesign, T2 <: DFDesign](
    implicit ctx : ContextOf[T1], mustBeTheClassOf: MustBeTheClassOf[T1]
  ) : ContextOf[T2] = new ContextOf[T2](ctx.meta, ctx.ownerInjector, ctx.db)
  implicit def evTop[T <: DFDesign](
    implicit meta: Meta, topLevel : TopLevel, mustBeTheClassOf: MustBeTheClassOf[T], lp : shapeless.LowPriority
  ) : ContextOf[T] = new ContextOf[T](meta, null, new DFDesign.DB.Mutable)
}
object DFDesign {
  protected[ZFiant] type Context = DFBlock.Context

  implicit class DesignExtender[T <: DFDesign](design : T) {
    import design.__db.getset
    private def onBlock(b : Block => Unit) : T = {b(design.block); design}
    def setName(value : String) : T = onBlock(_.setName(value))
    def keep : T = onBlock(_.keep)
  }

  sealed trait Block extends DFBlock {
    type TTags = DFMember.Tags.Basic
    def headerCodeString(implicit getset: MemberGetSet): String = s"trait $typeName extends DFDesign"
  }
  object Block {
    final case class Internal(ownerRef : DFBlock.Ref, tags : DFMember.Tags.Basic, inlinedRep : Option[MemberGetSet => String])(designType: String) extends Block {
      def setTags(tags : DFMember.Tags.Basic)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags)(designType))
      override lazy val typeName : String = designType
    }
    object Internal {
      def apply(designType : String, inlinedRep : Option[MemberGetSet => String])(implicit ctx : Context) : Block = ctx.db.addMember(
        if (ctx.ownerInjector == null) Top(ctx.meta)(ctx.db, designType) else Internal(ctx.owner, ctx.meta, inlinedRep)(designType))
    }
    final case class Top(tags : DFMember.Tags.Basic)(db: DB.Mutable, designType: String) extends Block {
      override lazy val ownerRef : DFBlock.Ref = ???
      override def getOwner(implicit getset : MemberGetSet): DFBlock = this
      override val isTop: Boolean = true
      override lazy val typeName : String = designType
      override def getFullName(implicit getset : MemberGetSet): String = name
      def setTags(tags : DFMember.Tags.Basic)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags)(db, designType))
    }
  }

  implicit class DevAccess(design : DFDesign) {
    def db : DB = design.__db.immutable
  }
  final case class DB(members : List[DFMember], refTable : Map[DFMember.Ref, DFMember]) {
    lazy val top : Block.Top = members.head match {
      case m : Block.Top => m
    }
    implicit val getset : MemberGetSet = new MemberGetSet {
      def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0 = refTable(ref).asInstanceOf[M0]
      def set[M <: DFMember](originalMember : M, newMember: M): M = newMember
    }
    lazy val memberTable : Map[DFMember, Set[DFMember.Ref]] = refTable.invert

    //There can only be a single connection to a value (but multiple assignments are possible)
    //                              To    From
    lazy val connectionTable : Map[DFAny, DFAny] =
      members.collect{case n : DFNet.Connection => (n.toRef.get, n.fromRef.get)}.toMap

    //we reserve the order of assignments within the list
    lazy val assignmentsTable : Map[DFAny, List[DFAny]] =
      members.reverse.foldLeft(Map.empty[DFAny, List[DFAny]]){
        case (at, n : DFNet.Assignment) =>
          val toVal = n.toRef.get
          val fromVal = n.fromRef.get
          at + (toVal -> (fromVal :: at.getOrElse(toVal, List())))
        case (at, _) => at
      }

//    lazy val aliasesTable : Map[DFAny, List[DFAny]] =
//      members.foldLeft(Map.empty[DFAny, List[DFAny]]){
//        case (at, a : DFAny.Alias[_,_,_]) => connectionTable.get(a)
//          val toVal = a
//          val fromVal = n.fromRef.get
//          at + (toVal -> (at.getOrElse(toVal, List()) :+ fromVal))
//        case (at, _) => at
//      }

    def getConnectionTo(v : DFAny) : Option[DFAny] = connectionTable.get(v)
    def getAssignmentsTo(v : DFAny) : List[DFAny] = assignmentsTable.getOrElse(v, List())

    def getAliasesTo(v : DFAny) : Option[DFAny] =
      members.collectFirst{case n : DFNet.Connection if n.toRef.get == v => n.fromRef.get}

    //Owner-to-members list generation via a tail recursive function that topologically sorts the blocks according to dependency
    @tailrec private def OMLGen(
      oml : List[(DFBlock, List[DFMember])], globalMembers : List[DFMember], localStack : List[(DFBlock, List[DFMember])]
    ) : List[(DFBlock, List[DFMember])] = {
      val ((localOwner, localMembers), updatedStack0) = (localStack.head, localStack.drop(1))
      globalMembers match {
        case m :: mList if m.getOwner == localOwner => //current member indeed belongs to current owner
          val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
          m match {
            case o : DFBlock => //Deep borrowing into block as the new owner
              val updatedStack2 = (o -> List()) :: updatedStack1
              OMLGen(oml, mList, updatedStack2)
            case _ => //Just a member
              OMLGen(oml, mList, updatedStack1)
          }
        case x :: xs => //current member does not belong to current owner
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          OMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil if updatedStack0.nonEmpty =>
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          OMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil =>
          (localOwner -> localMembers.reverse) :: oml
      }
    }

    //holds the topological order of owner block dependency
    lazy val ownerMemberList : List[(DFBlock, List[DFMember])] =
      OMLGen(List(), members.drop(1), List(top -> List())).reverse //head will always be the TOP block
    def printOwnerMemberList() : Unit =
      println(ownerMemberList.map(e => (e._1.show, s"(${e._2.map(x => x.show).mkString(", ")})")).mkString("\n"))

    //holds a hash table that lists members of each owner block. The member list order is maintained.
    lazy val ownerMemberTable : Map[DFBlock, List[DFMember]] = Map(ownerMemberList : _*)

    private implicit class RefTableOps(rt : Map[DFMember.Ref, DFMember]) {
      def replaceMember(origMember : DFMember, repMember : DFMember, scope : DB.Patch.Replace.Scope) : Map[DFMember.Ref, DFMember] =
        memberTable.get(origMember) match {
          case Some(refs) =>
            val scopeRefs = scope match {
              case DB.Patch.Replace.Scope.All => refs
              case DB.Patch.Replace.Scope.Outside(block) =>
                //for references that have owner references of their own, we check the owners location with respect
                //to the requested scope
                refs.collect{case r : DFMember.OwnedRef if r.owner.get.isOutsideDesign(block) => r}
            }
            scopeRefs.foldLeft(rt)((rt2, r) => rt2.updated(r, repMember))
          case None =>
            rt
        }
    }

    //replaces all members and references according to the patch list
    def patch(patchList : List[(DFMember, DB.Patch)]) : DB = if (patchList.isEmpty) this else {
      val patchTable = patchList.map {
        //If we attempt to replace with an existing member, then we convert the patch to remove
        //the old member just for the member list (references are replaced).
        case (m, DB.Patch.Replace(r, DB.Patch.Replace.Config.FullReplacement, _)) if memberTable.contains(r) => (m, DB.Patch.Remove)
        //If we add after a design block, we need to actually place after the last member of the block
        case (m : DFDesign.Block.Internal, DB.Patch.Add(db, DB.Patch.Add.Config.After)) =>
          (ownerMemberTable(m).last, DB.Patch.Add(db, DB.Patch.Add.Config.After))
        case x => x
      }.toMap
      //Patching member list
      val patchedMembers = members.flatMap(m => patchTable.get(m) match {
        case Some(DB.Patch.Replace(r, config, _)) => config match {
          case DB.Patch.Replace.Config.ChangeRefOnly => Some(m)
          case DB.Patch.Replace.Config.FullReplacement => Some(r)
        }
        case Some(DB.Patch.Add(db, config)) =>
          val notTop = db.members.drop(1) //adding the members without the Top design block
          config match {
            case DB.Patch.Add.Config.After => m :: notTop
            case DB.Patch.Add.Config.Before => notTop :+ m
            case DB.Patch.Add.Config.Replace => notTop
            case DB.Patch.Add.Config.Via => m :: notTop
          }
        case Some(DB.Patch.Remove) => None
        case Some(_ : DB.Patch.ChangeRef[_]) => Some(m)
        case None => Some(m) //not in the patch table, therefore remain as-is
      })
      //Patching reference table
      val patchedRefTable = patchList.foldLeft(refTable) {
        case (rt, (origMember, DB.Patch.Replace(repMember, _, scope))) => rt.replaceMember(origMember, repMember, scope)
        case (rt, (origMember, DB.Patch.Add(db, config))) =>
          val dbPatched = db.patch(db.top -> DB.Patch.Replace(origMember.getOwner, DB.Patch.Replace.Config.ChangeRefOnly))
          val repRT = config match {
            case DB.Patch.Add.Config.Replace =>
              val repMember = db.members(1) //At index 0 we have the Top. We don't want that.
              rt.replaceMember(origMember, repMember, DB.Patch.Replace.Scope.All)
            case DB.Patch.Add.Config.Via =>
              val repMember = db.members.last //The last member is used for Via addition.
              rt.replaceMember(origMember, repMember, DB.Patch.Replace.Scope.All)
            case _ => rt
          }
          repRT ++ dbPatched.refTable
        case (rt, (origMember, DB.Patch.Remove)) => memberTable.get(origMember) match {
          case Some(refs) => refs.foldLeft(rt)((rt2, r) => rt2 - r)
          case None => rt
        }
        case (rt, (_, DB.Patch.ChangeRef(origMember, refFunc, updatedRefMember))) =>
          val ref = refFunc(origMember)
          rt + (ref -> updatedRefMember)
      }
      DB(patchedMembers, patchedRefTable)
    }
    def patch(singlePatch : (DFMember, DB.Patch)) : DB = patch(List(singlePatch))
    @tailrec private def getGuards(currentOwner : DFBlock, targetOwner : DFBlock, currentGuards : List[DFAny]) : List[DFAny] =
      currentOwner match {
        case _ : DFDesign.Block => currentGuards //reached the design block
        case o : DFBlock if o == targetOwner => currentGuards //can't go past the target owner
        case cb : ConditionalBlock.IfBlock => getGuards(cb.getOwner, targetOwner, cb.condRef.get :: currentGuards)
        case cb : ConditionalBlock.ElseIfBlock => getGuards(cb.prevBlockRef, targetOwner, cb.condRef.get :: currentGuards)
        case cb : ConditionalBlock.ElseBlock => getGuards(cb.prevBlockRef, targetOwner, currentGuards)
        case cb : ConditionalBlock.CasePatternBlock[_] => getGuards(cb.getOwner, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
        case cb : ConditionalBlock.Case_Block => getGuards(cb.getOwner, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
      }

    //for a given consumer, we get a set of its producers
    lazy val consumerDependencyTable : Map[DFAny, Set[DFAny]] = {
      //first passing through all nets to get directionality of aliased values
      val netPass = members.foldLeft(Map.empty[DFAny, Set[DFAny]])((dt, m) => m match {
        case n : DFNet =>
          val toVal = n.toRef.get
          val fromVal = n.fromRef.get
          val depSet = dt.getOrElse(toVal, Set()) + fromVal
          val guards = n match {
            case _ : DFNet.Connection => List() //connections are insensitive to guards
            case a : DFNet.Assignment => getGuards(a.getOwner, toVal.getOwner, List())
          }
          dt + (toVal -> (depSet ++ guards))
        case _ => dt
      })
      members.foldLeft(netPass)((dt, m) => m match {
        case f : DFAny.Func2[_,_,_,_] => dt + (f-> Set(f.leftArgRef.get, f.rightArgRef.get))
        case a : DFAny.Alias[_,_,_] =>
          val relVal = a.relValRef.get
          (dt.get(a), dt.get(relVal)) match {
            //when alias is written to, then the relative value is also dependent on the alias
            case (Some(aDeps), Some(relDeps)) => dt + (a -> (aDeps + relVal)) + (relVal -> (relDeps + a))
            case (Some(aDeps), None) => dt + (a -> (aDeps + relVal)) + (relVal -> Set(a))
            //the alias is just read, and therefore it's dependent only on the rel
            case (None, _) => dt + (a -> Set(relVal))
          }
        case _ => dt
      })
    }

    //for a given producer, we get a set of its consumers
    lazy val producerDependencyTable : Map[DFAny, Set[DFAny]] = {
      consumerDependencyTable.foldLeft(Map.empty[DFAny, Set[DFAny]]){case (dt, (consumer, producerSet)) =>
        dt ++ producerSet.map(p => p -> (dt.getOrElse(p, Set()) + consumer))}
    }


    def printConsumerDependencyTable : DB = {
      println(consumerDependencyTable.map(op => s"${op._1.getFullName} <- ${op._2.map(e => e.getFullName)}").mkString("\n"))
      this
    }

    def printProducerDependencyTable : DB = {
      println(producerDependencyTable.map(op => s"${op._1.getFullName} -> ${op._2.map(e => e.getFullName)}").mkString("\n"))
      this
    }

    def printOwnership() : DB = {
      println(members.map(m => (m -> m.getOwner).toString()).mkString("\n"))
      this
    }
  }

  object DB {
    sealed trait Patch extends Product with Serializable
    object Patch {
      final case object Remove extends Patch
      final case class Replace(updatedMember : DFMember, config : Replace.Config, scope : Replace.Scope = Replace.Scope.All) extends Patch
      object Replace {
        sealed trait Config extends Product with Serializable
        object Config {
          //only modifies the reference table so that all members currently referencing the original member will reference
          //the updated member.
          case object ChangeRefOnly extends Config
          //The updated member is replacing the original member in the member list and all members currently
          //referencing the existing member will reference the updated member.
          //If the updated member already exists in the member list (at a different position), then the original member is
          //removed from the list without being replaced in its position.
          case object FullReplacement extends Config
        }
        sealed trait Scope extends Product with Serializable
        object Scope {
          //All references are replaced
          case object All extends Scope
          //Only references from outside the given block are replaced
          case class Outside(block : DFDesign.Block.Internal) extends Scope
        }
      }
      final case class Add(db : DB, config : Add.Config) extends Patch
      object Add {
        def apply(design : DFDesign, config : Add.Config) : Add = Add(design.db, config)
        sealed trait Config extends Product with Serializable
        object Config {
          //adds members before the patched member
          case object Before extends Config
          //adds members after the patched member
          case object After extends Config
          //adds members replacing the patched member.
          //The FIRST (non-Top) member is considered the reference replacement member
          case object Replace extends Config
          //adds members after the patched member.
          //The LAST member is considered the reference replacement member
          case object Via extends Config
        }
      }
      final case class ChangeRef[T <: DFMember](member : T, refAccess : T => DFMember.Ref, updatedRefMember : DFMember) extends Patch
    }
    class Mutable {
      private val members : mutable.ArrayBuffer[DFMember] = mutable.ArrayBuffer()
      def addConditionalBlock[Ret, CB <: ConditionalBlock.Of[Ret]](cb : CB, block : => Ret)(implicit ctx : DFBlock.Context) : CB = {
        addMember(cb)
        cb.applyBlock(block)
        cb
      }
      def addMember[M <: DFMember](member : M) : M = {
        memberTable += (member -> (members.length, Set()))
        members += member
        member
      }
      private val memberTable : mutable.Map[DFMember, (Int, Set[DFMember.Ref])] = mutable.Map()
      private val refTable : mutable.Map[DFMember.Ref, DFMember] = mutable.Map()
      def hasToConnectionFor(dfVar : DFAny) : Boolean = {
        memberTable.getOrElse(dfVar, (0, Set()))._2.collectFirst {
          case DFNet.ToRef() => true
        }.nonEmpty
      }
      def getMember[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0 = refTable(ref).asInstanceOf[M0]
      def setMember[M <: DFMember](originalMember : M, newMember : M) : M = {
        val x = memberTable.remove(originalMember).get
        x._2.foreach(r => refTable.update(r, newMember))
        memberTable.update(newMember, x)
        members.update(x._1, newMember)
        newMember
      }
      def newRefFor[M <: DFMember, T <: DFMember.Ref.Type, R <: DFMember.Ref.Of[T, M]](ref : R, member : M) : R = {
        memberTable.get(member) match {
          case Some(x) => memberTable.update(member, x.copy(_2 = x._2 + ref))
          case _ =>
          //In case where we do meta programming and planting one design into another,
          //we may not have the member available at the table. This is OK.
        }
        refTable += (ref -> member)
        ref
      }
      def immutable : DB = {
        refTable.keys.foreach{
          case or : DFMember.OwnedRef => or.owner  //touching all lazy owner refs to cause force their addition
          case _ => //do nothing
        }
        DB(members.toList, refTable.toMap)
      }

      implicit val getset : MemberGetSet = new MemberGetSet {
        def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref: DFMember.Ref.Of[T, M]): M0 = getMember(ref)
        def set[M <: DFMember](originalMember : M, newMember: M): M = setMember(originalMember, newMember)
      }
    }
  }
}