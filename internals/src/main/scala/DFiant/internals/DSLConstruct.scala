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

package DFiant.internals

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection._

trait DSLConstruct

trait DSLConfiguration {
  val foldComponents : Boolean
}

trait HasOwner {
  trait __DevHasOwner {
    implicit val owner : DSLOwnerConstruct
  }
}

trait DSLMemberConstruct extends DSLConstruct with HasProperties
  with Nameable with TypeNameable with HasPostConstructionOnlyDefs with HasOwner {self =>
  protected[DFiant] type ThisMember <: DSLMemberConstruct
  protected[DFiant] type ThisOwner <: DSLOwnerConstruct
  trait __DevDSLMemberConstruct extends __DevNameable with __DevTypeNameable with __DevHasOwner {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    val ownerOption : Option[DSLOwnerConstruct] = ctx.ownerOption
    final lazy val owner : ThisOwner = ownerOption.getOrElse(unexpectedNullOwner).asInstanceOf[ThisOwner]
    def unexpectedNullOwner = throw new IllegalArgumentException("\nUnexpected null Owner")
    final lazy val nonTransparentOwner : DSLOwnerConstruct = nonTransparentOwnerOption.getOrElse(unexpectedNullOwner)
    final lazy val nonTransparentOwnerOption : Option[DSLOwnerConstruct] = ownerOption.map(o => o.nonTransparent)
    final def hasSameOwnerAs(that : DSLMemberConstruct) : Boolean =
      nonTransparentOwnerOption == that.nonTransparentOwnerOption
    final def isDownstreamMemberOf(that : DSLOwnerConstruct) : Boolean =
      (nonTransparentOwnerOption, that) match {
        case (None, _) => false
        case (Some(a), b) if a == b => true
        case (Some(a), b) => a.isDownstreamMemberOf(that)
      }
    final def isConnectedAtOwnerOf(member : DSLMemberConstruct)(
      implicit callOwner : DSLOwnerConstruct
    ) : Boolean = member.nonTransparentOwnerOption.contains(callOwner.nonTransparent)
    final def isConnectedAtEitherSide(left : DSLMemberConstruct, right : DSLMemberConstruct)(
      implicit callOwner : DSLOwnerConstruct
    ) : Boolean = isConnectedAtOwnerOf(left.nonTransparentOwner) || isConnectedAtOwnerOf(right.nonTransparentOwner)
    final protected def getID : Int = ownerOption.map(o => o.addMember(self.asInstanceOf[o.ThisMember])).getOrElse(0)
    final lazy val id : Int = getID

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final lazy val meta = ctx.meta
    override lazy val nameScala: String = ownerOption match {
      case Some(o) => o.metaNameTable(self)
      case None => meta.name
    }
    final private def relativePath(refFullPath : String, callFullPath : String) : String = {
      val c = callFullPath.split('.')
      val r = refFullPath.split('.')
      if (r.length < c.length) {
        val idx = r.zip(c).indexWhere(e => e._1 != e._2)
        if (idx == -1) "" else r.takeRight(c.length-idx-1).mkString(".")
      } else {
        val idx = c.zip(r).indexWhere(e => e._1 != e._2)
        if (idx == -1) r.takeRight(r.length-c.length).mkString(".") else r.takeRight(r.length-idx).mkString(".")
      }
    }

    final private def relativePath(implicit callOwner : DSLOwnerConstruct) : String =
      relativePath(fullPath, callOwner.fullName)

    final private[DFiant] def relativeName(implicit callOwner : DSLOwnerConstruct) : String = relativeName(name)(callOwner)
    final private[DFiant] def relativeName(name : String)(implicit callOwner : DSLOwnerConstruct) : String = {
      val path = relativePath(callOwner)
      if (path == "") name else s"$path.$name"
    }

    def codeString : String
    def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = relativeName
  }
  override private[DFiant] lazy val __dev : __DevDSLMemberConstruct = ???
  __dev //touch dev. We only need the lazyness for initialization order
  import __dev._

  final val topOwner : DSLOwnerConstruct =
    ownerOption.map(o => o.topOwner).getOrElse(self.asInstanceOf[DSLOwnerConstruct])

  final lazy val name : CacheBoxRO[String] =
    ownerOption.map(o => CacheDerivedRO(o.nameTable)(o.nameTable(self))).getOrElse(nameTemp)
  final lazy val fullPath : CacheBoxRO[String] =
    ownerOption.map(o => CacheDerivedRO(o.fullName)(s"${o.fullName}")).getOrElse(CacheBoxRO(""))
  final lazy val fullName : CacheBoxRO[String] =
    CacheDerivedRO(name, fullPath)(if (fullPath.isEmpty) name else s"$fullPath.$name")

  private[DFiant] lazy val ctx : DSLOwnerConstruct.Context[DSLOwnerConstruct, DSLConfiguration] = ???

  override def toString: String = s"$fullName : $typeName"
}

object DSLMemberConstruct {
  implicit def fetchDev(from : DSLMemberConstruct)(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
}


trait DSLOwnerConstruct extends DSLMemberConstruct {self =>
  protected[DFiant] trait __DevDSLOwnerConstruct extends __DevDSLMemberConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final lazy val isTop : Boolean = ownerOption.isEmpty
    lazy val nonTransparent : DSLOwnerConstruct = self
    final private[DFiant] def callSiteSameAsOwnerOf(member : DSLMemberConstruct) : Boolean =
      if (self.nonTransparent eq member.nonTransparentOwner) true
      else if (self.nonTransparentOwnerOption.isEmpty) false
      else false
    final val addedMembers = CacheListRW(List[ThisMember]())
    lazy val members : CacheBoxRO[List[ThisMember]] = addedMembers
    final def addMember(member : ThisMember) : Int = {
      addedMembers += member
//            println(s"newItemGetID ${member.fullName} : ${member.typeName}")
      addedMembers.size
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final val metaNameTable : CacheBoxRO[Map[DSLMemberConstruct, Meta.Name]] =
      CacheDerivedRO(members) {
        //Name-Position to Latest Call-Position Map
        val latestPosMap = mutable.Map[Meta.Position, Meta.Position]()
        members.foreach {m =>
          latestPosMap.get(m.meta.namePosition) match {
            case Some(v) if !m.meta.name.anonymous && m.meta.position <= m.meta.namePosition && m.meta.position > v =>
              latestPosMap += (m.meta.namePosition -> m.meta.position)
//            case Some(v) if m.meta.position > m.meta.namePosition && m.meta.position > v =>
//              latestPosMap += (m.meta.namePosition -> m.meta.position)
            case None if !m.meta.name.anonymous =>
              latestPosMap += (m.meta.namePosition -> m.meta.position)
            case _ => //Do nothing
          }
        }
        def isAnonymous(member : DSLMemberConstruct) : Boolean =
          member.meta.name.anonymous ||
          (member.meta.position > member.meta.namePosition && !member.nameFirst) ||
          (latestPosMap(member.meta.namePosition) != member.meta.position) //&& !member.nameFirst

        val usagesMap = mutable.Map[Meta.Position, Int]()
        members.foreach {m =>
          usagesMap.get(m.meta.position) match {
            case Some(v) if !isAnonymous(m) =>
              usagesMap += (m.meta.position -> (v + 1))
            case _ if !isAnonymous(m) =>
              usagesMap += (m.meta.position -> 1)
            case _ =>
              usagesMap += (m.meta.position -> 0)
          }
        }
        def getUsages(member : DSLMemberConstruct) : Int = usagesMap(member.meta.position)

        val idxMap = usagesMap.clone()
        val nameMap = members.reverse.map {m =>
          val idx = idxMap(m.meta.position) - 1
          idxMap += m.meta.position -> idx
          m -> m.meta.name.copy(anonymous = isAnonymous(m), idx = idx, usages = getUsages(m))
        }
        Map(nameMap : _*)
      }

    //the table saves the number of occurrences for each member name, to generate unique names when the scala scope
    //isn't enough to protect from reusing the same name, e.g.: loops that generate new members.
    private val membersNamesTemp = CacheDerivedRO(members)(members.map(x => x.nameTemp))
    final val nameTable : CacheBoxRO[Map[DSLMemberConstruct, String]] =
      CacheDerivedRO(membersNamesTemp) {
        case class Info(usages : Int, idx : Int){
          override def toString : String = {
            val max_digits = usages.toString.length
            val digits = idx.toString.length
            val addedZeros = "0" * (max_digits-digits)
            s"$addedZeros$idx"
          }
          def incUsages : Info = copy(usages = usages + 1)
          def incIdx : Info = copy(idx = idx + 1)
        }

        val nt = mutable.HashMap[String, Info]()
        members.foreach {m =>
          nt.get(m.nameTemp) match {
            case Some(v) =>
              nt += (m.nameTemp.unbox -> v.incUsages)
            case None =>
              nt += (m.nameTemp.unbox -> Info(1, 0))
          }
        }
        //priority-named members are placed last, so they receive the non-indexed name
        val priorityNamedMembers = members.filterNot(x => x.nameFirst) ++ members.filter(x => x.nameFirst)
        val nameMap = priorityNamedMembers.map {m =>
          val info = nt(m.nameTemp)
          val finalName = if (info.idx == info.usages-1) m.nameTemp.unbox else s"${Meta.Name.AnonStart}${m.nameTemp}_$info"
          nt += (m.nameTemp.unbox -> info.incIdx)
          m -> finalName
        }
        Map(nameMap : _*)
      }
  }
  override private[DFiant] lazy val __dev : __DevDSLOwnerConstruct = ???
  import __dev._

  protected implicit def __theOwnerToBe : DSLOwnerConstruct = this
  val __config : DSLConfiguration
}

trait DSLContext {
  val ownerOption : Option[DSLOwnerConstruct]
  implicit lazy val owner : DSLOwnerConstruct =
    ownerOption.getOrElse(throw new IllegalArgumentException("\nExepcted a non-null owner, but got one"))
}
object DSLContext {
  final object MissingContext extends ErrorMsg (
    "Missing an implicit DFDesign Context.",
    "missing-context"
  ) {final val msg = getMsg}
}

object DSLOwnerConstruct {
  implicit def fetchDev(from : DSLOwnerConstruct)(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
  trait Context[+Owner <: DSLOwnerConstruct, +Config <: DSLConfiguration] extends DSLContext {
    val ownerOption : Option[Owner]
    override implicit lazy val owner : Owner =
      ownerOption.getOrElse(throw new IllegalArgumentException("\nExepcted a non-null owner, but got one"))
    implicit val config : Config
    val meta : Meta
    override def toString: String = meta.name
  }
  trait DB[Owner, Body <: Any] {
    private case class Info(id : Int, order : Int, owners : ListBuffer[Owner])
    private val db = mutable.HashMap.empty[String, mutable.HashMap[Body, Info]]
//    private var dbString = ""
    private var order = 0
    private def actualTypeName(ownerTypeName : String, info : Info) : String =
      if (info.id == 0) ownerTypeName else ownerTypeName + Meta.Name.Separator + info.id
    def addOwnerBody(ownerTypeName : String, ownerBody : Body, owner : Owner) : String = {
      var newBody : Boolean = false
      val csHM = db.getOrElseUpdate(ownerTypeName, {newBody = true; mutable.HashMap.empty[Body, Info]})
      val info = csHM.getOrElseUpdate(ownerBody, {newBody = true; Info(csHM.size, order, ListBuffer.empty)})
      info.owners += owner
      val atn = actualTypeName(ownerTypeName, info)
      if (newBody) {
        order += 1
//        dbString += ownerToString(atn, ownerBody) + "\n"
      }
      atn
    }
    final lazy val namedBodies : List[(String, String)] = db.toList.flatMap(e => e._2.toList.map(f => {
      val atn = actualTypeName(e._1, f._2)
      (f._2.order, atn, ownerToString(atn, f._1)) //unsorted tuple that includes the order count
    })).sortBy(e => e._1).map(e => (e._2, e._3))
    final lazy val dbString : String = namedBodies.map(e => e._2).mkString("\n")
    def ownerToString(ownerTypeName : String, ownerBody : Body) : String
    override def toString : String = dbString
  }
}

trait DSLTransparentOwnerConstruct extends DSLOwnerConstruct {
  protected[DFiant] trait __DevDSLTransparentOwnerConstruct extends __DevDSLOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override lazy val nonTransparent : DSLOwnerConstruct = owner.nonTransparent

  }
  override private[DFiant] lazy val __dev : __DevDSLTransparentOwnerConstruct = ???
  import __dev._
}

trait DSLFoldableOwnerConstruct extends DSLOwnerConstruct {
  protected[DFiant] trait __DevDSLFoldableOwnerConstruct extends __DevDSLOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Folding/Unfolding
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private var foldedMemberList : List[ThisMember] = List()

    private var folded : Boolean = false
    final val isFolded = CacheDerivedRO(members){
      folded
    }
    private[DFiant] def unfoldedRun : Unit = {}

    private lazy val firstFold : Unit = {
      foldedMemberList = addedMembers
      foldedRun
      folded = true
//      foldRequest = __config.foldComponents
    }
    private[DFiant] def preFoldUnfold() : Unit = {
      addedMembers.set(foldedMemberList)
    }

    final protected[DSLFoldableOwnerConstruct] lazy val foldRequest = CacheBoxRW(true)
    final override lazy val members : CacheBoxRO[List[ThisMember]] = CacheDerivedRO(addedMembers, foldRequest) {
      firstFold
      val foldReq = foldRequest.unbox
      if (folded != foldReq) {
        preFoldUnfold()
        if (foldReq) foldedRun else unfoldedRun
        folded = foldReq
      }
      addedMembers.unbox
    }
  }
  override private[DFiant] lazy val __dev : __DevDSLFoldableOwnerConstruct = ???
  import __dev._
  //override foldedRun to support folded run (inject output->input dependencies and setup initialization)
  protected def foldedRun : Unit = {}

  def fold : this.type = {
    foldRequest.set(true)
    this
  }
  def unfold : this.type = {
    foldRequest.set(false)
    this
  }

}

object DSLFoldableOwnerConstruct {
  implicit def fetchDev(from : DSLFoldableOwnerConstruct)(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
}

trait DSLSelfConnectedFoldableOwnerConstruct extends DSLFoldableOwnerConstruct