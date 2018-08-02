package DFiant.internals

import scala.collection.mutable.ListBuffer

trait DSLConstruct {

}

trait DSLConfiguration

trait DSLOwnableConstruct extends DSLConstruct with HasProperties with Nameable with TypeNameable with Discoverable {
  val owner : DSLOwnerConstruct
  def keep : this.type = {
    owner.mutableKeepList += this
    this
  }
  final lazy val fullName : String =
    if (owner != null) s"${owner.fullName}.$name"
    else name //Top

  private def relativeName(refFullName : String, callFullName : String) : String = {
    val c = callFullName.split('.')
    val r = refFullName.split('.')
    if (r.length < c.length)
      refFullName
    else {
      val same = c.zip(r).filter(e => e._1 != e._2).isEmpty
      if (same) r.takeRight(r.length-c.length).mkString(".") else ""
    }
  }

  def relativeName[Owner <: DSLOwnerConstruct, Config <: DSLConfiguration](
    implicit ctx : DSLOwnerConstruct.Context[Owner, Config]) : String = relativeName(fullName, ctx.owner.fullName)

  protected def discoveryDepenencies : List[Discoverable] = if (owner != null) List(owner) else List()
  final protected def getID : Int = if (owner != null) owner.newItemGetID(this) else 0
  def codeString : String
  protected def lateRun : Unit = {}
  final private[internals] lazy val lateRunOnce : Unit = lateRun
  val id : Int
}

trait DSLOwnerConstruct extends DSLOwnableConstruct {
  protected implicit def theOwnerToBe : DSLOwnerConstruct = this
  private var idCnt : Int = 0
  private val mutableOwnedList : ListBuffer[DSLOwnableConstruct] = ListBuffer.empty[DSLOwnableConstruct]
  final lazy val ownedList : List[DSLOwnableConstruct] = {
    mutableOwnedList.foreach(e => e.lateRunOnce)
    mutableOwnedList.toList
  }
  final protected[internals] def newItemGetID(item : DSLOwnableConstruct) : Int = {
    mutableOwnedList += item
    idCnt += 1
    idCnt
  }

  private[internals] val mutableKeepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  final lazy val keepList : List[Discoverable] = mutableKeepList.toList
  override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ keepList
  final lazy val discoveredList : List[DSLOwnableConstruct] = {
    discover
    ownedList.filterNot(o => o.isNotDiscovered)
  }
}
object DSLOwnerConstruct {
  trait Context[+Owner <: DSLOwnerConstruct, +Config <: DSLConfiguration] {
    implicit val owner : Owner
    implicit val config : Config
    val n : NameIt
  }
}