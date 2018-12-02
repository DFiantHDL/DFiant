package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

abstract class RTComponent(implicit ctx0 : RTComponent.Context, args : sourcecode.Args) extends DFInterface {
  val ctx = ctx0
  override implicit def theOwnerToBe : RTComponent = this
  protected def newGeneric() : Unit = {}
  final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ portsIn
  //final protected def discovery : Unit = {}

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final override private[DFiant] def nameDefault: String = ctx.getName
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final protected def setInitFunc[DFVal <: DFAny.Uninitialized[_]](dfVal : DFVal)(value : LazyBox[Seq[dfVal.TToken]])
  : Unit = dfVal.setInitFunc.forced(value)
  final protected def getInit[DFVal <: DFAny.Uninitialized[_]](dfVal : DFVal) : LazyBox[Seq[dfVal.TToken]] = dfVal.initLB

  final protected[DFiant] lazy val init : Unit = {}
  final val id = getID

  override lazy val typeName: String =
    getClass.getName + args.value.dropRight(1).map(e => e.map(f => f.value).mkString("(",", ",")")).mkString

  override def codeString: String = {
    s"\nval $name = new $typeName {}"
  }
}

object RTComponent {
  type Context = DFAnyOwner.ContextOf[RTComponent, DFBlock]
}