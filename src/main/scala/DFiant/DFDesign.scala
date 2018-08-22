package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

import scala.collection.mutable.{HashMap, ListBuffer}

abstract class DFBlock(implicit ctx : DFBlock.Context) extends DFAnyOwner with Implicits {
  override implicit def theOwnerToBe : DFBlock = this
  final val owner = ctx.owner
  final implicit val basicLib = ctx.basicLib
  final implicit val config = ctx.config
  final val topDsn : DFDesign =
    if (owner != null) owner.topDsn
    else this.asInstanceOf[DFDesign] //The top will always be a DFDesign
  private[DFiant] val designDB : DFDesign.DB =
    if (owner == null) new DFDesign.DB else owner.designDB

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Sub-Blocks
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final private lazy val blocks : List[DFBlock] = memberList.collect{case o : DFBlock => o}
  final private lazy val rtcomponents : List[RTComponent] = memberList.collect{case o : RTComponent => o}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def compileToVHDL(fileName : String) = ???

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Naming
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final def isTop : Boolean = owner == null
  override private[DFiant] def nameDefault: String = ctx.getName
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //final protected def discovery : Unit = protAlmanac

  final val id = getID
}
object DFBlock {
  trait ContextOf[+T, +Owner <: DFAnyOwner] extends DFAnyOwner.ContextWithLibOf[T, Owner] {
    self =>
    def updateOwner[Owner0 <: DFAnyOwner](owner0 : Owner0)(implicit n0 : NameIt) : ContextOf[T, Owner0] = new ContextOf[T, Owner0] {
      implicit val owner: Owner0 = owner0
      implicit val basicLib: DFBasicLib = self.basicLib
      implicit val config: DFAnyConfiguration = self.config
      val n: NameIt = n0
    }
  }
  object ContextOf {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit evOwner : Owner = null, evBasicLib : DFBasicLib, evConfig : DFAnyConfiguration, evNameIt : NameIt
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      implicit val owner: Owner = evOwner
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }
  type Context = ContextOf[Nothing, DFBlock]
}

protected[DFiant] trait ConditionalBlock

protected class DFIfBlock[RV](cond : DFBool, block : => RV)(implicit ctx : DFIfBlock.Context, mutableOwner: MutableOwner)
  extends DFDesign with ConditionalBlock {
  def elseifdf(elseCond : DFBool)(elseBlock : => RV)(implicit ctx : DFIfBlock.Context)
  : DFIfBlock[RV] = new DFElseIfBlock[RV](this, elseCond, elseBlock)
  def elsedf(elseBlock: => RV)(implicit ctx : DFIfBlock.Context)
  : DFElseBlock[RV] = new DFElseBlock[RV](this, elseBlock)

  override private[DFiant] def createAlmanac : AlmanacIf = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)
  override protected def discoveryDepenencies = super.discoveryDepenencies :+ cond
  override private[DFiant] def nameDefault: String = ctx.getName
  override def codeString: String = s"\nval $name = ifdf(${cond.refCodeString}) {$bodyCodeString\n}"

  private val originalOwner = mutableOwner.value
  mutableOwner.value = this
  block
  mutableOwner.value = originalOwner
}

protected class DFElseIfBlock[RV](prevIfBlock : DFIfBlock[RV], cond : DFBool, block : => RV)(implicit ctx : DFIfBlock.Context, mutableOwner : MutableOwner)
  extends DFIfBlock[RV](cond, block) {
  override private[DFiant] def nameDefault: String = ctx.getName + "$elseif"
  override private[DFiant] def createAlmanac : AlmanacElseIf =
    new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
  override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
  override def codeString: String = s".elseifdf(${cond.refCodeString}) {$bodyCodeString\n}"
}

protected class DFElseBlock[RV](prevIfBlock : DFIfBlock[RV], block : => RV)(implicit ctx : DFIfBlock.Context, mutableOwner : MutableOwner)
  extends DFDesign with ConditionalBlock {
  override private[DFiant] def nameDefault: String = ctx.getName + "$else"
  override private[DFiant] def createAlmanac : AlmanacElse =
    new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
  override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
  override def codeString: String = s".elsedf {$bodyCodeString\n}"

  private val originalOwner = mutableOwner.value
  mutableOwner.value = this
  block
  mutableOwner.value = originalOwner
}

object DFIfBlock {
  type Context = DFDesign.Context
}

class MutableOwner(var value : DFDesign)

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {
  private implicit val mutableOwner : MutableOwner = new MutableOwner(this)
  final override implicit def theOwnerToBe : DFDesign = mutableOwner.value

  //The block by value object is created within the context of the current DFDesign,
  //so we mutate `theOwnerToBe` via mutableOwner which is passed to the IfBlock constructs
  private[DFiant] def injectConditionalBlock[IB <: DFDesign](ifBlock : IB, block: => Unit)(mutableOwner: MutableOwner) : IB = {
    val originalOwner = mutableOwner.value
    mutableOwner.value = ifBlock
    block
    mutableOwner.value = originalOwner
    ifBlock
  }

  implicit class NewVarExtender[DF <: DFAny.NewVar](newVar : DF) {
    final object ifdf {
      def apply(cond: DFBool)(block: => DF#TVal)(implicit ctx : DFIfBlock.Context): DFIfBlock[DF#TVal] =
        new DFIfBlock(cond, block)
    }

  }
  final object ifdf {
    def apply(cond: DFBool)(block: => Unit)(implicit ctx : DFIfBlock.Context): DFIfBlock[Unit] =
      new DFIfBlock(cond, block)
  }

  def constructCodeString : String = designDB.addDesignCodeString(typeName, bodyCodeString, this)

  def valCodeString : String = s"\nval $name = new $constructCodeString {}"
  final override def refCodeString(implicit callOwner: DSLOwnerConstruct): String = super.refCodeString

  override protected def discoveryDepenencies : List[Discoverable] =
    if (isTop) portsOut ++ super.discoveryDepenencies else super.discoveryDepenencies

  override def codeString: String = {
    val valCode = valCodeString
    if (isTop) s"${designDB.codeString}\n$valCode" else valCode
  }


}

object DFDesign {
  protected[DFiant] type Context = DFBlock.Context
  type ContextOf[+T] = DFBlock.ContextOf[T, DFDesign]
  private[DFiant] class DB {
    case class Info(id : Int, designs : ListBuffer[DFDesign])
    private val db = HashMap.empty[String, HashMap[String, Info]]
    def actualTypeName(designTypeName : String, info : Info) : String =
      if (info.id == 0) designTypeName else designTypeName + "$" + info.id
    def addDesignCodeString(designTypeName : String, designBodyString : String, design : DFDesign) : String = {
      val csHM = db.getOrElseUpdate(designTypeName, HashMap.empty[String, Info])
      val info = csHM.getOrElseUpdate(designBodyString, Info(csHM.size, ListBuffer.empty))
      info.designs += design
      actualTypeName(designTypeName, info)
    }
    def designTraitCodeString(designTypeName : String, designBodyString : String, info : Info) : String =
      s"\ntrait ${actualTypeName(designTypeName, info)} extends DFDesign {$designBodyString\n}"
    def codeString : String = {
      val ret = db.flatMap(e => {
        val designTypeName = e._1
        e._2.map(e => designTraitCodeString(designTypeName, e._1, e._2))
      }).mkString("\n")
      ret
    }
  }
}


abstract class DFComponent[Comp <: DFComponent[Comp]](implicit ctx : DFComponent.Context[Comp], args : sourcecode.Args)
  extends DFDesign with DSLFoldableOwnerConstruct {
  def foldedConstructCodeString : String = {
    ctx.compName.value + args.value.dropRight(1).map(e => e.map(f => f.value).mkString("(",", ",")")).mkString
  }

  final override private[DFiant] def unfoldedRun = {
    ctx.impl(this.asInstanceOf[Comp])
    folded = false
  }

  override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ portsIn //TODO: should be changed so that any DFComponent can set its own port dependencies
  final protected def setInitFunc[DFVal <: DFAny.Uninitialized](dfVal : DFVal)(value : Seq[dfVal.TToken])
  : Unit = dfVal.setInitFunc(value)
  final protected def getInit[DFVal <: DFAny.Uninitialized](dfVal : DFVal) : Seq[dfVal.TToken] = dfVal.getInit

  final override def constructCodeString : String = if (config.foldComponents) foldedConstructCodeString else super.constructCodeString
  final override def codeString : String = super.codeString

  final class InPortExtended(dfVal : DFAny.Port[_ <: DFAny, _ <: IN]) {
    def isOpen : Boolean = dfVal.connectedSource.isEmpty
  }
  final implicit def InPortExtended(dfVal: DFAny.Port[_ <: DFAny, _ <: IN]): InPortExtended = new InPortExtended(dfVal)
  override lazy val typeName: String = getClass.getSimpleName

}

object DFComponent {
  trait Context[Comp <: DFComponent[Comp]] extends DFBlock.ContextOf[Nothing, DFBlock] {
    implicit val impl : DFComponent.Implementation[Comp]
    val compName : sourcecode.Name.OfType[Comp]
  }
  trait LowPriority {
    implicit def evFromOpContext[Comp <: DFComponent[Comp]](
      implicit evContext : DFAny.Op.Context, evImpl : DFComponent.Implementation[Comp],
      evNameIt : NameIt, evCompName : sourcecode.Name.OfType[Comp]
    ) : Context[Comp] = new Context[Comp] {
      implicit val owner: DFBlock = evContext.owner
      implicit val impl: DFComponent.Implementation[Comp] = evImpl
      implicit val basicLib: DFBasicLib = evContext.basicLib
      implicit val config: DFAnyConfiguration = evContext.config
      val n: NameIt = evNameIt
      val compName = evCompName
    }
  }
  object Context extends LowPriority {
    implicit def ev[Comp <: DFComponent[Comp]](
      implicit evOwner : DFBlock, evImpl : DFComponent.Implementation[Comp], evBasicLib : DFBasicLib,
      evConfig : DFAnyConfiguration, evNameIt : NameIt, evCompName : sourcecode.Name.OfType[Comp]
    ) : Context[Comp] = new Context[Comp] {
      implicit val owner: DFBlock = evOwner
      implicit val impl: DFComponent.Implementation[Comp] = evImpl
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
      val compName = evCompName
    }
  }

  trait Implementation[Comp <: DFComponent[Comp]] {
    def apply(comp : Comp) : Unit
  }
}

