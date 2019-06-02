package DFiant

import DFiant.BasicLib.DFBasicLib
import DFiant.compiler.Backend
import DFiant.internals._

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {self =>
  protected[DFiant] trait __DevDFDesign extends __DevDFBlock with __DevDFInterface {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Conditional Block
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    //The block by value object is created within the context of the current DFDesign,
    //so we mutate `__theOwnerToBe` via mutableOwner which is passed to the IfBlock constructs
    private[DFiant] def injectConditionalBlock[IB <: DFDesign](ifBlock : IB, block: => Unit)(mutableOwner: MutableOwner) : IB = {
      val originalOwner = mutableOwner.value
      mutableOwner.value = ifBlock
      block
      mutableOwner.value = originalOwner
      ifBlock
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected def designType : String = typeName
    private var _designDB : Option[DFDesign.DB] = None
    protected[DFDesign] def designDB : DFDesign.DB = if (isTop) _designDB.get else topDsn.__dev.designDB
    private[DFiant] def constructCodeString : String = designDB.addOwnerBody(designType, bodyCodeString, self)

    private[DFiant] def valCodeString : String = s"\nval $name = new $constructCodeString {}"
    //  final override def refCodeString(implicit callOwner: DSLOwnerConstruct): String = super.refCodeString

    override def codeString: String = {
      elaborate()
      _designDB = Some(new DFDesign.DB)
      val valCode = valCodeString
      if (isTop) s"$designDB\n$valCode" else valCode
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override protected def discoveryDependencies : List[Discoverable] =
      if (isTop) portsOut ++ super.discoveryDependencies else super.discoveryDependencies
  }
  override private[DFiant] lazy val __dev : __DevDFDesign = new __DevDFDesign {}
  import __dev._

  final override implicit def __theOwnerToBe : DFDesign = mutableOwner.value.asInstanceOf[DFDesign]

  protected def atOwnerDo[T](block : => T) : T = {
    val originalOwner = mutableOwner.value
    mutableOwner.value = owner.asInstanceOf[DFBlock]
    val ret = block
    mutableOwner.value = originalOwner
    ret
  }

  final lazy val isTop : Boolean = __dev.isTop

  private def openInputsCheck() : Unit = getDiscoveredMembers.collect {
    case p : DFAny.Port[_,_] if p.dir.isIn && !isTop && !p.isConnected && p.initLB.get.isEmpty =>
      throw new IllegalArgumentException(s"\nFound an uninitialized open input port: ${p.fullName}")
  }
  private lazy val init : Unit = {
    elaborate()
    openInputsCheck()
  }
  final def printCodeString : this.type = {println(codeString); this}
  def compileToVHDL : Backend.VHDL = {elaborate(); new Backend.VHDL(this)}
  final def printVHDLString : this.type = {compileToVHDL.print(); this}
}

object DFDesign {
  private[DFiant] type Context = DFBlock.Context
  trait ContextOf[+T] {
    val ownerOption : Option[DFBlock]
    val basicLib: DFBasicLib
    val config : DFAnyConfiguration
    val n : NameIt
  }
  object ContextOf {
    implicit def ev[T](
      implicit
      evOwner : DFBlock = null,
      evBasicLib : DFBasicLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextOf[_]]
    ) : ContextOf[T] = new ContextOf[T] {
      val ownerOption : Option[DFBlock] = Option(evOwner)
      val basicLib: DFBasicLib = evBasicLib
      val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }
  private[DFiant] class DB extends DSLOwnerConstruct.DB[DFDesign, String] {
    def ownerToString(designTypeName : String, designBodyString : String) : String =
      s"\ntrait $designTypeName extends DFDesign {$designBodyString\n}"
  }
}




