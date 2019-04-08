package DFiant

import DFiant.BasicLib.DFBasicLib
import internals._

abstract class DFBlock(implicit ctx0 : DFBlock.Context) extends DFAnyOwner with Implicits {self =>
  final private[DFiant] lazy val ctx = ctx0
  protected[DFiant] trait __DevDFBlock extends __DevDFAnyOwner {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ownership
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final val topDsn : DFDesign =
      ownerOption.map(o => o.asInstanceOf[DFBlock].topDsn).getOrElse(self.asInstanceOf[DFDesign])

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override protected def nameDefault: String = ctx.getName
    protected val designDB : DFDesign.DB =
      ownerOption.map(o => o.asInstanceOf[DFBlock].designDB).getOrElse(new DFDesign.DB)

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Simulation
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFiant] def hasSimMembers : Boolean = mutableMemberList.collectFirst{
      case m : DFAnySimMember => m
      case m : DFBlock if m.hasSimMembers => m
    }.nonEmpty
  }
  override private[DFiant] lazy val __dev : __DevDFBlock = ???
  import __dev._

  override type ThisOwner = DFBlock
  private[DFiant] implicit val mutableOwner : MutableOwner = new MutableOwner(this)
  final protected implicit val protInternalContext : DFBlock.InternalContext = DFBlock.InternalContext()
  override implicit def __theOwnerToBe : DFBlock = mutableOwner.value
  implicit val basicLib = ctx.basicLib

  final protected[DFiant] object ifdf extends ConditionalBlock.IfNoRetVal(mutableOwner)
  final protected[DFiant] object matchdf extends ConditionalBlock.MatchNoRetVal(mutableOwner)
  protected def selectdf[T <: DFAny](cond : DFBool)(thenSel : T, elseSel : T) : T = ???
  protected def selectdf[SW, T <: DFAny](sel : DFUInt[SW], default : => Option[T] = None)(args : List[T]) : T = ???

  protected object sim {
    final val Note = Severity.Note
    final val Warning = Severity.Warning
    final val Error = Severity.Error
    final val Failure = Severity.Failure
    def assert(cond : DFBool, msg : Message, severity : Severity = Warning) : Unit = {
      if (inSimulation) Assert(Some(cond), msg, severity)(ctx.updateOwner(__theOwnerToBe))
    }
    def report(msg : Message, severity : Severity = Note) : Unit = {
      if (inSimulation) Assert(None, msg, severity)(ctx.updateOwner(__theOwnerToBe))
    }
    def finish() : Unit = {
      if (inSimulation) Finish()(ctx.updateOwner(__theOwnerToBe))
    }
  }
}
object DFBlock {
  implicit def fetchDev(from : DFBlock)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
  trait ContextOf[+T, +Owner <: DFAnyOwner] extends DFAnyOwner.ContextWithLibOf[T, Owner] {
    self =>
    def updateOwner[Owner0 <: DFAnyOwner](owner0 : Owner0)(implicit n0 : NameIt) : ContextOf[T, Owner0] = new ContextOf[T, Owner0] {
      val ownerOption : Option[Owner0] = Some(owner0)
      implicit val basicLib: DFBasicLib = self.basicLib
      implicit val config: DFAnyConfiguration = self.config
      val n: NameIt = n0
    }
  }
  trait LowPriority {
    implicit def ev[T, Owner <: DFAnyOwner](
      implicit
      lp : shapeless.LowPriority,
      evOwner : Owner = null,
      evBasicLib : DFBasicLib,
      evConfig : DFAnyConfiguration,
      evNameIt : NameIt,
      forceNotVar : NameIt.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, Owner] = new ContextOf[T, Owner] {
      val ownerOption : Option[Owner] = Option(evOwner)
      implicit val basicLib: DFBasicLib = evBasicLib
      implicit val config: DFAnyConfiguration = evConfig
      val n: NameIt = evNameIt
    }
  }
  private[DFiant] case class InternalContext()
  object ContextOf extends LowPriority {
    implicit def evContext[T, T2](
      implicit
      lp : shapeless.LowPriority,
      evContext : DFDesign.ContextOf[T2],
      external : shapeless.Refute[InternalContext],
      forceNotVar : NameIt.ForceNotVar[ContextOf[_,_]]
    ) : ContextOf[T, DFBlock] = new ContextOf[T, DFBlock] {
      val ownerOption : Option[DFBlock] = evContext.ownerOption
      implicit val basicLib : DFBasicLib = evContext.basicLib
      implicit val config : DFAnyConfiguration = evContext.config
      val n : NameIt = evContext.n
    }
  }
  type Context = ContextOf[Nothing, DFBlock]
}



class MutableOwner(var value : DFBlock)

