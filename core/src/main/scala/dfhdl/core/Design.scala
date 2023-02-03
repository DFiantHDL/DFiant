package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import ir.DFDesignBlock.InstMode
import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

private[dfhdl] abstract class Design(using DFC) extends Container, HasNamePos:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[core] def mkInstMode(args: ListMap[String, Any]): InstMode = InstMode.Normal
  final private[core] def initOwner: TOwner =
    Design.Block(__domainType, "???", Position.unknown, InstMode.Normal)
  final protected def setClsNamePos(
      name: String,
      position: Position,
      args: ListMap[String, Any]
  ): Unit =
    val designBlock = owner.asIR
    setOwner(
      dfc.getSet.replace(designBlock)(
        designBlock.copy(dclName = name, dclPosition = position, instMode = mkInstMode(args))
      ).asFE
    )
  final override def onCreateStartLate: Unit =
    dfc.enterLate()
end Design

object Design:
  import ir.DFDesignBlock.InstMode
  type Block = DFOwner[ir.DFDesignBlock]
  object Block:
    def apply(domain: ir.DomainType, dclName: String, dclPosition: Position, instMode: InstMode)(
        using DFC
    ): Block =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFRef.OneWay.Empty)
      ir.DFDesignBlock(
        domain,
        dclName,
        dclPosition,
        instMode,
        ownerRef,
        dfc.getMeta,
        ir.DFTags.empty
      )
        .addMember
        .asFE
    end apply
  end Block

end Design

abstract class DFDesign(using DFC) extends Design:
  private[core] type TDomain = DFC.Domain.DF
  final protected given TDomain = DFC.Domain.DF
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.DF
