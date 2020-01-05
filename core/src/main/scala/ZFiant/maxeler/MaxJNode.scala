package ZFiant.maxeler
import ZFiant._

case class MaxJNode(
  design : DFDesign,
  pullInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  pullOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]],
  pushInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  pushOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]],
  scalarInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  scalarOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]]
) {
  private val designDB = design.db
  private val pullInZ = (pullInputs, pullInputs.map(p => new DFDesign() {
    final val data = DFAny.Port.In(p.dfType) setNamePrefix(s"${p.name}_")
    final val empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val almost_empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val read = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val pullOutZ = (pullOutputs, pullOutputs.map(p => new DFDesign() {
    final val data = DFAny.Port.Out(p.dfType) setNamePrefix(s"${p.name}_")
    final val empty = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val almost_empty = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val read = DFBool() <> IN setNamePrefix(s"${p.name}_")
  })).zipped
  private val pushInZ = (pushInputs, pushInputs.map(p => new DFDesign() {
    final val data = DFAny.Port.In(p.dfType) setNamePrefix(s"${p.name}_")
    final val stall = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val valid = DFBool() <> IN setNamePrefix(s"${p.name}_")
  })).zipped
  private val pushOutZ = (pushOutputs, pushOutputs.map(p => new DFDesign() {
    final val data = DFAny.Port.Out(p.dfType) setNamePrefix(s"${p.name}_")
    final val stall = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val valid = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val scalaInZ = (scalarInputs, scalarInputs.map(p => new DFDesign() {
    final val reg = p.prev() setNamePrefix(s"${p.name}_")
  })).zipped

  private val control = new DFDesign() {
    final val ready = !pullInZ.head._2.empty && !pushOutZ.head._2.stall
    pushOutZ.head._2.valid := false
    final val guard = ifdf(ready){
      pushOutZ.head._2.valid := true
    }
  }

  val db : DFDesign.DB = {
    import designDB.getset
    import DFDesign.DB.Patch
    import DFCompiler._
    val extendedPortsDB = designDB
      .patch(pullInZ.map((p, e) => p -> Patch.Add(e, Patch.Add.Config.Replace)))
      .patch(pushOutZ.map((p, e) => p -> Patch.Add(e, Patch.Add.Config.Replace)))
      .patch(scalaInZ.map((p, e) => p -> Patch.Replace(e.reg, Patch.Replace.Config.ChangeRefOnly)))
      .patch(scalaInZ.map((p, e) => p -> Patch.Add(e, Patch.Add.Config.After)))
      .moveConnectableFirst
    val guardedMembers = designDB.ownerMemberTable(design.block).collect{case m : CanBeGuarded => m}
    val guardedDB = extendedPortsDB
      .patch(guardedMembers.head -> Patch.Add(control.db, Patch.Add.Config.Before))
      .patch(guardedMembers.map(m => m -> Patch.ChangeRef(m, (m : DFMember) => m.ownerRef, control.guard)))
    guardedDB
  }

  private val instName : String = design.block.name
  private val packName : String = design.block.name
  private val className : String = s"${design.typeName}Node"
  private val vhdlName : String = s"${design.typeName}Source"
  private val clkName : String = "clk"
  private val rstName : String = "rst"

  val pullInStr : String = pullInputs.map(p => s"""\n\t\taddInputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PULL, 1);""").mkString
  val pullOutStr : String = pullOutputs.map(p => s"""\n\t\taddOutputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PULL, 1);""").mkString
  val pushInStr : String = pushInputs.map(p => s"""\n\t\taddInputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PUSH, 2);""").mkString
  val pushOutStr : String = pushOutputs.map(p => s"""\n\t\taddOutputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PUSH, 2);""").mkString
  val scalarInStr : String = scalarInputs.map(p => s"""\n\t\taddScalarInput("${p.name}", ${p.width});""").mkString
  val scalarOutStr : String = scalarOutputs.map(p => s"""\n\t\taddScalarOutput("${p.name}", ${p.width});""").mkString

  val nodeMaxJString : String =
    s"""
       |package $packName;
       |
       |import com.maxeler.maxcompiler.v2.managers.custom.CustomManager;
       |import com.maxeler.maxcompiler.v2.managers.custom.blocks.CustomHDLNode;
       |
       |final class $className extends CustomHDLNode {
       |	ScalarHDLNode(CustomManager manager, String instance_name) {
       |		super(manager, instance_name, "$instName");
       |
       |		CustomNodeClock nodeClock = addClockDomain("$clkName");
       |		nodeClock.setNeedsReset("$rstName");
       |		$pullInStr$pullOutStr$pushInStr$pushOutStr$scalarInStr$scalarOutStr
       |
       |		addVHDLSource("$vhdlName.vhdl", false);
       |	}
       |}
       |""".stripMargin
}
