package DFiant
package compiler
package backend
package verilog

import DFiant.internals.BigIntExtrasCO
import constraints.timing.sync.{ClockParams, ResetParams}
import DFiant.sim._

import scala.collection.mutable
import printer.formatter._
import RTL.Analysis

final class Compiler[D <: DFDesign](c : IRCompilation[D]) {
  private def isSyncMember(member : DFMember)(implicit printer : Printer) : Boolean = member match {
    case RTL.IfBlock(_,_) | RTL.ElseIfBlock(_,_) => true
    case _ => false
  }

  private def getProcessStatements(block : DFBlock, filterFunc : DFMember => Boolean = _ => true)(
    implicit printer : Printer
  ) : List[String] = {
    import printer.config._
    val simGuardSetOption : Option[String] =
      if (printer.inSimulation) Some(Verilator.ifndef(s"${Sim.guardName} = $LIT 1;"))
      else None
    val (_, statements) = printer.getSet.designDB.blockMemberTable(block).filter(filterFunc).foldRight(("", List.empty[String])) {
      case (cb @ DFConditional.IfElseBlock(None,Some(_),_,_), (_, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb @ RTL.ElseIfBlock(_,_), (closing, statements)) =>
        (If.Else(getProcessStatements(cb) ++ simGuardSetOption), statements)
      case (cb @ DFConditional.IfElseBlock(Some(condRef),Some(_),_,_), (closing, statements)) =>
        (If.ElsIf(Value.ref(condRef.get), getProcessStatements(cb), closing), statements)
      case (cb @ RTL.IfBlock(RTL.IsClock(),_), (closing, statements)) =>
        ("", getProcessStatements(cb) ++ statements ++ simGuardSetOption)
      case (cb @ DFConditional.IfElseBlock(Some(condRef),None,_,_), (closing, statements)) =>
        ("", If(Value.ref(condRef.get), getProcessStatements(cb), closing) :: statements)
      case (cb @ DFConditional.CaseBlock(_,_,None,_,_), (_, statements)) =>
        (Case.Item(Case.Choice.Default(), getProcessStatements(cb)), statements)
      case (cb @ DFConditional.CaseBlock(matchHeaderRef,_,Some(pattern),_,_), (items, statements)) =>
        val width = matchHeaderRef.get.matchValRef.get.width
        val item = Case.Item(Case.Choice.Pattern(pattern, width), getProcessStatements(cb))
        (if (items.isEmpty) item else s"$item\n$items", statements)
      case (mh : DFConditional.MatchHeader, (items, statements)) =>
        ("", Case(Value.ref(mh.matchValRef.get), items, printer.getSet.designDB.caseWithDontCare(mh)) :: statements)
      case (Net.Internal(netStr), (closing, statements)) => (closing, netStr :: statements)
      case (Sim.Assert(assertStr), (closing, statements)) => (closing, assertStr :: statements)
      case (Sim.Finish(finishStr), (closing, statements)) => (closing, finishStr :: statements)
      case (_, nochange) => nochange
    }
    statements
  }

  private def matchForceCover(getSet : MemberGetSet) : Iterable[DFConditional.MatchHeader] = {
    implicit val __getSet : MemberGetSet = getSet
    val matchHeaders = getSet.designDB.members.collect{case mh : DFConditional.MatchHeader => mh}
    matchHeaders.filter { mh =>
      mh.matchValRef.get match {
        //For enumeration only, we check the bits-width exhaustively coverage, since RTL enumeration
        //is more limited than the DFiant enumeration coverage check. In Verilog, all enumerations are
        //manually encoded, so bits-width coverage is a must.
        case DFEnum(entries) if BigInt.maxUnsignedFromWidth(entries.width) > entries.all.size => true
        //Non-enumeration exhaustively coverage is already handled in the ExplicitPrev stage
        case _ => false
      }
    }
  }

  //save names for for-loop iterators
  private val for_iterators : Set[String] = Set("for_i", "for_j", "for_k")

  def verilogCompile[R <: Revision](implicit revision : R) = {
    val designDB =
      c.dropUnreferenced
       .fixAnonymous
       .flattenNames
//       .flattenStructs
       .moveCBDesigns
//       .controlDesigns
       .orderMembers(OrderMembers.Order.LazyConnectionLast)
       .explicitPrev
       .forceOthersCaseCoverage(matchForceCover)
       .carryMathConversion
       .uniqueDesigns
       .namedSelection
       .uniqueNames(reservedKeywords ++ for_iterators + Sim.guardName, caseSensitive = true)
       .toRTLForm
       .viaPortConnection
       .orderMembers
       .db

    import designDB.__getset

    implicit val printer : Printer = new Printer {
      val getSet : MemberGetSet = __getset
      val config : Printer.Config = new Printer.Config(revision)
    }
    import printer.config.{KW, FN, LIT, OP, TP}
    val designTypes = mutable.Set.empty[String]
    val files = designDB.designMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design : DFDesign.Block, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val (ports, wires, regs) = members.foldRight((List.empty[String],List.empty[String],List.empty[String])){
          case (p @ DFAny.Port.In(), (ports, wires, regs)) =>
            (Port(p.name, Port.Dir.In(), Type(p.dfType), Type.arrayDim(p.dfType)) :: ports, wires, regs)
          case (p @ DFAny.Port.Out(), (ports, wires, regs)) =>
            (Port(p.name, Port.Dir.Out(), Type(p.dfType), Type.arrayDim(p.dfType)) :: ports, wires, regs)
          case (RTL.IsClock() | RTL.IsReset(), pwr) if design.isTop && printer.inSimulation => pwr
          case (s : DFAny.Member, (ports, wires, regs)) if !s.isAnonymous => designDB.getConnectionTo(s) match {
            case Some(n) if n.fromRef.isPortOut => (ports, Wire(s.name, Type(s.dfType), Type.arrayDim(s.dfType)) :: wires, regs)
            case _ => (ports, wires, Reg(s.name, Type(s.dfType), Init(s), Type.arrayDim(s.dfType)) :: regs)
          }
          case (_, pwr) => pwr
        }
        val clkRstMembers = members.collect {
          case m @ RTL.IsClock() => m
          case m @ RTL.IsReset() => m
        }
        val enumInstances =
          if (printer.inSimulation) members.collect {
            case DFEnum(entries) => entries
          }.distinct.map(e => EnumInstance(e))
          else Nil
        val clkrstPorts = if (design.isTop && printer.inSimulation)
          Verilator.ifdef(clkRstMembers.map(cr => Port(cr.name, Port.Dir.In(), Type(cr.dfType))).mkString(",\n")) :: ports
        else ports
        object ClkSim {
          def unapply(clk : DFAny.Member) : Option[String] = clk match {
            case RTL.IsClock() =>
              val reg = Reg(clk.name, Type(clk.dfType), Init(clk))
              val sim = s"$KW always #5 ${Value.ref(clk)} = $OP!${Value.ref(clk)};"
              Some(s"$reg\n$sim")
            case _ => None
          }
        }
        object RstSim {
          def unapply(rst : DFAny.Member) : Option[String] = rst match {
            case RTL.IsReset() =>
              val reg = Reg(rst.name, Type(rst.dfType), Init(rst))
              val sim = s"$KW initial #10 ${Value.ref(rst)} = $LIT${ResetParams.get.inactiveInt};"
              Some(s"$reg\n$sim")
            case _ => None
          }
        }
        val simGuardRegOption : Option[String] =
          if (printer.inSimulation && clkRstMembers.nonEmpty) Some(Verilator.ifndef(Reg(Sim.guardName, "", s" = $LIT 0")))
          else None
        val simGuardClearOption : Option[String] =
          if (printer.inSimulation && clkRstMembers.nonEmpty) Some(Verilator.ifndef(s"${Sim.guardName} = $LIT 0;"))
          else None

        val clkrstRegs = if (design.isTop && printer.inSimulation)
          Verilator.ifndef(clkRstMembers.map{case ClkSim(s) => s case RstSim(s) => s}.mkString("\n")) :: regs ++ simGuardRegOption
        else regs ++ simGuardRegOption

        val moduleName = design.designType
        val moduleInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.blockMemberTable(x).collect {
              case Net.External(netStr) => netStr
            }
            ModuleInstance(x.name, x.designType, connections)
        }
        val asyncStatements = getProcessStatements(design, !isSyncMember(_)) ++ simGuardClearOption
        val asyncSensitivityList : String = revision match {
          case Revision.V95 => AlwaysBlock.Sensitivity(designDB.getSensitivityList(design))
          case Revision.V2001 => AlwaysBlock.Sensitivity(Nil)
        }
        //TODO: handle general case of multi-dimensional arrays or arrays of arrays
        val iteratorDcls = members.collectFirst {
          case DFVector(_,_) => s"$TP integer for_i;"
        }
        val asyncBlock = AlwaysBlock(asyncSensitivityList, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncSensitivityList = AlwaysBlock.Sensitivity(members.collect {
          case cb @ RTL.IfBlock(clkOrRst, edge) if cb.getOwner == design =>
            if (edge) s"$KW posedge ${clkOrRst.name}"
            else s"$KW negedge ${clkOrRst.name}"
          case RTL.ElseIfBlock(clk, edge) =>
            if (edge) s"$KW posedge ${clk.name}"
            else s"$KW negedge ${clk.name}"
        })
        val emits = members.collect {
          case Emitter(emitStr) => emitStr
        }.mkString("\n")
        val syncProcess = AlwaysBlock(syncSensitivityList, syncStatements)
        val localEnumEntries = designDB.getLocalEnumEntries(design)
        val enumDefines = localEnumEntries.map(e => EnumEntriesDcl.defines(e)).toList
        val enumModuleDcls =
          if (printer.inSimulation) localEnumEntries.map(e =>
            s"""/* verilator lint_off DECLFILENAME */
               |${EnumEntriesDcl(e)}""".stripMargin
          ).toList
          else Nil
        val declarations =
          wires ++ enumDefines ++ clkrstRegs ++ enumInstances ++ iteratorDcls ++ moduleInstances :+ asyncBlock ++ syncProcess ++ emits
        val modules = enumModuleDcls :+ Module(moduleName, clkrstPorts, declarations)
        val file = File(GlobalDefsFile.Name(), "", modules.mkString(s"\n$EMPTY\n"))
        Some(BackendStage.File(s"${moduleName}.v", s"$file"))
      case _ => None
    }
    val globalDefsFile = BackendStage.File(s"${GlobalDefsFile.Name()}.v", GlobalDefsFile())
    BackendStage.Compilation[D, Backend[R]](c.dsn, designDB, globalDefsFile :: files)
  }
}