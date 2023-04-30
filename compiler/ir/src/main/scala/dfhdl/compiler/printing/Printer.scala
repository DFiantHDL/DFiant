package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*
import scala.collection.mutable
import analysis.*
import java.io.FileWriter
import java.nio.file.Paths

protected trait AbstractPrinter:
  type TPrinter <: Printer
  given printer: TPrinter
  given getSet: MemberGetSet

trait Printer
    extends AbstractTypePrinter,
      AbstractTokenPrinter,
      AbstractValPrinter,
      AbstractOwnerPrinter:
  def csViaConnectionSep: String
  val normalizeViaConnection: Boolean
  val normalizeConnection: Boolean
  def csAssignment(lhsStr: String, rhsStr: String): String
  def csNBAssignment(lhsStr: String, rhsStr: String): String
  def csConnection(lhsStr: String, rhsStr: String, directionStr: String): String
  def csViaConnection(lhsStr: String, rhsStr: String, directionStr: String): String
  def csLazyConnection(lhsStr: String, rhsStr: String, directionStr: String): String
  final def csDFNet(net: DFNet): String =
    // True if the net needs to be shown in a swapped order.
    // Normalized late connections always have the internal port on the LHS.
    // Normalized connections always have the receiver port on the LHS.
    val swapLR = net match
      // swapped if the net is a late construction and the RHS is the internal port
      case _ if net.isViaConnection =>
        normalizeViaConnection && net.rhsRef.get.isSameOwnerDesignAs(net)
      // swapped if the net is a regular connection and the RHS is receiver
      case DFNet.Connection(_, _, swapped) =>
        swapped && normalizeConnection
      case _ => false
    val directionStr =
      net.lhsRef.get match
        case dfIfc: DFInterfaceOwner => "<->"
        case dfVal: DFVal =>
          if (dfVal.getConnectionTo.contains(net) ^ swapLR) "<--"
          else "-->"
    val (lhsRef, rhsRef) = if (swapLR) (net.rhsRef, net.lhsRef) else (net.lhsRef, net.rhsRef)
    val lhsStr = lhsRef.refCodeString
    val rhsStr = rhsRef.refCodeString
    net.op match
      case DFNet.Op.Assignment     => csAssignment(lhsStr, rhsStr)
      case DFNet.Op.NBAssignment   => csNBAssignment(lhsStr, rhsStr)
      case DFNet.Op.Connection     => csConnection(lhsStr, rhsStr, directionStr)
      case DFNet.Op.ViaConnection  => csViaConnection(lhsStr, rhsStr, directionStr)
      case DFNet.Op.LazyConnection => csLazyConnection(lhsStr, rhsStr, directionStr)
    end match
  end csDFNet
  def csTimeUnit(time: Time): String = s"${time.usec}.us"
  def csFreqUnit(freq: Freq): String = s"${freq.hertz}.Hz"
  def csRatioUnit(ratio: Ratio): String = s"${ratio.value}"
  def csTimer(timer: Timer): String
  def csNameCfg(nameCfg: NameCfg): String =
    nameCfg match
      case _: DerivedCfg.type => "DerivedCfg"
      case name: String       => s""""$name""""
  def csClkEdgeCfg(edgeCfg: ClkCfg.EdgeCfg): String =
    edgeCfg match
      case _: DerivedCfg.type => "DerivedCfg"
      case edge: ClkCfg.Edge =>
        edge match
          case _: ClkCfg.Edge.Rising.type  => "ClkCfg.Edge.Rising"
          case _: ClkCfg.Edge.Falling.type => "ClkCfg.Edge.Falling"
  def csClkCfg(clkCfg: ClkCfg): String =
    clkCfg match
      case _: DerivedCfg.type => "DerivedCfg"
      case _: None.type       => "None"
      case ClkCfg.Explicit(edge) =>
        s"ClkCfg(${csClkEdgeCfg(edge)})"
  def csRstModeCfg(modeCfg: RstCfg.ModeCfg): String =
    modeCfg match
      case _: DerivedCfg.type => "DerivedCfg"
      case mode: RstCfg.Mode =>
        mode match
          case _: RstCfg.Mode.Sync.type  => "RstCfg.Mode.Sync"
          case _: RstCfg.Mode.Async.type => "RstCfg.Mode.Async"
  def csRstActiveCfg(activeCfg: RstCfg.ActiveCfg): String =
    activeCfg match
      case _: DerivedCfg.type => "DerivedCfg"
      case active: RstCfg.Active =>
        active match
          case _: RstCfg.Active.High.type => "RstCfg.Active.High"
          case _: RstCfg.Active.Low.type  => "RstCfg.Active.Low"
  def csRstCfg(rstCfg: RstCfg): String =
    rstCfg match
      case _: DerivedCfg.type => "DerivedCfg"
      case _: None.type       => "None"
      case RstCfg.Explicit(mode, active) =>
        s"RstCfg(${csRstModeCfg(mode)}, ${csRstActiveCfg(active)})"
  def csRTDomainCfg(clkCfg: ClkCfg, rstCfg: RstCfg): String =
    s"""RTDomainCfg(
       |    clkCfg = ${printer.csClkCfg(clkCfg)},
       |    rstCfg = ${printer.csRstCfg(rstCfg)}
       |)""".stripMargin
  def csRTDomainCfg(cfg: RTDomainCfg): String =
    cfg match
      case _: DerivedCfg.type => "DerivedCfg"
      case RTDomainCfg.Explicit(name, clkCfg, rstCfg) =>
        if (name.isEmpty) csRTDomainCfg(clkCfg, rstCfg)
        else name
  def csCommentInline(comment: String): String
  def csCommentEOL(comment: String): String
  def csDocString(doc: String): String
  final def csDocString(meta: Meta): String =
    meta.docOpt.map(printer.csDocString).map(x => s"$x\n").getOrElse("")
  final def csDFMember(member: DFMember): String =
    val cs = member match
      case dfVal: DFVal.CanBeExpr if dfVal.isAnonymous => csDFValExpr(dfVal)
      case dfVal: DFVal                                => csDFValNamed(dfVal)
      case net: DFNet                                  => csDFNet(net)
      case design: DFDesignBlock                       => csDFDesignBlockInst(design)
      case pb: ProcessBlock                            => csProcessBlock(pb)
      case domain: DomainBlock                         => csDomainBlock(domain)
      case timer: Timer                                => csTimer(timer)
      case _                                           => ???
    s"${printer.csDocString(member.meta)}$cs"
  def designFileName(designName: String): String
  def globalFileName: String
  def csGlobalFileContent: String = csGlobalTypeDcls
  val alignEnable = true
  def alignCode(cs: String): String
  val colorEnable = true
  def colorCode(cs: String): String
  import io.AnsiColor._
  val keywordColor: String = s"$BLUE$BOLD"
  val keyword2Color: String = s"$MAGENTA$BOLD"
  val typeColor: String = "\u001B[38;5;94m"
  final def formatCode(cs: String): String =
    val alignedContents = if (alignEnable) alignCode(cs) else cs
    if (colorEnable) colorCode(alignedContents) else alignedContents
  final def csFile(design: DFDesignBlock): String =
    s"${csDocString(design.dclMeta)}${csDFDesignBlockDcl(design)}"
  final def printedDB: DB =
    val designDB = getSet.designDB
    val uniqueDesigns = mutable.Set.empty[String]
    val globalSourceFile =
      SourceFile(SourceType.Compiled, globalFileName, formatCode(csGlobalFileContent))
    val compiledFiles = globalSourceFile :: designDB.designMemberList.collect {
      case (block: DFDesignBlock, _) if !uniqueDesigns.contains(block.dclName) =>
        uniqueDesigns += block.dclName
        SourceFile(
          SourceType.Compiled,
          designFileName(block.dclName),
          formatCode(csFile(block))
        )
    }
    // removing existing compiled/committed files and adding the newly compiled files
    val srcFiles = designDB.srcFiles.filter {
      case SourceFile(SourceType.Compiled | SourceType.Committed, _, _) => false
      case _                                                            => true
    } ++ compiledFiles
    designDB.copy(srcFiles = srcFiles)
  end printedDB

  final def csDB: String =
    val designDB = getSet.designDB
    val uniqueDesigns = mutable.Set.empty[String]
    val csFileList = designDB.designMemberList.collect {
      case (block: DFDesignBlock, members) if !uniqueDesigns.contains(block.dclName) =>
        uniqueDesigns += block.dclName
        csFile(block)
    }
    val csFiles = s"${csGlobalFileContent.emptyOr(v => s"$v\n")}${csFileList.mkString("\n")}\n"
    if (alignEnable) alignCode(csFiles) else csFiles
  end csDB
end Printer

object Printer:
  def printGenFiles(db: DB): Unit =
    db.srcFiles.foreach {
      case srcFile @ SourceFile(SourceType.Compiled | SourceType.Committed, path, contents) =>
        println("==========================================================")
        println(srcFile.sourceType)
        println(path)
        println("==========================================================")
        println(contents)
        println("")
      case _ =>
    }
  def toFolder(db: DB, folderPathStr: String): DB =
    val updatedSrcFiles = db.srcFiles.map {
      case srcFile @ SourceFile(SourceType.Compiled, filePathStr, contents) =>
        val finalPathStr =
          if (Paths.get(filePathStr).isAbsolute) filePathStr
          else Paths.get(folderPathStr).resolve(filePathStr).toAbsolutePath.normalize().toString
        val pw = new FileWriter(finalPathStr)
        pw.write(contents.decolor)
        pw.close()
        srcFile.copy(sourceType = SourceType.Committed, path = finalPathStr)
      case other => other
    }
    db.copy(srcFiles = updatedSrcFiles)
end Printer

class DFPrinter(using val getSet: MemberGetSet)
    extends Printer,
      DFTypePrinter,
      DFTokenPrinter,
      DFValPrinter,
      DFOwnerPrinter:
  type TPrinter = DFPrinter
  given printer: TPrinter = this
  def csViaConnectionSep: String = ""
  def csAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr := $rhsStr"
  def csNBAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr :== $rhsStr"
  def csConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr <> $rhsStr"
  def csViaConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"this.$lhsStr <>/*$directionStr*/ $rhsStr"
  def csLazyConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr `<LZ>`/*$directionStr*/ $rhsStr"
  val normalizeViaConnection: Boolean = true
  val normalizeConnection: Boolean = true
  // to remove ambiguity in referencing a port inside a class instance we add `this.` as prefix
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"// $comment"
  def csDocString(doc: String): String = doc.betterLinesIterator.mkString("/**", "\n  *", "*/")
  def csTimer(timer: Timer): String =
    val timerBody = timer match
      case p: Timer.Periodic =>
        (p.triggerRef.get, p.periodOpt) match
          case (DFMember.Empty, None)         => "Timer()"
          case (DFMember.Empty, Some(period)) => s"Timer(${csTimeUnit(period)})"
          case (trigger: DFVal, None) =>
            s"Timer(${p.triggerRef.refCodeString})"
          case (trigger: DFVal, Some(period)) =>
            s"Timer(${p.triggerRef.refCodeString},${csTimeUnit(period)})"
          case _ => ??? // impossible
      case f: Timer.Func =>
        val argStr = f.arg match
          case r: Ratio => csRatioUnit(r)
          case t: Time  => csTimeUnit(t)
        s"${f.sourceRef.refCodeString} ${f.op} $argStr"
    if (timer.isAnonymous) timerBody else s"val ${timer.name} = $timerBody"
  end csTimer
  def globalFileName: String = s"${getSet.designDB.top.dclName}_globals.scala"
  def designFileName(designName: String): String = s"$designName.scala"
  def alignCode(cs: String): String =
    cs
      .align("[ \\t]*val .*", "=", ".*<>.*")
      .align("[ \\t]*val .*", "<>", ".*")
      .align("[ \\t]*val .*<>.*", "init", ".*")
      .align("[ ]*[a-zA-Z0-9_.]+[ ]*", ":=|<>|:==", ".*")
      .align("[ ]*[a-zA-Z0-9_.]+[ ]*(?::=|<>|:==)", " ", ".*")
      // align enums
      .align("[ ]*case [a-zA-Z0-9_]+[ ]*", "extends", ".*")
      // align cases
      .align("[ ]*case [a-zA-Z0-9_.]+[ ]*", "=>", ".*")

  import io.AnsiColor._
  val scalaKW: Set[String] =
    Set("class", "end", "enum", "extends", "new", "object", "val", "if", "else", "match", "case",
      "final")
  val dfhdlKW: Set[String] =
    Set("VAR", "REG", "WIRE", "IN", "OUT", "INOUT", "VAL", "DFDesign", "RTDesign", "EDDesign",
      "DFDomain", "RTDomain", "EDDomain", "process", "forever", "all")
  val dfhdlOps: Set[String] = Set("<>", ":=", ":==")
  val dfhdlTypes: Set[String] =
    Set("Bit", "Boolean", "UInt", "SInt", "Bits", "X", "Encode", "Struct", "Opaque", "StartAt",
      "OneHot", "Grey")
  def colorCode(cs: String): String =
    cs
      .colorWords(scalaKW, keywordColor)
      .colorWords(dfhdlKW, keyword2Color)
      .colorOps(dfhdlOps, keyword2Color)
      .colorWords(dfhdlTypes, typeColor)
end DFPrinter

extension (member: DFMember)(using printer: Printer)
  def codeString: String =
    printer.csDFMember(member)
extension (dfType: DFType)(using printer: DFTypePrinter)
  def codeString: String =
    printer.csDFType(dfType)
extension (token: DFTokenAny)(using printer: DFTokenPrinter)
  def codeString: String =
    printer.csDFToken(token)

def DefaultPrinter(using MemberGetSet): Printer = new DFPrinter:
  override val alignEnable: Boolean = false
