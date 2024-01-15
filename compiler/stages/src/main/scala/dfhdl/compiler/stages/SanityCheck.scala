package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.printing.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec
import scala.collection.mutable

case object SanityCheck extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def refCheck()(using MemberGetSet): Unit =
    val refTable = getSet.designDB.refTable
    val memberTable = getSet.designDB.memberTable
    var hasViolations: Boolean = false
    def reportViolation(msg: String): Unit =
      hasViolations = true
      println(msg)
    // checks for all members
    getSet.designDB.members.foreach { m =>
      // check for missing references
      if (
        m.getRefs.exists {
          case _: DFRef.Empty => false
          case r              => !refTable.contains(r)
        }
      )
        reportViolation(s"Missing ref for the member: $m")
      // check for missing origin references
      if (
        m.getRefs.collect { case tf: DFRef.TwoWayAny => tf.originRef }.exists {
          case _: DFRef.Empty => false
          case r              => !refTable.contains(r)
        }
      )
        reportViolation(s"Missing origin ref to the member: $m")
      var originRef: DFRefAny | Null = null
      var missingRef: DFRefAny | Null = null
      if (
        memberTable.getOrElse(m, Set.empty).collect { case r: DFRef.TwoWayAny =>
          (r, r.originRef)
        }
          .exists {
            case (_, _: DFRef.Empty) => false
            case (r, o) =>
              missingRef = r
              originRef = o
              !refTable.contains(o)
          }
      )
        reportViolation(s"Ref $missingRef missing origin ref $originRef to the member: $m")
      // check for circular references
      if (m.originRefs.exists(_.get == m))
        reportViolation(s"Circular reference for the member: $m")
      m match
        // check for missing owner references
        case m: DFDesignBlock if !m.isTop =>
          if (!refTable.contains(m.ownerRef))
            reportViolation(s"Missing owner ref for the member: $m")
        // check by-name selectors
        case pbns: DFVal.PortByNameSelect =>
          val design = pbns.designInstRef.get
          // check port existence
          getSet.designDB.portsByName(design).get(pbns.portNamePath) match
            case None =>
              reportViolation(
                s"Missing port ${pbns.portNamePath} for by-name port selection: ${pbns}"
              )
            case _ =>
          // check usage
          if (pbns.originRefs.isEmpty)
            reportViolation(s"No references to the by-name port selection: ${pbns}")
        // check references always refer to internal design instance ports via
        // by name selections and never directly
        case m =>
          m.getRefs.foreach { r =>
            r.get match
              case port @ DclPort() =>
                if (!m.isSameOwnerDesignAs(port))
                  reportViolation(
                    s"""|Direct internal port referencing instead of by-name selection found.
                        |Referencing member: $m
                        |Referenced port: $port""".stripMargin
                  )
              case _ =>
          }
      end match
      // check that anonymous values are referenced only once
      m match
        case dfVal: DFVal if dfVal.isAnonymous =>
          val deps = dfVal.getReadDeps
          if (deps.size > 1)
            reportViolation(
              s"""|An anonymous value has more than one reference.
                  |Referenced value: $dfVal
                  |Referencing members: ${deps.mkString("\n\t", "\n\t", "")}""".stripMargin
            )
        case _ =>
    }
    val memberSet = getSet.designDB.members.toSet
    // checks for all references
    refTable.foreach { (r, m) =>
      if (m != DFMember.Empty && !memberSet.contains(m))
        reportViolation(s"Ref $r exists for a removed member: $m")
    }
    require(!hasViolations, "Failed reference check!")
  end refCheck
  private def memberExistenceCheck()(using MemberGetSet): Unit =
    given Printer = DefaultPrinter
    val members = getSet.designDB.members
    val memberTable = getSet.designDB.memberTable
    val violations = members.flatMap {
      case n @ DFNet(toRef, DFNet.Op.Assignment, fromRef, _, _, _) =>
        val toMember = toRef.get
        val fromMember = fromRef.get
        val toValMissing = !memberTable.contains(toMember)
        val fromValMissing = fromMember match
          case _: DFVal.Const => false
          case _              => !memberTable.contains(fromMember)
        if (toValMissing)
          println(s"Foreign value ${toMember.getName} at net ${n.codeString}")
          members.collectFirst {
            case m: DFMember.Named if m.getName == toMember.getName => m
          } match
            case Some(value) => println(s"Found:\n$value\nInstead of:\n$toMember")
            case None        =>
        if (fromValMissing)
          println(s"Foreign value ${fromMember.getName} at net ${n.codeString}")
          members.collectFirst {
            case m: DFMember.Named if m.getName == fromMember.getName => m
          } match
            case Some(value) => println(s"Found:\n$value\nInstead of:\n$fromMember")
            case None        =>
        if (toValMissing || fromValMissing) Some(n)
        else None
      case _ => None
    }
    require(violations.isEmpty, "Failed member existence check!")
  end memberExistenceCheck
  @tailrec private def ownershipCheck(currentOwner: DFOwner, members: List[DFMember])(using
      MemberGetSet
  ): Unit =
    members match
      case m :: nextMembers if (m.getOwner == currentOwner) =>
        m match // still in current owner
          case o: DFOwner => ownershipCheck(o, nextMembers) // entering new owner
          case _          => ownershipCheck(currentOwner, nextMembers) // new non-member found
      case (_: DFVal.Const) :: nextMembers => // we do not care about constant ownership
        ownershipCheck(currentOwner, nextMembers)
      case Nil => // Done! All is OK
      case m :: _ => // not in current owner
        if (currentOwner.isTop)
          println(
            s"The member ${m.hashString}:\n$m\nHas owner ${m.getOwner.hashString}:\n${m.getOwner}"
          )
          val idx = getSet.designDB.members.indexOf(m)
          val prevMember = getSet.designDB.members(idx - 1)
          println(
            s"Previous member ${prevMember.hashString}:\n$prevMember\nHas owner ${prevMember.getOwner
                .hashCode()
                .toHexString}:\n${prevMember.getOwner}"
          )
          require(false, "Failed ownership check!")
        ownershipCheck(currentOwner.getOwner, members) // exiting current owner

  // checks that a member can only reference members that were defined before it
  private def orderCheck()(using MemberGetSet): Unit =
    val discoveredMembers = mutable.Set[DFMember](DFMember.Empty)
    var hasViolations: Boolean = false
    getSet.designDB.members.foreach { m =>
      m.getRefs.foreach {
        case r @ DFRef(rm) if !discoveredMembers.contains(rm) =>
          println(
            s"The member ${m.hashString}:\n$m\nHas reference $r\nPointing to a later member ${rm.hashString}:\n${rm}"
          )
          hasViolations = true
          require(!hasViolations, "Failed member order check!")
        case _ =>
      }
      discoveredMembers += m
    }
    require(!hasViolations, "Failed member order check!")
  end orderCheck

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    refCheck()
    memberExistenceCheck()
    ownershipCheck(designDB.top, designDB.membersNoGlobals.drop(1))
    orderCheck()
    designDB.check()
    designDB
end SanityCheck

extension [T: HasDB](t: T)
  def sanityCheck(using CompilerOptions): DB = StageRunner.run(SanityCheck)(t.db)
