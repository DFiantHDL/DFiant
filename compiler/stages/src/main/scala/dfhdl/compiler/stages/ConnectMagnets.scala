package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.{asFE, ModifierAny}

/** This stage creates explicit magnet port connections across the entire design.
  */
case object ConnectMagnets extends Stage:
  def dependencies: List[Stage] = List(AddMagnets)
  def nullifies: Set[Stage] = Set(ViaConnection, SimpleOrderMembers)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    // Populating a design magnet connection map
    val magnetConns = mutable.Map.empty[DFDesignBlock, List[(DFVal.Dcl, DFVal.Dcl)]]
    designDB.magnetConnectionTable.foreach { (toPort, fromPort) =>
      val toPortDsn = toPort.getOwnerDesign
      val fromPortDsn = fromPort.getOwnerDesign
      val dfType = toPort.dfType
      def anotherMagnetConn(dsn: DFDesignBlock): Unit =
        magnetConns.get(dsn) match
          case None           => magnetConns += dsn -> List((toPort, fromPort))
          case Some(connList) => magnetConns += dsn -> ((toPort, fromPort) :: connList)
      (toPort, fromPort) match
        case (DclIn(), DclIn())   => anotherMagnetConn(fromPortDsn)
        case (DclOut(), DclOut()) => anotherMagnetConn(toPortDsn)
        case (DclIn(), DclOut())  => anotherMagnetConn(toPortDsn.getOwnerDesign)
        case _                    => // do nothing
      end match
    }
    val patchList: List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      // for all designs that have magnet connections
      case (design, _) if magnetConns.contains(design) =>
        // sorting added magnets for consistent port connection addition order
        val magnets = magnetConns(design).sortBy(_._1.getName)
        val dsn = new MetaDesign(design, Patch.Add.Config.InsideLast):
          for ((toPort, fromPort) <- magnets)
            toPort.asDclAny <> fromPort.asDclAny
        // the ports are added as first members
        Some(dsn.patch)
      case _ => Nil
    }
    designDB.patch(patchList)
  end transform
end ConnectMagnets

extension [T: HasDB](t: T)
  def connectMagnets(using CompilerOptions): DB = StageRunner.run(ConnectMagnets)(t.db)
