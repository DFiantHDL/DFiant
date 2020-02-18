package ZFiant.compiler.backend.vhdl

import ZFiant._
import ZFiant.compiler.backend.utils._

class Compiler(design : DFDesign) {
  private implicit def getVHDLType(from : DFAny) : adt.Value.Type = from.dfType match {
    case DFBits.Type(width) => adt.Value.Type.std_logic_vector(width)
    case DFUInt.Type(width) => adt.Value.Type.unsigned(width)
    case DFBool.Type() => adt.Value.Type.std_logic
  }
  private implicit def getName(from : DFAny)(implicit nameDB : NameDB) : adt.Name =
    if (from.isAnonymous) adt.Name.anonymous
    else {
      //port names are capitalized to make ports more visible
      val modifiedName = from match {
        case DFAny.In() => from.name.toUpperCase
        case DFAny.Out() => from.name.toUpperCase
        case _ => from.name
      }
      adt.Name(nameDB.getUniqueName(modifiedName))
    }

  private val designDB = design.getDB
  implicit val nameDB : NameDB = new NameDB(reservedKeywords, false)
  private val members = designDB.ownerMemberTable(design.block)
//  private val clkPort = ast.Clock()
  private val portMap : Map[DFAny, adt.Value] = Map.from(members.map {
    case p@DFAny.In() => p -> adt.Value.Dcl.Port.In(p, p, None)
    case p@DFAny.Out() => p -> adt.Value.Dcl.Port.Out(p, p, None)
  })


}
