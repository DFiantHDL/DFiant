package DFiant
package compiler

import DFDesign.DB.Patch
import DFDesign.Frontend._
import DFiant.DFAny.Func2.Op
final class ExplicitConversions[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  private def resizeUInt(dfVal : DFAny.Member, updatedWidth : Int)(implicit ctx : DFBlock.Context) : DFAny.Member = dfVal match {
    case DFAny.Const(dfType, token : UInt.Token, ownerRef, tags) =>
      DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
    case _ =>
      dfVal.asValOf[UInt.Type[Int]].resize(updatedWidth).anonymize.member
  }
  private def resizeSInt(dfVal : DFAny.Member, updatedWidth : Int)(implicit ctx : DFBlock.Context) : DFAny.Member = dfVal match {
    case DFAny.Const(dfType, token : SInt.Token, ownerRef, tags) =>
      DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
    case _ =>
      dfVal.asValOf[SInt.Type[Int]].resize(updatedWidth).anonymize.member
  }
  private def toggleLogical(dfVal : DFAny.Member)(implicit ctx : DFBlock.Context) : DFAny.Member = dfVal match {
    case DFAny.Const(_, Bool.Token(logical, value), ownerRef, tags) =>
      DFAny.Const(Bool.Type(!logical), Bool.Token(!logical, value), ownerRef, tags)
    case Bit() =>
      (dfVal.asValOf[Bool.Type] === 1).anonymize.member
    case Bool() =>
      DFAny.Alias.AsIs(Bool.Type(false), dfVal.asValOf[Bool.Type]).anonymize.member
  }

  import designDB.__getset
  //with-carry arithmetics function on uint/sint
  def carryMathConversion : IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      case func @ DFAny.Func2.Unref(dfType, leftArg, op : DFAny.Func2.Op.Carry, rightArg,_,_) =>
        val dsn = new MetaDesign() {
          private val opNC = op match {
            case Op.+^ => Op.+
            case Op.-^ => Op.-
            case Op.*^ => Op.*
          }
          private val tokenFuncNC : (_ <: DFAny.Token, _ <: DFAny.Token) => DFAny.Token = (op, dfType) match {
            case (Op.+^, UInt.Type(_)) => (l : UInt.Token, r : UInt.Token) => l + r
            case (Op.-^, UInt.Type(_)) => (l : UInt.Token, r : UInt.Token) => l - r
            case (Op.*^, UInt.Type(_)) => (l : UInt.Token, r : UInt.Token) => l * r
            case (Op.+^, SInt.Type(_)) => (l : SInt.Token, r : SInt.Token) => l + r
            case (Op.-^, SInt.Type(_)) => (l : SInt.Token, r : SInt.Token) => l - r
            case (Op.*^, SInt.Type(_)) => (l : SInt.Token, r : SInt.Token) => l * r
            case _ => ???
          }
          private val (left, right) = dfType match {
            case UInt.Type(targetWidth) =>
              if (leftArg.width >= rightArg.width)
                (leftArg.asValOf[UInt.Type[Int]].resize(targetWidth).member.anonymize, rightArg)
              else (leftArg, rightArg.asValOf[UInt.Type[Int]].resize(targetWidth).member.anonymize)
            case SInt.Type(targetWidth) =>
              if (leftArg.width >= rightArg.width)
                (leftArg.asValOf[SInt.Type[Int]].resize(targetWidth).member.anonymize, rightArg)
              else (leftArg, rightArg.asValOf[SInt.Type[Int]].resize(targetWidth).member.anonymize)
          }
          DFAny.Func2.forced(dfType, left, opNC, right)(tokenFuncNC) setTags(_ => func.tags)
        }
        Some(func -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }

  def explicitConversions : IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      //explicit widening when assigning/connecting to a wider variable or converting bit/bool
      case net : DFNet =>
        val toVal = net.toRef.get
        val fromVal = net.fromRef.get
        val conv = (toVal, fromVal) match {
          case (UInt(toWidth), UInt(fromWidth)) if toWidth > fromWidth => true
          case (SInt(toWidth), SInt(fromWidth)) if toWidth > fromWidth => true
          case (Bool(), Bit()) => true
          case (Bit(), Bool()) => true
          case _ => false
        }
        if (conv) {
          val dsn = new MetaDesign() {
            val updatedFromVal = (toVal, fromVal) match {
              case (UInt(toWidth), UInt(fromWidth)) if toWidth > fromWidth =>
                fromVal.asValOf[UInt.Type[Int]].resize(toWidth).member.anonymize
              case (SInt(toWidth), SInt(fromWidth)) if toWidth > fromWidth =>
                fromVal.asValOf[SInt.Type[Int]].resize(toWidth).member.anonymize
              case (Bool(), Bit()) => toggleLogical(fromVal)
              case (Bit(), Bool()) => toggleLogical(fromVal)
            }
            toVal.assign(updatedFromVal)
          }
          Some(net -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
        } else None
      //function between a bit and a boolean
      case func : DFAny.Func2 =>
        val leftArg = func.leftArgRef.get
        val rightArg = func.rightArgRef.get
        val conv = (leftArg, rightArg) match {
          case (Bit(), Bool()) => true
          case (Bool(), Bit()) => true
          case _ => false
        }
        if (conv) {
          val dsn = new MetaDesign() {
            (leftArg, rightArg) match {
              case (Bit(), Bool()) =>
                DFAny.Func2.forced(Bool.Type(true), toggleLogical(leftArg), func.op, rightArg)(func.tokenFunc).anonymize
              case (Bool(), Bit()) =>
                DFAny.Func2.forced(Bool.Type(true), leftArg, func.op, toggleLogical(rightArg))(func.tokenFunc).anonymize
              case _ => None
            }
          }
          Some(func -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
        } else None
      //conditional on bit
      case cb @ DFConditional.IfElseBlock(Some(condRef),prevBlockRefOption,_,_) =>
        val cond = condRef.get
        val prevIfOption = prevBlockRefOption.map(r => r.get)
        cond match {
          case Bit() =>
            val dsn = new MetaDesign() {
              DFConditional.IfElseBlock(Some(toggleLogical(cond)), prevIfOption)
            }
            Some(cb -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
          case _ => None
        }
      //assert on bit
      case asrt @ sim.DFSimMember.Assert.Unref(Some(cond @ Bit()), msg, severity, _, _) =>
        val dsn = new MetaDesign() {
          sim.DFSimMember.Assert(Some(toggleLogical(cond).asInstanceOf[Bool]), msg, severity)
        }
        Some(asrt -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }
}
