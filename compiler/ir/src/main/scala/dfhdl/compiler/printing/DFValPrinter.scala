package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*
import DFVal.*
import analysis.*

extension (ref: DFRef.TwoWayAny)
  def refCodeString(using printer: AbstractValPrinter): String = printer.csRef(ref, false)
  def refCodeString(typeCS: Boolean)(using printer: AbstractValPrinter): String =
    printer.csRef(ref, typeCS)

extension (intParamRef: IntParamRef)
  def refCodeString(typeCS: Boolean)(using printer: AbstractValPrinter): String = intParamRef match
    case ref: DFRef.TwoWayAny => printer.csRef(ref, typeCS)
    case int: Int             => int.toString
  def refCodeString(using printer: AbstractValPrinter): String = intParamRef.refCodeString(false)
  def uboundCS(using printer: AbstractValPrinter): String = intParamRef match
    case ref: DFRef.TwoWayAny =>
      // TODO: consider implementing an associative int operation reduction
      // import printer.getSet
      // ref.get match
      //   case func @ ir.DFVal.Func(
      //         ir.DFInt32,
      //         op @ (Func.Op.+ | Func.Op.-),
      //         List(argRef, ir.DFRef(const: ir.DFVal.Const)),
      //         _,
      //         _,
      //         _
      //       ) =>
      //     val int = const.data.asInstanceOf[Option[BigInt]].get.toInt
      //     val csArg = printer.csRef(argRef, false)
      //     if (int == 1) csArg
      //     else s"$csArg $op ${int - 1}"
      //   case _ =>
      s"${printer.csRef(ref, false)} - 1"
    case int: Int => (int - 1).toString
end extension

extension (alias: Alias)
  def relValCodeString(using printer: AbstractValPrinter): String = printer.csRelVal(alias)

trait AbstractValPrinter extends AbstractPrinter:
  final def csSimpleRef(ref: DFRef.TwoWayAny): String =
    ref.get match
      case DFVal.Const(_: DFDecimal, Some(i), _, _, _) => i.toString
      case _                                           => ref.refCodeString
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String
  final def csRef(ref: DFRef.TwoWayAny, typeCS: Boolean): String =
    val member = ref.get
    extension (named: DFMember.Named)
      def nameCS: String = if (typeCS) s"${named.getName}.type" else named.getName
    member match
      case dfVal @ DesignParam(_) => dfVal.nameCS
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        if (dfVal.isAnonymous) printer.csDFValExpr(dfVal)
        else dfVal.nameCS
      case dfVal: DFVal =>
        val callOwner = ref.originMember.getOwner
        val cs = printer.csDFValRef(dfVal, callOwner)
        dfVal match
          case ch: DFConditional.Header if ch.isAnonymous => csConditionalExprRel(cs, ch)
          case _                                          => cs
      case named: DFMember.Named =>
        named.nameCS
      case _ =>
        throw new IllegalArgumentException("Fetching refCodeString from irrelevant member.")
    end match
  end csRef
  final def csRelVal(alias: Alias): String =
    alias.relValRef.refCodeString.applyBrackets()
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String
  final def csDFValConstExpr(dfVal: Const): String =
    printer.csConstData(dfVal.dfType, dfVal.data)
  def csDFValDclWithoutInit(dfVal: Dcl): String
  def csInitKeyword: String
  def csInitSingle(ref: Dcl.InitRef): String
  def csInitSeq(refs: List[Dcl.InitRef]): String
  def csDFValDclEnd(dfVal: Dcl): String
  final def csDFValDcl(dfVal: Dcl): String =
    val noInit = csDFValDclWithoutInit(dfVal)
    val init = dfVal.initRefList match
      case ref :: Nil => s" $csInitKeyword ${csInitSingle(ref)}"
      case Nil        => ""
      case refs       => s" $csInitKeyword ${csInitSeq(refs)}"
    val end = csDFValDclEnd(dfVal)
    s"$noInit$init$end"
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String
  def csDFValAliasAsIs(dfVal: Alias.AsIs): String
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx): String
  def csDFValAliasSelectField(dfVal: Alias.SelectField): String
  def csDFValAliasHistory(dfVal: Alias.History): String
  def csTimerIsActive(dfVal: Timer.IsActive): String
  final def csDFValAliasExpr(dfVal: Alias): String = dfVal match
    case dv: Alias.AsIs        => csDFValAliasAsIs(dv)
    case dv: Alias.History     => csDFValAliasHistory(dv)
    case dv: Alias.ApplyRange  => csDFValAliasApplyRange(dv)
    case dv: Alias.ApplyIdx    => csDFValAliasApplyIdx(dv)
    case dv: Alias.SelectField => csDFValAliasSelectField(dv)
  final def csDFValExpr(dfValExpr: DFVal.CanBeExpr, typeCS: Boolean = false): String =
    dfValExpr match
      case dv: Const                => csDFValConstExpr(dv)
      case dv: Func                 => csDFValFuncExpr(dv, typeCS)
      case dv: Alias                => csDFValAliasExpr(dv)
      case dv: DFConditional.Header => printer.csDFConditional(dv)
      case dv: Timer.IsActive       => csTimerIsActive(dv)
  def csDFValNamed(dfVal: DFVal): String
  final def csDFValRef(dfVal: DFVal, fromOwner: DFOwner): String =
    dfVal.stripPortSel match
      case expr: CanBeExpr if expr.isAnonymous => csDFValExpr(expr)
      case PortOfDesignDef(Modifier.OUT, design) =>
        if (design.isAnonymous) printer.csDFDesignDefInst(design)
        else design.getName
      case open: DFVal.Open => printer.csOpenKeyWord
      case dfVal            => dfVal.getRelativeName(fromOwner)
end AbstractValPrinter

protected trait DFValPrinter extends AbstractValPrinter:
  type TPrinter <: DFPrinter
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String =
    s"(${csExp.applyBrackets()}: ${printer.csDFType(ch.dfType, typeCS = true)} <> VAL)"
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String =
    s"${csDFValExpr(dfVal)}"
  def csDFValDclModifier(modifier: Modifier): String = modifier.toString
  def csDFValDclWithoutInit(dfVal: Dcl): String =
    s"${printer.csDFType(dfVal.dfType)} <> ${csDFValDclModifier(dfVal.modifier)}"
  def csInitKeyword: String = "init"
  def csInitSingle(ref: Dcl.InitRef): String = ref.refCodeString
  def csInitSeq(refs: List[Dcl.InitRef]): String = refs.view.map(_.refCodeString).mkStringBrackets
  def csDFValDclEnd(dfVal: Dcl): String = ""
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String =
    dfVal.args match
      // repeat func
      case argL :: argR :: Nil if dfVal.op == Func.Op.repeat =>
        val csArgL = argL.refCodeString(typeCS)
        val csArgR = argR.refCodeString(typeCS)
        s"${csArgL.applyBrackets()}.repeat${csArgR.applyBrackets(onlyIfRequired = false)}"
      // infix func
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        val csArgL = argL.refCodeString(typeCS)
        val csArgR = argR.refCodeString(typeCS)
        val opStr = dfVal.op match
          case Func.Op.=== => "=="
          case Func.Op.=!= => "!="
          // boolean logical operations
          case Func.Op.| | Func.Op.& if argL.get.dfType.width == 1 =>
            s"${dfVal.op}${dfVal.op}"
          // if the result width for +/-/* ops is larger than the left argument width
          // then we have a carry-inclusive operation
          case Func.Op.+ | Func.Op.- | Func.Op.`*` if dfVal.dfType.width > argL.get.dfType.width =>
            s"${dfVal.op}^"
          case op => op.toString
        s"${csArgL.applyBrackets()} $opStr ${csArgR.applyBrackets()}"
      // unary/postfix func
      case arg :: Nil =>
        val csArg = arg.refCodeString(typeCS)
        val opStr = dfVal.op.toString
        dfVal.op match
          case Func.Op.unary_! | Func.Op.unary_- | Func.Op.unary_! =>
            s"${opStr.last}${csArg.applyBrackets()}"
          case Func.Op.clog2 =>
            s"${opStr}${csArg.applyBrackets(onlyIfRequired = false)}"
          case _ => s"${csArg.applyBrackets()}.${opStr}"
      // multiarg func
      case args =>
        val csArgs = args.map(_.refCodeString)
        dfVal.op match
          case DFVal.Func.Op.++ =>
            def argsInBrackets = csArgs.mkStringBrackets
            dfVal.dfType match
              case structType @ DFStruct(structName, fieldMap) =>
                if (structType.isTuple) argsInBrackets
                else
                  structType.getName +
                    fieldMap
                      .lazyZip(csArgs)
                      .map { case ((n, _), r) =>
                        s"$n = $r"
                      }
                      .mkStringBrackets
              case DFVector(_, _) =>
                if (csArgs.allElementsAreEqual) s"all(${csArgs.head})"
                else s"Vector${csArgs.mkStringBrackets}"
              // all args are the same ==> repeat function
              case _ if args.view.map(_.get).allElementsAreEqual =>
                s"${(csArgs.head).applyBrackets()}.repeat(${args.length})"
              // regular concatenation function
              case _ => s"$argsInBrackets.toBits"
            end match
          case _ =>
            csArgs
              .map(_.applyBrackets())
              .mkString(s" ${dfVal.op} ")
        end match
  def csDFValAliasAsIs(dfVal: Alias.AsIs): String =
    val relVal = dfVal.relValRef.get
    val relValStr = dfVal.relValCodeString
    val fromType = relVal.dfType
    val toType = dfVal.dfType
    (toType, fromType) match
      case (t, f) if t == f => // ident
        // an ident is used as a placeholder and therefore does not require
        // applying brackets
        val callOwner = dfVal.getOwner
        printer.csDFValRef(relVal, callOwner)
      case (DFSInt(Int(tWidth)), DFUInt(Int(fWidth))) =>
        assert(tWidth == fWidth + 1)
        s"${relValStr}.signed"
      case (DFUInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        s"${relValStr}.uint"
      case (DFSInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        s"${relValStr}.sint"
      case (DFBits(tWidthParamRef), DFBits(_)) =>
        s"${relValStr}.resize(${tWidthParamRef.refCodeString})"
      case (DFBits(Int(tWidth)), _) =>
        assert(tWidth == fromType.width)
        s"${relValStr}.bits"
      case (DFUInt(tWidthParamRef), DFUInt(_)) =>
        s"${relValStr}.resize(${tWidthParamRef.refCodeString})"
      case (DFInt32, DFSInt(_)) =>
        s"${relValStr}.toInt"
      case (DFSInt(tWidthParamRef), DFSInt(_)) =>
        s"${relValStr}.resize(${tWidthParamRef.refCodeString})"
      case (DFBit, DFBool) =>
        s"${relValStr}.bit"
      case (DFBool, DFBit) =>
        s"${relValStr}.bool"
      case (t, DFOpaque(_, _, ot)) if ot == t =>
        s"${relValStr}.actual"
      case (_, DFBits(_)) | (DFOpaque(_, _, _), _) =>
        s"${relValStr}.as(${printer.csDFType(toType)})"
      case (DFUInt(tWidthParamRef), DFInt32) =>
        s"""d"${printer.csWidthInterp(tWidthParamRef)}'$${${relValStr}}""""
      case (DFSInt(tWidthParamRef), DFInt32) =>
        s"""sd"${printer.csWidthInterp(tWidthParamRef)}'$${${relValStr}}""""
      case (DFInt32, DFUInt(_) | DFSInt(_)) =>
        s"${relValStr}.toInt"
      case _ =>
        throw new IllegalArgumentException("Unsupported alias/conversion")
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String =
    s"${dfVal.relValCodeString}(${dfVal.relBitHigh}, ${dfVal.relBitLow})"
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx): String =
    val relIdxStr = dfVal.relIdx.refCodeString
    s"${dfVal.relValCodeString}($relIdxStr)"
  // when the tuple field number exceeds this number, the tuple
  // field selections changes from `dv._${idx+1}` to `dv($idx)`
  val TUPLE_MIN_INDEXING = 3
  def csDFValAliasSelectField(dfVal: Alias.SelectField): String =
    val dfType @ DFStruct(structName, fieldMap) = dfVal.relValRef.get.dfType: @unchecked
    val fieldSel =
      if (dfType.isTuple)
        if (fieldMap.size > TUPLE_MIN_INDEXING)
          s"(${dfVal.fieldName.drop(1).toInt - 1})"
        else s".${dfVal.fieldName}"
      else s".${dfVal.fieldName}"
    s"${dfVal.relValCodeString}$fieldSel"
  def csDFValAliasHistory(dfVal: Alias.History): String =
    val opStr = dfVal.op match
      case Alias.History.Op.State =>
        dfVal.getOwnerDomain.domainType match
          case DomainType.DF    => ".prev"
          case DomainType.RT(_) => ".reg"
          case DomainType.ED    => ??? // impossible!
      case Alias.History.Op.Pipe => ".pipe"
    val appliedStr =
      dfVal.initRefOption match
        case Some(ref)            => s"$opStr(${dfVal.step}, init = ${ref.refCodeString})"
        case _ if dfVal.step == 1 => opStr
        case _                    => s"$opStr(${dfVal.step})"
    s"${dfVal.relValCodeString}$appliedStr"
  end csDFValAliasHistory
  def csTimerIsActive(dfVal: Timer.IsActive): String =
    s"${dfVal.timerRef.refCodeString}.isActive"
  def csDFValNamed(dfVal: DFVal): String =
    def typeAnnot = dfVal match
      case dv: DFConditional.Header if dv.dfType != DFUnit => printer.csDFValType(dfVal.dfType)
      case const @ DclConst()                              => printer.csDFValConstType(dfVal.dfType)
      case _                                               => ""
    def valDef = s"val ${dfVal.getName}$typeAnnot ="
    val rhs = dfVal.stripPortSel match
      case dcl: DFVal.Dcl        => csDFValDcl(dcl)
      case const @ DclConst()    => csDFValDclConst(const)
      case expr: DFVal.CanBeExpr => csDFValExpr(expr)
      case _                     => ??? // unexpected
    val indentRHS =
      if (rhs.contains("\n")) s"\n${rhs.hindent}"
      else s" ${rhs}"
    s"$valDef$indentRHS"
  end csDFValNamed
end DFValPrinter
