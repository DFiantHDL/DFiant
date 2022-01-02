package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*
import DFVal.*
import analysis.*

extension (ref: DFVal.Ref)
  def refCodeString(using getSet: MemberGetSet, printer: DFValPrinter): String =
    val dfVal = ref.get
    val callOwner = ref.originRef.get.getOwner
    val cs = printer.csDFVal(dfVal, Some(callOwner))
    dfVal match
      case ch: DFConditional.Header if ch.isAnonymous =>
        s"(${cs.applyBrackets()}: ${printer.printer.csDFType(ch.dfType, typeCS = true)} <> VAL)"
      case _ => cs
  def simpleRefCodeString(using
      getSet: MemberGetSet,
      printer: DFValPrinter
  ): String =
    ref.get match
      case DFVal.Const(DFDecimal.Token(_, Some(i)), _, _, _) => i.toString
      case _ => ref.refCodeString
end extension

extension (alias: Alias)
  def relValCodeString(using
      getSet: MemberGetSet,
      printer: DFValPrinter
  ): String = alias.relValRef.refCodeString.applyBrackets()

protected trait DFValPrinter extends AbstractPrinter:
  def csDFValConst(dfVal: Const): String =
    s"${printer.csDFType(dfVal.dfType)} const ${printer.csDFToken(dfVal.token)}"
  def csDFValConstRef(dfVal: Const): String =
    printer.csDFToken(dfVal.token)
  def csDFValDcl(dfVal: Dcl): String =
    s"${printer.csDFType(dfVal.dfType)} <> ${dfVal.modifier}"

  def csDFValFuncRef(dfVal: Func)(using MemberGetSet): String =
    dfVal.args match
      // infix func
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        val opStr = dfVal.op match
          case Func.Op.=== => "=="
          case Func.Op.=!= => "!="
          case Func.Op.| | Func.Op.& if argL.get.dfType.width == 1 =>
            s"${dfVal.op}${dfVal.op}"
          case op => op.toString
        val rhsStr = dfVal.op match
          case Func.Op.>> | Func.Op.<< => argR.simpleRefCodeString
          case _                       => argR.refCodeString
        s"${argL.refCodeString.applyBrackets()} $opStr ${rhsStr.applyBrackets()}"
      // unary/postfix func
      case arg :: Nil =>
        val opStr = dfVal.op.toString
        val argStr = arg.refCodeString.applyBrackets()
        if (opStr.startsWith("unary_")) s"${opStr.last}$argStr"
        else s"${argStr}.${opStr}"
      // multiarg func
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ =>
            def argsInBrackets = args.map(_.refCodeString).mkStringBrackets
            dfVal.dfType match
              case DFStruct(structName, fieldMap) =>
                if (structName.isEmpty) argsInBrackets
                else
                  structName + fieldMap
                    .lazyZip(args)
                    .map { case ((n, _), r) =>
                      s"$n = ${r.refCodeString}"
                    }
                    .mkStringBrackets
              // all args are the same ==> repeat function
              case _ if args.view.map(_.get).allElementsAreEqual =>
                s"${(args.head.refCodeString).applyBrackets()}.repeat(${args.length})"
              // regular concatenation function
              case _ => argsInBrackets
            end match
          case _ =>
            args
              .map(_.refCodeString.applyBrackets())
              .mkString(s" ${dfVal.op} ")
  def csDFValAliasAsIs(dfVal: Alias.AsIs)(using MemberGetSet): String =
    val relVal = dfVal.relValRef.get
    val relValStr = dfVal.relValCodeString
    val fromType = relVal.dfType
    val toType = dfVal.dfType
    (toType, fromType) match
      case (t, f) if t == f => // ident
        // an ident is used as a placeholder and therefore does not require
        // applying brackets
        val callOwner = dfVal.relValRef.originRef.get.getOwner
        printer.csDFVal(relVal, Some(callOwner))
      case (DFSInt(tWidth), DFUInt(fWidth)) =>
        assert(tWidth == fWidth + 1)
        s"${relValStr}.signed"
      case (DFUInt(tWidth), DFBits(fWidth)) =>
        assert(tWidth == fWidth)
        s"${relValStr}.uint"
      case (DFSInt(tWidth), DFBits(fWidth)) =>
        assert(tWidth == fWidth)
        s"${relValStr}.sint"
      case (DFBits(tWidth), DFBits(_)) =>
        s"${relValStr}.resize($tWidth)"
      case (DFBits(tWidth), _) =>
        assert(tWidth == fromType.width)
        s"${relValStr}.bits"
      case (DFUInt(tWidth), DFUInt(_)) =>
        s"${relValStr}.resize($tWidth)"
      case (DFSInt(tWidth), DFSInt(_)) =>
        s"${relValStr}.resize($tWidth)"
      case (DFBit, DFBool) =>
        s"${relValStr}.bit"
      case (DFBool, DFBit) =>
        s"${relValStr}.bool"
      case (t, DFOpaque(_, ot)) if ot == t =>
        s"${relValStr}.actual"
      case (_, DFBits(_)) | (DFOpaque(_, _), _) =>
        s"${relValStr}.as(${printer.csDFType(toType)})"
      case _ =>
        throw new IllegalArgumentException("Unsupported alias/conversion")
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange)(using
      MemberGetSet
  ): String =
    s"${dfVal.relValCodeString}(${dfVal.relBitHigh}, ${dfVal.relBitLow})"
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx)(using
      MemberGetSet
  ): String =
    val relIdxStr = dfVal.relIdx.simpleRefCodeString
    s"${dfVal.relValCodeString}($relIdxStr)"
  // when the tuple field number exceeds this number, the tuple
  // field selections changes from `dv._${idx+1}` to `dv($idx)`
  val TUPLE_MIN_INDEXING = 3
  def csDFValAliasSelectField(dfVal: Alias.SelectField)(using
      MemberGetSet
  ): String =
    val DFStruct(structName, fieldMap) = dfVal.relValRef.get.dfType
    val fieldSel =
      if (structName.isEmpty)
        if (fieldMap.size > TUPLE_MIN_INDEXING)
          s"(${dfVal.fieldName.drop(1).toInt - 1})"
        else s".${dfVal.fieldName}"
      else s".${dfVal.fieldName}"
    s"${dfVal.relValCodeString}$fieldSel"
  def csDFValAliasRef(dfVal: Alias)(using MemberGetSet): String = dfVal match
    case dv: Alias.AsIs        => csDFValAliasAsIs(dv)
    case _: Alias.Prev         => ???
    case dv: Alias.ApplyRange  => csDFValAliasApplyRange(dv)
    case dv: Alias.ApplyIdx    => csDFValAliasApplyIdx(dv)
    case dv: Alias.SelectField => csDFValAliasSelectField(dv)
  def csDFVal(dfVal: DFVal, fromOwner: Option[DFOwner])(using
      MemberGetSet
  ): String =
    def typeAnnot = dfVal match
      case dv: DFConditional.Header if dv.dfType != NoType =>
        s": ${printer.csDFType(dfVal.dfType, typeCS = true)} <> VAL"
      case _ => ""
    def valDef = s"val ${dfVal.name}$typeAnnot ="
    def rhs = dfVal match
      case dv: Dcl                  => csDFValDcl(dv)
      case dv: Const                => csDFValConst(dv)
      case dv: Func                 => csDFValFuncRef(dv)
      case dv: Alias                => csDFValAliasRef(dv)
      case dv: DFConditional.Header => printer.csDFConditional(dv)
    def rhsInit = dfVal.getTagOf[ExternalInit] match
      case Some(ExternalInit(initSeq)) if initSeq.size > 1 =>
        s"$rhs init ${printer.csDFTokenSeq(initSeq)}"
      case Some(ExternalInit(initSeq)) if initSeq.size == 1 =>
        s"$rhs init ${printer.csDFToken(initSeq.head)}"
      case _ => rhs
    (dfVal, fromOwner) match
      case (c: Const, Some(_)) if c.isAnonymous => csDFValConstRef(c)
      case (dv, Some(owner)) if !dv.isAnonymous =>
        dfVal.getRelativeName(owner)
      case (dv, None) if !dv.isAnonymous =>
        val rhsInitVal = rhsInit
        val delimRHS =
          if (rhsInitVal.contains("\n")) s"\n${rhsInitVal.indent(1)}"
          else s" ${rhsInitVal}"
        s"$valDef$delimRHS"
      case _ => rhsInit
  end csDFVal
end DFValPrinter
