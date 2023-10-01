package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.tpd
import StdNames.nme
import Names.*
import Constants.Constant
import Types.*

import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec

class MetaContextGenPhase(setting: Setting) extends CommonPhase:
  import tpd._

//  override val debugFilter: String => Boolean =
//    _.contains("PluginSpec.scala")
  val phaseName = "MetaContextGen"

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextDelegate")
  var setMetaSym: Symbol = _
  var dfTokenSym: Symbol = _
  var dfValSym: Symbol = _
  var metaGenSym: Symbol = _
  var designFromDefSym: Symbol = _
  var designFromDefGetInputSym: Symbol = _
  var inlineAnnotSym: Symbol = _
  val treeOwnerMap = mutable.Map.empty[String, Tree]
  val contextDefs = mutable.Map.empty[String, Tree]
  var clsStack = List.empty[TypeDef]
  val inlinedOwnerStack = mutable.Map.empty[Apply, Inlined]
  var applyPosStack = List.empty[util.SrcPos]

  extension (tree: Tree)(using Context)
    def isDFVal: Boolean =
      val rhsSym = tree.tpe.dealias.typeSymbol
      rhsSym == dfValSym
    def setMeta(
        nameOpt: Option[String],
        srcPos: util.SrcPos,
        docOpt: Option[String],
        annotations: List[Annotations.Annotation]
    ): Tree =
      val nameOptTree = mkOptionString(nameOpt)
      val positionTree = srcPos.positionTree
      val docOptTree = mkOptionString(docOpt)
      val annotTree = mkList(annotations.map(_.tree))
      tree
        .select(setMetaSym)
        .appliedToArgs(
          nameOptTree :: positionTree :: docOptTree :: annotTree :: Nil
        )
        .withType(TermRef(tree.tpe, setMetaSym))
    end setMeta
  end extension

  extension (tree: ValOrDefDef)(using Context)
    def genMeta: Tree =
      val nameOptTree = mkOptionString(Some(tree.name.toString.nameCheck(tree)))
      val positionTree = tree.srcPos.positionTree
      val docOptTree = mkOptionString(tree.symbol.docString)
      val annotTree = mkList(tree.symbol.annotations.map(_.tree))
      ref(metaGenSym).appliedToArgs(
        nameOptTree :: positionTree :: docOptTree :: annotTree :: Nil
      )
  end extension

  extension (sym: Symbol)
    def fixedFullName(using Context): String =
      sym.fullName.toString.replace("._$", ".")

  extension (name: String)
    def nameCheck(posTree: Tree)(using Context): String =
      val finalName =
        posTree.symbol.getAnnotation(defn.TargetNameAnnot)
          .flatMap(_.argumentConstantString(0))
          .getOrElse(name)
      if (
        !finalName.matches("^[a-zA-Z0-9_]*$") && !posTree.symbol.flags.is(
          Flags.Synthetic
        )
      )
        report.error(
          s"""Unsupported DFHDL member name $finalName.
           |Only alphanumric or underscore characters are supported.
           |You can leave the Scala name as-is and add @targetName("newName") annotation.""".stripMargin,
          posTree.srcPos
        )
      finalName

  private def ignoreValDef(tree: ValDef)(using Context): Boolean =
    tree.name.toString match
      case inlinedName(prefix) =>
        tree.tpe match
          case x: TermRef =>
            x.underlying.dealias.typeSymbol.name.toString == prefix ||
            x.parents.exists(_.typeSymbol.name.toString == prefix)
          case _ => false
      case _ => false
  override def transformApply(tree: Apply)(using Context): Tree =
    val srcPos = applyPosStack.head
    applyPosStack = applyPosStack.drop(1)
    if (tree.tpe.isParameterless)
      tree match
        case ContextArg(argTree) =>
          val sym = argTree.symbol
          treeOwnerMap.get(srcPos.show) match
            case Some(t: ValDef) =>
              if (t.symbol.flags.is(Flags.Mutable))
                report.warning(
                  "Scala `var` modifier for DFHDL values/classes is highly discouraged!\nConsider changing to `val`.",
                  t.srcPos
                )
              val (nameOpt, docOpt, annots) =
                if (ignoreValDef(t)) (None, None, Nil)
                else
                  (
                    Some(t.name.toString.nameCheck(t)),
                    t.symbol.docString,
                    t.symbol.staticAnnotations
                  )
              tree.replaceArg(argTree, argTree.setMeta(nameOpt, srcPos, docOpt, annots))
            case Some(t: TypeDef) if t.name.toString.endsWith("$") =>
              tree.replaceArg(
                argTree,
                argTree.setMeta(
                  Some(t.name.toString.dropRight(1).nameCheck(t)),
                  srcPos,
                  t.symbol.docString,
                  t.symbol.staticAnnotations
                )
              )
            case Some(t) => // Def or Class
              contextDefs.get(sym.fixedFullName) match
                case Some(ct) if !(ct sameTree t) =>
                  report.error(
                    s"${t.symbol} is missing an implicit Context parameter",
                    t.symbol
                  )
                case _ =>
              // do nothing
              tree
            case _ => // Anonymous
              tree.replaceArg(argTree, argTree.setMeta(None, srcPos, None, Nil))
          end match
        case _ => tree
    else tree
  end transformApply

  val localPattern = "\\<local (.*)\\$\\>".r
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    tree.rhs match
      case template: Template =>
        if (!tree.symbol.isAnonymousClass)
          template.parents.foreach(p => addToTreeOwnerMap(p, tree)(using None))
          addContextDef(tree)
        clsStack = tree :: clsStack
      case _ =>
    ctx

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    tree.rhs match
      case template: Template =>
        clsStack = clsStack.drop(1)
      case _ =>
    tree

  private def addToTreeOwnerMap(tree: Tree, ownerTree: Tree)(using
      inlinedPosOpt: Option[util.SrcPos]
  )(using Context): Unit =
    // debug("Adding!!!")
    val srcPos = inlinedPosOpt.getOrElse(tree.srcPos)
    // debug(srcPos.show)
    treeOwnerMap += (srcPos.show -> ownerTree)

  @tailrec private def nameValOrDef(
      tree: Tree,
      ownerTree: Tree,
      typeFocus: Type,
      inlinedPosOpt: Option[util.SrcPos]
  )(using Context): Unit =
    // debug("------------------------------")
    // debug("pos--->  ", tree.srcPos.show)
    // debug(tree.showSummary(10))
    // in case this returns an iterable of type T, then the arguments that
    // have the type T will also be candidates to get the name
    lazy val iterableType: Option[Type] = typeFocus match
      case AppliedType(tycon, typeArg :: Nil)
          if tycon.typeSymbol.inherits("scala.collection.Iterable") =>
        Some(typeArg)
      case _ => None
    tree match
      // any `map` or `for` would yield a block of anonymous function and closure
      case Apply(_, List(Block(List(defdef: DefDef), _: Closure))) =>
        // debug("Map/For")
        nameValOrDef(defdef.rhs, ownerTree, defdef.rhs.tpe.simple, inlinedPosOpt)
      case apply: Apply =>
        // debug("Apply done!")
        // ignoring anonymous method unless it has a context argument
        val add =
          if (ownerTree.symbol.isAnonymousFunction)
            ownerTree match
              // this case is for functions like `def foo(block : DFC ?=> Unit) : Unit`
              case DefDef(_, List(List(arg)), _, _) => arg.tpe <:< metaContextTpe
              case _                                => false
          else true
        if (add) iterableType match
          case Some(typeArg) =>
            apply.args.collectFirst {
              case termArg if termArg.tpe <:< typeArg => termArg
            } match
              case Some(termArg) => nameValOrDef(termArg, ownerTree, typeArg, inlinedPosOpt)
              case None          =>
          case _ => addToTreeOwnerMap(apply, ownerTree)(using inlinedPosOpt)
      case Typed(tree, _) =>
        // debug("Typed")
        nameValOrDef(tree, ownerTree, typeFocus, inlinedPosOpt)
      case TypeApply(Select(tree, _), _) =>
        // debug("TypeApply")
        nameValOrDef(tree, ownerTree, typeFocus, inlinedPosOpt)
      case inlined @ Inlined(_, _, tree) =>
        // debug("Inlined")
        nameValOrDef(
          tree,
          ownerTree,
          typeFocus,
          Some(inlinedPosOpt.getOrElse(inlined.srcPos))
        )
      case Block((cls @ TypeDef(_, template: Template)) :: _, _) if cls.symbol.isAnonymousClass =>
        // debug("Block done!")
        template.parents.foreach(p => addToTreeOwnerMap(p, ownerTree)(using inlinedPosOpt))
      case block: Block =>
        // debug("Block expr")
        nameValOrDef(block.expr, ownerTree, typeFocus, inlinedPosOpt)
      case tryBlock: Try =>
        // debug("Try block")
        nameValOrDef(tryBlock.expr, ownerTree, typeFocus, inlinedPosOpt)
      case _ =>
    end match
  end nameValOrDef

  def addContextDef(tree: Tree)(using Context): Unit =
    val defdefTree = tree match
      case tree: DefDef  => tree
      case tree: TypeDef =>
        // debug(tree.symbol)
        tree.rhs.asInstanceOf[Template].constr
    defdefTree.paramss.flatten.view.reverse.collectFirst {
      case a if a.tpe <:< metaContextTpe =>
        val fixedName = a.symbol.fixedFullName
        // debug(s"Def   ${fixedName}, ${tree.show}")
        contextDefs += (fixedName -> tree)
    }
  private def rejectBadPrimitiveOps(tree: Apply, pos: util.SrcPos)(using
      Context
  ): Unit =
    tree match
      case Apply(Select(lhs, fun), List(rhs))
          if (fun == nme.EQ || fun == nme.NE) &&
            (lhs.tpe <:< defn.IntType || lhs.tpe <:< defn.BooleanType || lhs.tpe <:< defn.TupleTypeRef) =>
        val rhsSym = rhs.tpe.dealias.typeSymbol
        if (rhsSym == dfValSym || rhsSym == dfTokenSym)
          report.error(
            s"Unsupported Scala primitive at the LHS of `$fun` with a DFHDL value or token.\nConsider switching positions of the arguments.",
            pos
          )
      case Apply(Select(lhs, fun), List(Apply(Apply(Ident(hackName), _), _)))
          if (fun == nme.ZOR || fun == nme.ZAND || fun == nme.XOR) && hackName.toString == "BooleanHack" =>
        report.error(
          s"Unsupported Scala Boolean primitive at the LHS of `$fun` with a DFHDL value.\nConsider switching positions of the arguments.",
          pos
        )
      case Apply(Apply(Ident(hackName), _), _) if hackName.toString == "BooleanHack" =>
        report.error(
          s"Found unexpected DFHDL boolean to Scala boolean conversion.",
          pos
        )
      case _ =>

  override def prepareForApply(tree: Apply)(using Context): Context =
    val srcPos = inlinedOwnerStack.get(tree) match
      case Some(inlined) =>
        inlinedOwnerStack.remove(tree)
        inlined.srcPos
      case _ => tree.srcPos

    rejectBadPrimitiveOps(tree, srcPos)
    applyPosStack = srcPos :: applyPosStack
    ctx

  @tailrec private def inlinePos(
      tree: Tree,
      inlinedTree: Inlined
  )(using Context): Unit =
//    debug("pos--->  ", tree.srcPos.show)
//    debug(tree.show)
    tree match
      case apply: Apply =>
        if (!inlinedOwnerStack.contains(apply) && !inlinedTree.call.isEmpty)
          // debug("INLINE:")
          // debug("from", apply.srcPos.show)
          // debug("to  ", inlinedTree.srcPos.show)
          inlinedOwnerStack += (apply -> inlinedTree)
      case Typed(tree, _) =>
        inlinePos(tree, inlinedTree)
      case TypeApply(Select(tree, _), _) =>
        inlinePos(tree, inlinedTree)
      case Inlined(_, _, tree) =>
        inlinePos(tree, inlinedTree)
      case block: Block =>
        inlinePos(block.expr, inlinedTree)
      case _ =>
    end match
  end inlinePos

  // design construction from definitions
  override def transformDefDef(tree: DefDef)(using Context): tpd.Tree =
    val sym = tree.symbol
    lazy val dfValArgs = tree.paramss.view.flatten.collect {
      case vd: ValDef if vd.tpt.isDFVal => vd
    }.toList
    lazy val dfcArg = ContextArg.at(tree)
    if (
      // ignore inline methods and exported methods
      !(sym is Inline) && !sym.hasAnnotation(inlineAnnotSym) && !(sym is Exported) &&
      // trivially ignorable methods before type checking
      !sym.isConstructor && !(sym is JavaStatic) &&
      // accept only methods that return a DFHDL value and
      // have at least one DFHDL parameter and
      // have a context argument
      tree.tpt.isDFVal && dfValArgs.nonEmpty && dfcArg.nonEmpty
    )
      debug("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      // debug(tree.show)
      // list of tuples of the old arguments and their meta data
      val args = mkList(dfValArgs.map(a => mkTuple(List(a.ident, a.genMeta))))
      // input map to replace old arg references with new input references
      val inputsMap = dfValArgs.view.zipWithIndex.map((a, i) =>
        a.name -> ref(designFromDefGetInputSym)
          .appliedToType(a.tpt.tpe.widen)
          .appliedTo(Literal(Constant(i)))
          .appliedTo(dfcArg.get)
      ).toMap
      // replacing the old arg references according to the input map
      val identReplacer = new TreeMap():
        override def transform(tree: Tree)(using Context): Tree =
          tree match
            case Ident(n: TermName) if inputsMap.contains(n) => inputsMap(n)
            case _                                           => super.transform(tree)
      val updatedRHS = identReplacer.transform(tree.rhs)
      debug(updatedRHS.show)
      // calling the runtime method that constructs the design from the definition
      val designFromDef =
        ref(designFromDefSym)
          .appliedToType(tree.rhs.tpe.widen)
          .appliedToArgs(List(args, tree.genMeta))
          .appliedTo(updatedRHS)
          .appliedTo(dfcArg.get)
      cpy.DefDef(tree)(rhs = designFromDef)
    else tree
    end if
  end transformDefDef

  override def prepareForInlined(inlined: Inlined)(using Context): Context =
    inlinePos(inlined.expansion, inlined)
    ctx

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    if (
      !tree.symbol.isClassConstructor &&
      !tree.name.toString.contains("$proxy") && !(tree.symbol is Exported)
    )
      addContextDef(tree)
      nameValOrDef(tree.rhs, tree, tree.tpe.simple, None)
    ctx

  private val inlinedName = "(.*)_this".r
  override def prepareForValDef(tree: ValDef)(using Context): Context =
    tree.name.toString match
      case n if n.contains("$proxy") => // do nothing
      case _ if tree.mods.is(Param)  => // do nothing
      case _ if tree.rhs.isEmpty     => // do nothing
      case _                         =>
        // debug("================================================")
        // debug(s"prepareForValDef: ${tree.name}")
        nameValOrDef(tree.rhs, tree, tree.tpe.simple, None)
    ctx

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    setMetaSym = metaContextCls.requiredMethod("setMeta")
    metaGenSym = requiredMethod("dfhdl.compiler.ir.Meta.gen")
    designFromDefSym = requiredMethod("dfhdl.core.__For_Plugin.designFromDef")
    designFromDefGetInputSym = requiredMethod("dfhdl.core.__For_Plugin.designFromDefGetInput")
    dfValSym = requiredClass("dfhdl.core.DFVal")
    dfTokenSym = requiredClass("dfhdl.core.DFToken")
    inlineAnnotSym = requiredClass("scala.inline")
    treeOwnerMap.clear()
    contextDefs.clear()
    inlinedOwnerStack.clear()
    ctx
end MetaContextGenPhase
