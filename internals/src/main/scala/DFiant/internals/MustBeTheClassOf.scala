package DFiant.internals

import scala.annotation.tailrec
import scala.reflect.macros.whitebox


sealed class MustBeTheClassOf[T]
object MustBeTheClassOf {
  implicit def ev[T] : MustBeTheClassOf[T] = macro evMacro[T]
  def evMacro[T](c: whitebox.Context)(implicit n : c.WeakTypeTag[T]) : c.Tree = {
    import c.universe._
    val tp = weakTypeOf[T]
    @tailrec def explore(tree : c.Tree): Boolean = {
      tree match {
        case Select(This(_), _) =>
          tree.symbol.fullName == tp.typeSymbol.fullName
        case Apply(Select(sup @ Super(next@ This(_), _),_), _) =>
          val SuperType(_, t) = sup.tpe
          if (t.typeSymbol.fullName == tp.typeSymbol.fullName) true
          else explore(next)
        case Apply(Select(New(t), _), _) => t.symbol.fullName == tp.typeSymbol.fullName
        case Apply(Select(t,_), _) => t.symbol.fullName == tp.typeSymbol.fullName
        case Select(next, _) => explore(next)
        case This(_) => tree.symbol.fullName == tp.typeSymbol.fullName
        case TypeApply(tree, _) => explore(tree)
        case t@TypeTree() => t.symbol.fullName == tp.typeSymbol.fullName
        case t@Ident(_) => t.symbol.fullName == tp.typeSymbol.fullName
        case _ =>
          false
      }
    }
    val owner = c.internal.enclosingOwner.owner
    val ownerOK =   if (owner.isClass) {
      //           anonymous class                         object
      if (owner.name.toString.startsWith("$anon") || owner.isModuleClass) owner.asClass.baseClasses match {
        case _ :: parent :: _ => parent.fullName == tp.typeSymbol.fullName //get the name of the base class
        case _ => false
      }
      else owner.fullName == tp.typeSymbol.fullName //any other class
    } else false

    val ok = if (ownerOK) true else explore(c.enclosingImplicits.last.tree)
//    println(showRaw(c.enclosingImplicits.last.tree), owner, "compared to", tp, "got", ok)

    if (ok)  q"new DFiant.internals.MustBeTheClassOf[$tp]"
    else c.abort(c.enclosingPosition, "Wrong class symbol")
  }
}