/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

//package DFiant
//
//trait DFTaggedUnion[Val <: DFAny] extends DFAny.Val[WUnsafe, Val, Val with DFTaggedUnion.Var[Val]] {
//  this : Val =>
//  type TField1 <: DFAny
//  type TField2 <: DFAny
//  type TField3 <: DFAny
//  type TField4 <: DFAny
//  type TField5 <: DFAny
//  private var privWidth : Int = 0
//  val width = privWidth
//
//  type TAlias1 <: TField1#TVal
//  type TAlias2 <: TField2#TVal
//  type TAlias3 <: TField3#TVal
//  type TAlias4 <: TField4#TVal
//  type TAlias5 <: TField5#TVal
//
//  protected[DFiant] def privInsert(dfVar : DFAny) = {
//
//  }
//
//  protected def insert(dfVar : DFBits[WUnsafe]#TVar) : TBits[WUnsafe] = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TBits[WUnsafe]]
//  }
//  protected def insert(dfVar : DFBool#TVar) : TBool = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TBool]
//  }
////  protected def insert(dfVar : DFUInt#TVar) : TUInt = {
////    privInsert(dfVar)
////    dfVar.asInstanceOf[TUInt]
////  }
//
//  protected def insert(dfVar : TField1#TVar) : TAlias1 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias1]
//  }
//  protected def insert(dfVar : TField2#TVar)(implicit d1: DummyImplicit) : TAlias2 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias2]
//  }
//  protected def insert(dfVar : TField3#TVar)(implicit d1: DummyImplicit, d2: DummyImplicit) : TAlias3 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias3]
//  }
//  protected def insert(dfVar : TField4#TVar)(implicit d1: DummyImplicit, d2: DummyImplicit, d3: DummyImplicit) : TAlias4 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias4]
//  }
//  protected def insert(dfVar : TField5#TVar)(implicit d1: DummyImplicit, d2: DummyImplicit, d3: DummyImplicit, d4: DummyImplicit) : TAlias5 = {
//    privInsert(dfVar)
//    dfVar.asInstanceOf[TAlias5]
//  }
//
//}
//
//
//object DFTaggedUnion {
//  abstract class Var[Val <: DFAny]() extends DFAny.Var[WUnsafe, Val, Val with DFTaggedUnion.Var[Val]] with DFTaggedUnion[Val] {
//    this : Val =>
//    type TAlias1 = TField1#TVar
//    type TAlias2 = TField2#TVar
//    type TAlias3 = TField3#TVar
//    type TAlias4 = TField4#TVar
//    type TAlias5 = TField5#TVar
//  }
//
//  import scala.reflect.macros.blackbox.Context
//  import scala.language.experimental.macros
//
//  def helper[Val <: DFAny : c.WeakTypeTag, PVar : c.WeakTypeTag](c : Context)() : c.Expr[Val] = {
//    import c.universe._
//    val weakVal = weakTypeOf[Val]
//    val sym = symbolOf[PVar]
//    val valTree = tq"$weakVal"
//    val appliedTree = tq"$sym[$weakVal]"
//    val list = List(appliedTree, valTree)
//    val className = c.freshName()
//    val classType = TypeName(className)
//    val classTerm = TermName(className)
//    val genTree = q"""
//        case class $classType() extends ..$list {
//          def newEmptyDFVar = copy().asInstanceOf[TVar]
//        }
//        $classTerm()
//      """
//    c.Expr(genTree)
//  }
//  def apply[Val <: DFAny]() : DFTaggedUnion.Var[Val] with Val = macro helper[Val, DFTaggedUnion.Var[Val]]
//}
//
