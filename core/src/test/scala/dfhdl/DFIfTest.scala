///*
// *     This file is part of DFHDL.
// *
// *     DFHDL is free software: you can redistribute it and/or modify
// *     it under the terms of the GNU Lesser General Public License as published by
// *     the Free Software Foundation, either version 3 of the License, or
// *     any later version.
// *
// *     DFHDL is distributed in the hope that it will be useful,
// *     but WITHOUT ANY WARRANTY; without even the implied warranty of
// *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// *     GNU Lesser General Public License for more details.
// *
// *     You should have received a copy of the GNU Lesser General Public License
// *     along with DFHDL.  If not, see <https://www.gnu.org/licenses/>.
// */
//
//package dfhdl
//import GlobalDesign._
//import singleton.ops.GetArg0
//
//object IfTest {
//  val a = Bool()
//  val b = UInt(8)
//  val c = a && a
//  val d : Bool = b >= 1 && b < 2
//  b := b
//
////  implicit class ElseIfClauseBuilder(cond : Bool){
////    def apply(block : => Unit): ElseIfClause = ??? // new ElseIfClause(cond, block)
////    def && (that : Bool) : ElseIfClauseBuilder = ???
////  }
//
//  ifdf (a) {
//
//  }.elseifdf (b != b) {
//
//  }.elsedf {
//
//  }
//}
//
//object MatchTest {
//  val a = UInt[8]
//  val b = UInt[8]
//  val aa = 0 to 5
//
////  a.casedf(a) {
////
//////    casedf(b) {
//////
//////    }
//////    casedf_ {
//////
//////    }
////  }.casedf(b, b, b, b, b) {
////
////  }.casedf(a) {
////
////  }.case_ {
////
////  }
//}
//
//import dfhdl.internals._
//object vb {
////  val a = 1 To 8
////  for (i <- a) {
////
////  }
//
//}