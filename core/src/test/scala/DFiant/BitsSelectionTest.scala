///*
// *     This file is part of DFiant.
// *
// *     DFiant is free software: you can redistribute it and/or modify
// *     it under the terms of the GNU Lesser General Public License as published by
// *     the Free Software Foundation, either version 3 of the License, or
// *     any later version.
// *
// *     DFiant is distributed in the hope that it will be useful,
// *     but WITHOUT ANY WARRANTY; without even the implied warranty of
// *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// *     GNU Lesser General Public License for more details.
// *
// *     You should have received a copy of the GNU Lesser General Public License
// *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
// */
//
//package DFiant
//import shapeless.test.illTyped
//import TestUtils._
//import DFiant.internals.Meta
//
//class SafeBitsSelectionFromSafeVarSpec extends DFProperties("SafeBitsSelectionFromSafeVarSpec") {
//  property("DFBits[W] @ W < 0 compile error") = {
//    illTyped { """DFBits[0]""" }
//    illTyped { """DFBits[-1]""" }
//    true
//  }
//  property("DFBits[4].width") = {
//    val a = DFBits[4]
//    implicitly[a.Width =:= 4]
//    a.width.getValue == 4
//  }
//  property("DFBits[4].bits(2,1)") = {
//    val a = DFBits[4]; val b = a.bits(2,1)
//    implicitly[b.Width =:= 2]
//    b.width.getValue == 2
//  }
//  property("DFBits[4].apply(2,1)") = {
//    val a = DFBits[4]; val b = a(2,1)
//    implicitly[b.Width =:= 2]
//    b.width.getValue == 2
//  }
//  property("DFBits[4].bits(1,1)") = {
//    val a = DFBits[4]; val b = a.bits(1,1)
//    implicitly[b.Width =:= 1]
//    b.width.getValue == 1
//  }
//  property("DFBits[4].resizeRight(2)") = {
//    val a = DFBits[4]; val b = a.resizeRight(2)
//    implicitly[b.Width =:= 2]
//    b.width.getValue == 2
//  }
//  property("DFBits[4].resize(2)") = {
//    val a = DFBits[4]; val b = a.resize(2)
//    implicitly[b.Width =:= 2]
//    b.width.getValue == 2
//  }
//  property("DFBits[4] out of range bits range selections compile error") = {
//    var three = 3
//    illTyped { """val a = DFBits[4]; a.bits(4,three)""" }
//    illTyped { """val a = DFBits[4]; a.bits(4,3)""" }
//    illTyped { """val a = DFBits[4]; a.bits(-1,-2)""" }
//    illTyped { """val a = DFBits[4]; a.bits(1,-2)""" }
//    illTyped { """val a = DFBits[4]; a.bits(1,3)""" }
//    illTyped { """val a = DFBits[4]; a.resizeRight(0)""" }
//    illTyped { """val a = DFBits[4]; a.resizeRight(-1)""" }
//    illTyped { """val a = DFBits[4]; a.resize(0)""" }
//    illTyped { """val a = DFBits[4]; a.resize(-1)""" }
//    illTyped { """val a = DFBits[4]; a(4,3)""" }
//    illTyped { """val a = DFBits[4]; a(-1,-2)""" }
//    illTyped { """val a = DFBits[4]; a(1,-2)""" }
//    illTyped { """val a = DFBits[4]; a(1,3)""" }
//    true
//  }
//  property("DFBits[4].bit(1)") = {
//    val a = DFBits[4]; val b : DFBool = a.bit(1)
//    b.width.getValue == 1
//  }
//  property("DFBits[4].apply(1)") = {
//    val a = DFBits[4]; val b : DFBool = a(1)
//    b.width.getValue == 1
//  }
//  property("DFBits[4] out of range single bit selections compile error") = {
//    illTyped { """val a = DFBits[4]; a.bit(4)""" }
//    illTyped { """val a = DFBits[4]; a.bit(-1)""" }
//    illTyped { """val a = DFBits[4]; a(4)""" }
//    illTyped { """val a = DFBits[4]; a(-1)""" }
//    true
//  }
//}
//
//
//class UnsafeBitsSelectionFromSafeVarSpec extends DFProperties("UnsafeBitsSelectionFromSafeVarSpec") {
//  var two = 2
//  var one = 1
//  var neg_two = -2
//  var neg_one = -1
//  var three = 3
//  var four = 4
//  var five = 5
//  property("DFBits[4].bits(two,1)") = {
//    val a = DFBits[4]; val b = a.bits(two,1)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 2
//  }
//  property("DFBits[4].bits(2,one)") = {
//    val a = DFBits[4]; val b = a.bits(2,one)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 2
//  }
//  property("DFBits[4].bits(two,one)") = {
//    val a = DFBits[4]; val b = a.bits(two,one)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 2
//  }
//  property("DFBits[4].bits(1,one)") = {
//    val a = DFBits[4]; val b = a.bits(1,one)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 1
//  }
//  property("DFBits[4].bits(one,1)") = {
//    val a = DFBits[4]; val b = a.bits(one,1)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 1
//  }
//  property("DFBits[4].bits(one,one)") = {
//    val a = DFBits[4]; val b = a.bits(one,one)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 1
//  }
//  property("DFBits[4].msbits(two)") = {
//    val ttwo = 2
//    val a = DFBits[4]; val b = a.resizeRight(ttwo)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 2
//  }
//  property("DFBits[4].lsbits(two)") = {
//    val a = DFBits[4]; val b = a.resize(two)
//    implicitly[b.Width =:= Int]
//    b.width.getValue == 2
//  }
//  property("DFBits[4] out of range bits range selections exception") = wellTyped {
////    illRun { val a = DFBits[4]; a.bits(neg_one,neg_two) }
////    illRun { val a = DFBits[4]; a.bits(one,neg_two) }
////    illRun { val a = DFBits[4]; a.bits(one,three) }
////    illRun { val a = DFBits[4]; a.resizeRight(neg_one) }
////    illRun { val a = DFBits[4]; a.resize(neg_one) }
//  }
//  property("DFBits[4].bit(1)") = {
//    val a = DFBits[4]; val b = a.bit(1)
//    implicitly[b.Width =:= 1]
//    b.width.getValue == 1
//  }
//  property("DFBits[4] out of range single bit selections compile error") =  wellTyped {
////    illRun { val a = DFBits[4]; a.bit(four) }
////    illRun { val a = DFBits[4]; a.bit(neg_one) }
//  }
//}