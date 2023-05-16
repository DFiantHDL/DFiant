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
//import dfhdl._
//
//class PortArgDesign(val ti : UInt[8] <> IN, val to : UInt[8] <> OUT)(implicit ctx : DFDesign.ContextOf[PortArgDesign]) extends DFDesign {
//  val padInt = new PortArgDesignInt(ti, to)
//}
//
//class PortArgDesignInt(val ti : UInt[8] <> IN, val to : UInt[8] <> OUT)(implicit ctx : DFDesign.ContextOf[PortArgDesignInt]) extends DFDesign {
//  to <> ti + ti//.bits.uint
//}
//
//trait PortArgTest extends DFDesign {
//  val i = UInt(8) <> IN
//  val o = UInt(8) <> OUT
//
//  val temp = UInt(8)
//  val io1 = new PortArgDesign(i, temp) {}
//  val io2 = new PortArgDesign(temp, o) {}
//}
//
//object PortArgApp extends DFApp {
//  val paTest = new PortArgTest {}.printCodeString
//  import internals._
//}
