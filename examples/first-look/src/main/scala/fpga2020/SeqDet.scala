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
package fpga2020
import DFiant._
import fsm._
@df class SeqDet extends DFDesign {
  final val seqIn  = DFBool() <> IN
  final val detOut = DFBool() <> OUT
  private val S0    : FSM = step {detOut := 0} =?> seqIn =!> S1     =!> S0
  private val S1    : FSM = step {detOut := 0} =?> seqIn =!> S1     =!> S10
  private val S10   : FSM = step {detOut := 0} =?> seqIn =!> S1     =!> S100
  private val S100  : FSM = step {detOut := 0} =?> seqIn =!> S1001  =!> S0
  private val S1001 : FSM = step {detOut := 1} =?> seqIn =!> S1     =!> S10

  private val SB0    : FSM = step {detOut := 0} =?> seqIn =!> SB1     =!> SB0
  private val SB1    : FSM = step {detOut := 0} =?> seqIn =!> SB0     =!> SB1
}

//trait SeqDetTest extends DFSimulator {
//  val TestSeq = Seq(1, 1, 0, 1, 0, 0, 1, 0, 1)
//  val seqIn = DFBool() init TestSeq.reverse
//  val dut = new SeqDet {}
//  dut.seq <> seqIn.prev(TestSeq.length)
//  sim.report(dfs"det: ${dut.det}")
//}
//
object SeqDetApp extends App {
  val seqDet = new SeqDet
  seqDet.printCodeString
}