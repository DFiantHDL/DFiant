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
package fpga2019
import DFiant._

trait SeqDet extends DFDesign {
  val seqIn  = DFBool() <> IN
  val detOut = DFBool() <> OUT
  object State extends Enum.Auto {
    val S0, S1, S10, S100, S1001 = Entry
  }
  val state = DFEnum(State) init State.S0
  matchdf(state)
    .casedf(State.S0) {
      detOut := 0
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S0}
    }.casedf(State.S1) {
      detOut := 0
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S10}
    }.casedf(State.S10) {
      detOut := 0
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S100}
    }.casedf(State.S100) {
      detOut := 0
      ifdf (seqIn) {state := State.S1001}
      .elsedf      {state := State.S0}
    }.casedf(State.S1001) {
      detOut := 1
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S10}
    }
}
