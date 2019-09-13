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

trait Fib extends DFDesign {
  val out = DFUInt[64] <> OUT
  val fib = DFUInt[64] init (1, 0)
  fib := fib.prev + fib.prev(2)
  out := fib.prev(2)
}
