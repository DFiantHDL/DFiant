/*
 *     This file is part of DFHDL.
 *
 *     DFHDL is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFHDL is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFHDL.  If not, see <https://www.gnu.org/licenses/>.
 */

package dfhdl

import dfhdl.compiler.csprinter.CSPrinter

/**
  * A dataflow bit companion object.
  * Most inter-workings are relying on a Bool
  */
object Bit {
  type Type = Bool.Type
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Construct a new dataflow bit
    * @param ctx An implicit dataflow design context
    */
  def unapply(arg: DFAny.Member): Boolean = arg.dfType match {
    case Bool.Type(logical) if !logical => true
    case _ => false
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
