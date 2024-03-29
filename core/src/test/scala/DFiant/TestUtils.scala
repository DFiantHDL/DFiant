/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import compiler.printer.formatter._

object TestUtils {
  def sameType[A, B](implicit ev: A =:= B): Boolean = true

  //nf = not-final. used to force a not-final value. e.g., nf(3) returns a non-literal 3
  def nf(t : Int) = t
  def nf(t : Long) = t

  def illRun(body: => Unit) : Boolean = {
    val isIll = try {
      body
      false
    } catch {
      case _ : Throwable =>
        true
    }
    if (!isIll)
      assert(false, "Expected assertion did not occur")
    true
  }
  def illRunCompare(msg : String)(body: => Unit) : Boolean = {
    val illMsg = try {
      body
      "Expected exception not raised"
    } catch {
      case e  : Throwable =>
        e.getMessage
    }
    illMsg =@= msg
  }

  def trimWhites(s : String) : String = s.replaceAll("(?m)^[\\s&&[^\\n]]+|[\\s+&&[^\\n]]+$", "").trim.filter(_ >= ' ')

  implicit class StringEnhancer(s : String) {
    def =@= (that : String) : Boolean = {
      val ret = trimWhites(s.uncolor) == trimWhites(that.uncolor)
      if (!ret) println(s)
      ret
    }
  }
}
