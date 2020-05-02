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

package RISCV

/**************************************************************************************************************
  * DO NOT MODIFY THIS FILE
  *************************************************************************************************************/

import DFiant._
import DFiant.internals.BitVectorExtras

case class IMemEntry(addr : BitVector, inst : BitVector, instStr : String)
case class DMemEntry(addr : BitVector, data : BitVector)

case class ProgramIMem(list : List[IMemEntry], startAddress : BitVector, finishAddress : BitVector, failAddress : Option[BitVector])
case class ProgramDMem(list : List[DMemEntry]) {
  def toInitArr(size : Int) : Array[BitVector] = {
    val initArr = Array.fill(size)(BitVector.low(32))
    list.foreach(e => {
      val i = e.addr.bits(9, 2).toBigInt.toInt
      e.data.length match {
        case 32 => e.addr.bits(1, 0).toBigInt.toInt match {
          case 0 => initArr(i) = e.data
          case 1 =>
            initArr(i) = e.data.bits(23, 0) ++ initArr(i).bits(7, 0)
            initArr(i+1) = initArr(i+1).bits(31, 8) ++ e.data.bits(31, 24)
          case 2 =>
            initArr(i) = e.data.bits(15, 0) ++ initArr(i).bits(15, 0)
            initArr(i+1) = initArr(i+1).bits(31, 16) ++ e.data.bits(31, 16)
          case 3 =>
            initArr(i) = e.data.bits(7, 0) ++ initArr(i).bits(23, 0)
            initArr(i+1) = initArr(i+1).bits(31, 24) ++ e.data.bits(31, 8)
        }
        case 16 => e.addr.bits(1, 0).toBigInt.toInt match {
          case 0 => initArr(i) = initArr(i).bits(31, 16) ++ e.data
          case 1 => initArr(i) = initArr(i).bits(31, 24) ++ e.data ++ initArr(i).bits(7, 0)
          case 2 => initArr(i) = e.data ++ initArr(i).bits(15, 0)
          case 3 =>
            initArr(i) = e.data.bits(7, 0) ++ initArr(i).bits(23, 0)
            initArr(i+1) = initArr(i+1).bits(31, 8) ++ e.data.bits(15, 8)
        }
        case 8 => e.addr.bits(1, 0).toBigInt.toInt match {
          case 0 => initArr(i) = initArr(i).bits(31, 8) ++ e.data
          case 1 => initArr(i) = initArr(i).bits(31, 16) ++ e.data ++ initArr(i).bits(7, 0)
          case 2 => initArr(i) = initArr(i).bits(31, 24) ++ e.data ++ initArr(i).bits(15, 0)
          case 3 => initArr(i) = e.data ++ initArr(i).bits(23, 0)
        }
      }
    })
    initArr
  }
}
case class Program(imem : ProgramIMem, dmem : ProgramDMem)

object Program {
  import scala.io.Source
  private val extractor = """[ \t]*([0-9a-f]+):[ \t]*([0-9a-f]+)[ \t]*(.+)""".r
  private val mainExtractor = """[ \t]*([0-9a-f]+) <main>:[ \t]*""".r
  private val testExtractor = """[ \t]*([0-9a-f]+) <test_2>:[ \t]*""".r
  private val failExtractor = """[ \t]*([0-9a-f]+) <fail>:[ \t]*""".r
  private val passExtractor = """[ \t]*([0-9a-f]+) <pass>:[ \t]*""".r
  private val dataStart = "Disassembly of section .data:"
  private def imemFromFile(progMemFile : String) : ProgramIMem = {
    val file = Source.fromFile(progMemFile)
    var mainAddr : Option[BitVector] = None
    var endAddr : Option[BitVector] = None
    var failAddr : Option[BitVector] = None
    var reachedData : Boolean = false
    val list = file.getLines.collect {
      case extractor(addr, inst, asm) if mainAddr.isDefined && endAddr.isEmpty && asm == "ret" & !reachedData =>
        endAddr = Some(BitVector.fromHex(addr).get.resize(32))
        Some(IMemEntry(endAddr.get, BitVector.fromHex(inst).get, asm))
      case extractor(addr, inst, asm) if !reachedData =>
        if (inst.length < 8) None
        else Some(IMemEntry(BitVector.fromHex(addr).get.resize(32), BitVector.fromHex(inst).get, asm))
      case mainExtractor(addr) =>
        mainAddr = Some(BitVector.fromHex(addr).get.resize(32))
        None
      case testExtractor(addr) =>
        mainAddr = Some(BitVector.fromHex(addr).get.resize(32))
        None
      case passExtractor(addr) =>
        endAddr = Some(BitVector.fromHex(addr).get.resize(32))
        None
      case failExtractor(addr) =>
        failAddr = Some(BitVector.fromHex(addr).get.resize(32))
        None
      case l if l == dataStart =>
        reachedData = true
        None
    }.toList.flatten

    file.close
    ProgramIMem(list, mainAddr.getOrElse(list.head.addr), endAddr.getOrElse(list.last.addr), failAddr)
  }

  private def dmemFromFile(progMemFile : String) : ProgramDMem = {
    val file = Source.fromFile(progMemFile)
    var reachedData : Boolean = false
    val list = file.getLines.collect {
      case extractor(addr, inst, asm) if reachedData =>
        Some(DMemEntry(BitVector.fromHex(addr).get.resize(32), BitVector.fromHex(inst).get))
      case l if l == dataStart =>
        reachedData = true
        None
    }.toList.flatten
    file.close
    ProgramDMem(list)
  }
  def fromFile(progMemFile : String) : Program = Program(imemFromFile(progMemFile), dmemFromFile(progMemFile))


  def empty() : ProgramIMem =
    ProgramIMem(List(), BitVector.fromHex("0").get.resize(32), BitVector.fromHex("0").get.resize(32), None)
}


