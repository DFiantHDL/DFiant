package DFiant
package sim.tools

import java.io.{FileWriter, File}

import DFiant.compiler.backend.verilog.{Revision, Backend}
import DFiant.sim.{DFSimDesign, Simulation}

import scala.language.postfixOps
import scala.sys.process._
import constraints.timing.sync._

final case class VerilatorSimulation[D <: DFSimDesign, R <: Revision](
  db : DFDesign.DB,
  fileNameSeq : Seq[String],
  programFile : String = "verilator", //default: assumes the program 'verilator' is in path
  workDir : String = "obj_dir",     //default: work folder at current path
  userFlags : String = ""        //default: no additional user flags
)(implicit revision: R) extends Simulation[D, Backend[R]] {
  import db.__getset
  private val topVerilogFilePath = fileNameSeq.last
  private val dir = {
    val f = new File(topVerilogFilePath)
    f.getParentFile.getAbsolutePath
  }
  private val topVerilogFileName = {
    val f = new File(topVerilogFilePath)
    f.getName
  }
  private val Mdir = s"$dir\\$workDir"
  private val topName = db.top.designType
  private val mainSimFileName : String = s"${topName}_sim.cpp"
  private val mainSimFilePath : String = s"$dir\\$mainSimFileName"
  private val clkName = ClockParams.get.name
  private val clkActive : Int = ClockParams.get.activeInt
  private val clkInactive : Int = ClockParams.get.inactiveInt
  private val rstName = ResetParams.get.name
  private val rstActive : Int = ResetParams.get.activeInt
  private val rstInactive : Int = ResetParams.get.inactiveInt
  private val VerilatedTop = s"V$topName"
  private val simProgFilePath : String = s"$Mdir\\$VerilatedTop"
  private val mainSimContents : String =
    s"""#include <stdlib.h>
       |#include "$VerilatedTop.h"
       |#include "verilated.h"
       |
       |int main(int argc, char **argv) {
       |  // Initialize Verilators variables
       |  Verilated::commandArgs(argc, argv);
       |
       |  // Create an instance of our module under test
       |  $VerilatedTop *tb = new $VerilatedTop;
       |
       |  // Toggle initial reset with clock
       |  tb->$rstName = $rstActive;
       |  tb->$clkName = $clkInactive;
       |  tb->eval();
       |  tb->$clkName = $clkActive;
       |  tb->eval();
       |  tb->$clkName = $clkInactive;
       |  tb->$rstName = $rstInactive;
       |  tb->eval();
       |  while(!Verilated::gotFinish()) {
       |    tb->$clkName = $clkActive;
       |    tb->eval();
       |    tb->$clkName = $clkInactive;
       |    tb->eval();
       |  } exit(EXIT_SUCCESS);
       |}
       |""".stripMargin
  private def writeMainSimFile() : Unit = {
    println(s"Writing Verilator simulation file $mainSimFileName")
    val f = new FileWriter(new File(mainSimFilePath))
    f.write(mainSimContents)
    f.close()
  }
  private val buildCmd = s"$programFile -Wall -Wno-UNUSED -cc $topVerilogFileName --exe --build $mainSimFileName"
  private def bashRun(dir : String, cmd : String) : Unit = {
    s"""bash -l -c "cd '$dir'; $cmd" """ !!
  }
  private def buildSim() : Unit = {
    print("Building Verilator simulation... ")
//    println(buildCmd)
    bashRun(dir, buildCmd)
    println("Done!")
  }
  private lazy val initSim : Unit = {
    writeMainSimFile()
    buildSim()
  }
  def run() : this.type = {
    initSim
    println("Running simulation...")

    {simProgFilePath !}
    this
  }
}
