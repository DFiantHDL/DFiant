package RISCV
import DFiant._

trait Proc extends DFDesign {
  private val pc = DFBits[32] init StartAddress
  pc.keep

  ////////////////////////////////////////////////////////////////////////
  // Fetch
  ////////////////////////////////////////////////////////////////////////
  private val imem = new IMem(pc)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Decode
  ////////////////////////////////////////////////////////////////////////
  private val decoder = new Decoder(imem.inst)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Register File (Read)
  ////////////////////////////////////////////////////////////////////////
  private val regFile = new RegFile {}
  private val rs1_data = regFile.readConn1(decoder.inst.rs1_addr)
  private val rs2_data = regFile.readConn2(decoder.inst.rs2_addr)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // ALU (Execute)
  ////////////////////////////////////////////////////////////////////////
  private val execute = new Execute {}
  private val (pcCalc, executeInst) = execute.exConn(pc, decoder.inst, rs1_data, rs2_data)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Memory
  ////////////////////////////////////////////////////////////////////////
  private val dmem = new DMem {}
  private val dmem_dataFromMem = dmem.readWriteConn(executeInst.dmem_addr, executeInst.dataToMem, executeInst.dmemSel)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Write Back
  ////////////////////////////////////////////////////////////////////////
  private val wbData = DFBits[32].matchdf(executeInst.wbSel)
    .casedf(WriteBackSel.ALU)     {executeInst.aluOut}
    .casedf(WriteBackSel.PCPlus4) {pcCalc.pcPlus4}
    .casedf_                      {dmem_dataFromMem}

  regFile.writeConn(decoder.inst.rd_addr, wbData, decoder.inst.rd_wren)
  ////////////////////////////////////////////////////////////////////////

  pc := pcCalc.pcNext
}

object ProcTest extends App {
  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
}