import DFiant._

package object RISCV {
  final val XLEN = 32
  type XLEN = XLEN.type
  final val regsNum = 0 until 32

  sealed trait MicroArchitecture
  case object OneCycle extends MicroArchitecture
  case object TwoCycle extends MicroArchitecture

  val microArchitecture : MicroArchitecture = OneCycle

  val caseIMem : Boolean = true //when true, implements the IMem for synthesis as a case statement
  val caseDMem : Boolean = true //when true, implements the DMem for synthesis as a case statement

  final val NOPInst = h"00000013"
  object DebugOp extends EnumType.Auto {
    val Unsupported, LUI, AUIPC, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU,
    LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, SLTI, SLTIU, XORI, ORI, ANDI,
    SLLI, SRLI, SRAI, ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
    FENCE, FENCE_I, ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI = Entry()
  }
  type DebugOp = DebugOp.type

  object BranchSel extends EnumType.Auto {
    val Next, BNE, BEQ, BGE,
    BGEU, BLT, BLTU, JAL, JALR = Entry()
  }
  type BranchSel = BranchSel.type

  object RS1OpSel extends EnumType.Auto {
    val RegSource, Immediate  = Entry()
    val DontCare = RegSource //Giving another name to an entry as a Don't Care value
  }
  type RS1OpSel = RS1OpSel.type

  object RS2OpSel extends EnumType.Auto {
    val RegSource, Immediate, PC  = Entry()
    val DontCare = RegSource
  }
  type RS2OpSel = RS2OpSel.type

  object ALUSel extends EnumType.Auto {
    val ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1 = Entry()
    val DontCare = ADD
  }
  type ALUSel = ALUSel.type

  object WriteBackSel extends EnumType.Auto {
    val ALU, Mem, PCPlus4, CSR = Entry()
    val DontCare = ALU
  }
  type WriteBackSel = WriteBackSel.type

  object DMemSel extends EnumType.Auto {
    val LB, LH, LW, LBU, LHU, SB, SH, SW = Entry()
    val DontCare = LW
  }
  type DMemSel = DMemSel.type

}

