package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.verilog.{getVerilogCode}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}, {code = ":=="}]}

class PrintVerilogCodeSpec extends StageSpec:
  class ID extends EDDesign:
    val x  = SInt(16) <> IN
    val y  = SInt(16) <> OUT
    val y2 = SInt(16) <> OUT
    y  <> x
    y2 <> x

  class IDTop extends EDDesign:
    self =>
    val x     = SInt(16) <> IN
    val y     = SInt(16) <> OUT
    val id1_x = SInt(16) <> VAR
    val id1_y = SInt(16) <> VAR
    val id2_x = SInt(16) <> VAR
    val id2_y = SInt(16) <> VAR
    val id1 = new ID:
      this.x  <> id1_x
      this.y  <> id1_y
      this.y2 <> OPEN
    val id2 = new ID:
      this.x  <> id2_x
      this.y  <> id2_y
      this.y2 <> OPEN
    id1_x <> x
    id2_x <> id1_y
    y     <> id2_y
  end IDTop

  test("Basic ID design") {
    val id = (new ID).getVerilogCode
    assertNoDiff(
      id,
      """|`ifndef ID_DEFS
         |`define ID_DEFS
         |
         |
         |`endif
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "ID_defs.sv"
         |
         |module ID(
         |  input wire logic signed [15:0] x,
         |  output logic signed [15:0] y,
         |  output logic signed [15:0] y2
         |);
         |  assign y = x;
         |  assign y2 = x;
         |endmodule
         |""".stripMargin
    )
  }

  test("Basic hierarchy design") {
    val top = (new IDTop).getVerilogCode
    assertNoDiff(
      top,
      """|`ifndef IDTOP_DEFS
         |`define IDTOP_DEFS
         |
         |
         |`endif
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module ID(
         |  input wire logic signed [15:0] x,
         |  output logic signed [15:0] y,
         |  output logic signed [15:0] y2
         |);
         |  assign y = x;
         |  assign y2 = x;
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module IDTop(
         |  input wire logic signed [15:0] x,
         |  output logic signed [15:0] y
         |);
         |  logic signed [15:0] id1_x;
         |  logic signed [15:0] id1_y;
         |  logic signed [15:0] id2_x;
         |  logic signed [15:0] id2_y;
         |  ID id1(
         |    .x /*<--*/ (id1_x),
         |    .y /*-->*/ (id1_y),
         |    .y2 /*-->*/ (/*open*/)
         |  );
         |  ID id2(
         |    .x /*<--*/ (id2_x),
         |    .y /*-->*/ (id2_y),
         |    .y2 /*-->*/ (/*open*/)
         |  );
         |  assign id1_x = x;
         |  assign id2_x = id1_y;
         |  assign y = id2_y;
         |endmodule
         |""".stripMargin
    )
  }
  test("process block") {
    class Top extends EDDesign:
      val clk = Bit      <> IN
      val rst = Bit      <> IN
      val x   = Bits(16) <> IN
      val y   = Bits(16) <> OUT
      val z   = Bits(16) <> VAR
      process(clk, rst) {
        val c: Bits[16] <> CONST = all(0)
        if (rst)
          y :== c
        else if (clk.rising)
          y :== x
      }
      val myblock = process(all) {
        val my_var = Bits(16) <> VAR
        my_var := x
        y     :== my_var
      }
      process.forever {
        z :== x
        y :== z
      }
    end Top
    val top = (new Top).getVerilogCode(align = true)
    assertNoDiff(
      top,
      """|`ifndef TOP_DEFS
         |`define TOP_DEFS
         |
         |
         |`endif
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.sv"
         |
         |module Top(
         |  input wire logic        clk,
         |  input wire logic        rst,
         |  input wire logic [15:0] x,
         |  output logic [15:0]     y
         |);
         |  logic [15:0] z;
         |  parameter logic [15:0] c = 16'h0000;
         |  logic [15:0] my_var;
         |  always @(posedge clk, posedge rst)
         |  begin
         |    if (rst) y <= c;
         |    else y <= x;
         |  end
         |  myblock : always @(*)
         |  begin
         |    my_var = x;
         |    y      <= my_var;
         |  end
         |  always
         |  begin
         |    z      <= x;
         |    y      <= z;
         |  end
         |endmodule
         |""".stripMargin
    )
  }
  test("literals") {
    class Top extends EDDesign:
      val c01: Bit <> CONST            = 0
      val c02: Bit <> CONST            = 1
      val c03: Bit <> CONST            = ?
      val c04: Boolean <> CONST        = false
      val c05: Boolean <> CONST        = true
      val c06: Bits[8] <> CONST        = h"22"
      val c07: Bits[7] <> CONST        = h"7'22"
      val c08: Bits[3] <> CONST        = b"101"
      val c09: UInt[3] <> CONST        = 7
      val c10: UInt[48] <> CONST       = d"48'239794508230343"
      val c11: SInt[4] <> CONST        = -8
      val c12: SInt[49] <> CONST       = sd"49'-239794508230343"
      val c13: UInt[8] <> CONST        = ?
      val c14: SInt[8] <> CONST        = ?
      val c15: (Bits[3], Bit) <> CONST = (all(0), 1)
    end Top
    val top = (new Top).getVerilogCode
    assertNoDiff(
      top,
      """|`ifndef TOP_DEFS
         |`define TOP_DEFS
         |
         |
         |`endif
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.sv"
         |
         |module Top;
         |  typedef struct packed {
         |    logic [2:0] _1;
         |    logic _2;
         |  } DFTuple2;
         |  parameter logic c01 = 1'b0;
         |  parameter logic c02 = 1'b1;
         |  parameter logic c03 = 1'bx;
         |  parameter logic c04 = 0;
         |  parameter logic c05 = 1;
         |  parameter logic [7:0] c06 = 8'h22;
         |  parameter logic [6:0] c07 = 7'h22;
         |  parameter logic [2:0] c08 = 3'h5;
         |  parameter logic [2:0] c09 = 3'd7;
         |  parameter logic [47:0] c10 = 48'd239794508230343;
         |  parameter logic signed [3:0] c11 = -4'sd8;
         |  parameter logic signed [48:0] c12 = -49'sd239794508230343;
         |  parameter logic [7:0] c13 = 8'hxx;
         |  parameter logic signed [7:0] c14 = $signed(8'hxx);
         |  parameter DFTuple2 c15 = '{3'h0, 1'b1};
         |endmodule
         |""".stripMargin
    )
  }

  test("Docstrings"):
    /** HasDocs has docs */
    class HasDocs extends DFDesign:
      /** My in */
      val x = Bit <> IN

      /** My Out
        */
      val y = Bit <> OUT

      /** My very very very very very very very very very very very very very very very very very
        * very very very very very very very very very very very very very very very very very very
        * very very very very very very very very very very long doc
        */
      val z = Bit <> VAR

    val top = (new HasDocs).getVerilogCode
    assertNoDiff(
      top,
      """|`ifndef HASDOCS_DEFS
         |`define HASDOCS_DEFS
         |
         |
         |`endif
         |
         |/* HasDocs has docs */
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "HasDocs_defs.sv"
         |
         |module HasDocs(
         |  /* My in */
         |  input wire logic x,
         |  /* My Out
         |    */
         |  output logic y
         |);
         |  /* My very very very very very very very very very very very very very very very very very
         |     very very very very very very very very very very very very very very very very very very
         |     very very very very very very very very very very long doc
         |    */
         |  logic z;
         |
         |endmodule
         |""".stripMargin
    )
end PrintVerilogCodeSpec
