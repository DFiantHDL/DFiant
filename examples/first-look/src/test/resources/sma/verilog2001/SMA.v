`default_nettype	           none
`timescale 1ns/1ps
`include "SMA_defs.v"


module SMA(
  input  wire               clk,
  input  wire               rst,
  input  wire signed [15:0] x,
  output reg  signed [15:0] y
);
  reg         signed [15:0] x_prev1 = 16'sd0;
  reg         signed [15:0] x_prev2 = 16'sd0;
  reg         signed [15:0] x_prev3 = 16'sd0;
  reg         signed [15:0] sum = 16'sd0;
  reg         signed [15:0] x_prev1_sig;
  reg         signed [15:0] x_prev2_sig;
  always @(*)
  begin
    sum                     = (x + x_prev1) + (x_prev2 + x_prev3);
    x_prev1_sig             = x_prev1;
    x_prev2_sig             = x_prev2;
    y                       = sum;
  end
  always @(negedge rst or posedge clk)
  begin
    if (rst == 1'b0) 
    begin
      x_prev1               <= 16'sd0;
      x_prev2               <= 16'sd0;
      x_prev3               <= 16'sd0;
    end
    else 
    begin
      x_prev1               <= x;
      x_prev2               <= x_prev1_sig;
      x_prev3               <= x_prev2_sig;
    end
  end
endmodule