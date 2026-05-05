`timescale 1ns/1ps

module keypad #(N = 300_000) (
  input clk, rst_n,
  input [3:0] row,
  output [3:0] col,
  output reg [15:0] keys
);

  wire scan;
  reg [1:0] state;

  initial begin
    state = 0;
    keys = 0;
  end

  clock_divider #(.N(N)) clkdiv (.clk(clk), .rst_n(rst_n), .div_clk(scan));

  assign col[0] = (state == 0) ? 0 : 1'bz;
  assign col[1] = (state == 1) ? 0 : 1'bz;
  assign col[2] = (state == 2) ? 0 : 1'bz;
  assign col[3] = (state == 3) ? 0 : 1'bz;

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      state <= 0;
      keys <= 0;
    end
    else if(scan) begin
      case (state)
        0: begin
          keys[4'hC] <= ~row[0];
          keys[4'hD] <= ~row[1];
          keys[4'hE] <= ~row[2];
          keys[4'hF] <= ~row[3];
        end
        1: begin
          keys[4'h8] <= ~row[0];
          keys[4'h9] <= ~row[1];
          keys[4'hA] <= ~row[2];
          keys[4'hB] <= ~row[3];
        end
        2: begin
          keys[4'h4] <= ~row[0];
          keys[4'h5] <= ~row[1];
          keys[4'h6] <= ~row[2];
          keys[4'h7] <= ~row[3];
        end
        3: begin
          keys[4'h0] <= ~row[0];
          keys[4'h1] <= ~row[1];
          keys[4'h2] <= ~row[2];
          keys[4'h3] <= ~row[3];
        end
      endcase
      state <= state + 1;
    end
  end

endmodule
