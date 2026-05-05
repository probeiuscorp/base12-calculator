`timescale 1ns/1ps

module clock_divider #(N = 300_000) (
  input clk, rst_n,
  output reg div_clk
);

  integer clk_count;
  initial begin
    div_clk = 0;
    clk_count = 0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      div_clk <= 0;
      clk_count <= 0;
    end
    else begin
      if (clk_count == N) begin
        clk_count <= 0;
        div_clk <= 1;
      end
      else begin
        clk_count <= clk_count + 1;
        div_clk <= 0;
      end
    end
  end

endmodule
