`timescale 1ns/1ps


module delayTimer 
  (
   input 	clk,
   input 	rst_n,
   input 	clr,
   input [31:0] N,
   output reg 	t
   );

   reg [31:0]   count;
   
   initial begin
      t = 0;      
   end

   always @(posedge clk, negedge rst_n) begin
      if (!rst_n) begin
	 // Reset behavior:
	 t     <= 0;
	 count <= 0;	 
      end
      else begin
	 // Normal behavior:
	 if (clr) begin
	    count <= 0;
	    t     <= 0;	    
	 end
	 else begin
	    if (count >= N) begin
	       t <= 1;	       
	    end
	    else begin
	       t     <= 0;	       
	       count <= count + 1;	       
	    end	    
	 end
      end
   end
   
endmodule
