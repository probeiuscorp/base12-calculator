`timescale 1ns/1ps

module spi
  #(
    parameter N=100
    )
  (
   input       clk,
   input       rst_n,
   input       en, 
   input [7:0] data,
   input       send,
   output reg  rdy,
   output reg  sclk,
   output reg  sdo
   );

   reg [7:0]   count;
   initial begin
      count = 0;

   end

   localparam IDLE=0;
   localparam SEND=1;
   localparam DONE=2;

   reg [3:0] state;
   reg [3:0] idx;

   always @(posedge clk, negedge rst_n) begin
      if (!rst_n) begin
	 sclk <= 1;
	 count <= 0;
      end
      else begin
	 if (!en) begin
	    count <= 0;
	    sclk <= 1;
	 end
	 else if (count >= (N<<1)) begin
	    count <= 0;	    
	 end
	 else if (count >= N) begin
	    count <= count + 1;
	    sclk  <= 1;
	 end
	 else begin
 	    sclk <= 0;	    
	    count <= count + 1;
	 end
      end
   end


   
   
   initial begin
      state = IDLE;
      idx   = 7;      
   end

   reg sclk_d;
   
   always @(posedge clk, negedge rst_n) begin
      if (!rst_n) begin
	 // Reset behavior:
	 state <= IDLE;
	 idx   <= 7;	 
      end
      else begin
	 // Normal behavior:
	 sclk_d <= sclk;

	 if (!en) begin
	    state <= IDLE;
	    rdy   <= 1;
	 end
	 else begin
	 case (state)
	   IDLE: begin
	      idx <= 7;		 
	      if (send && !sclk && sclk_d) begin
		 state <= SEND;
		 rdy   <= 0;		 
	      end
	   end
	   SEND: begin
	      sdo    <= data[idx];
	      if (idx==0) begin
		 if (sclk && !sclk_d) begin
		    //idx <= 7;		    
		    state <= DONE;
		 end
	      end
	      else if (!sclk && sclk_d) begin
		 idx   <= idx - 1; 
	      end	      
	   end
	   DONE: begin	
	      if (!send) begin
		state <= IDLE;
		 rdy <= 1;
	      end
	      //rdy   <= 1;
	   end
	 endcase // case (state)
	 end	 
      end
   end
   
endmodule
