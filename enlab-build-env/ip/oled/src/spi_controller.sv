`timescale 1ns/1ps


module spi_controller 
  (
   input       clk,
   input       rst_n,
   input [1:0] numbytes,
   input [7:0] cmd [2:0],
   input       send,
   output reg  rdy,

   output sclk,
   output sdo,
   output reg cs_n
   
	 );

   // Divider ratio for sclk, 1MHz default
   parameter N=200;

   reg 	      en;
   
   reg 	      spisend;
   reg [7:0]  spidata;
   integer    bytecount;
   
   wire      t;
   reg 	     tclr;
   reg [31:0] delay;

   localparam Dshort=200;
   localparam Dlong=400;
   
   delayTimer Timer 
  (
   .clk(clk),
   .rst_n(rst_n),
   .clr(tclr),
   .N(delay),
   .t(t)
   );

   initial begin
      delay = Dshort;
   end
   
   // Bus interface:
   spi #(.N(N)) DUT
     (
      .clk(clk),
      .rst_n(rst_n),
      .en(en),
      .send(spisend),
      .data(spidata),
      .rdy(spirdy),
      .sclk(sclk),
      .sdo(sdo)
      );

   reg [2:0]	      state;
   localparam IDLE  = 0;
   localparam SEND  = 1;
   localparam START = 2;
   localparam STOP  = 3;
   localparam WAIT  = 4;
   localparam CHECK = 5;
   
   
   always @(posedge clk, negedge rst_n) begin
      if (!rst_n) begin
	 // Reset behavior:
	 spisend   <= 0;
	 spidata   <= 0;
	 rdy       <= 0;
	 bytecount <= 0;
	 cs_n  <= 1;
	 en    <= 0;
	 tclr  <= 1;
	 delay <= Dshort;
	 
      end
      else begin
	 // Normal behavior:
	 case (state)
	   IDLE: begin
	      if (send) begin
		 rdy   <= 0;
		 state <= CHECK;
	      end
	      else
		rdy <= 1;
	      
	   end
	   CHECK: begin
	      if (spirdy && (bytecount < numbytes)) begin
		 rdy     <= 0;
		 cs_n    <= 0;
		 spidata <= cmd[bytecount];
		 bytecount++;		    
		 tclr    <= 0;
		 delay   <= Dshort;
		 state   <= START;
	      end 
	      else if (bytecount == numbytes) begin
		 bytecount <= 0;
		 state     <= IDLE;		 
		 rdy       <= 1;
		 en        <= 0;
		 tclr      <= 1;		 
	      end
	   end // case: IDLE
	   START: begin
	      if (t) begin
		 spisend <= 1;
		 state   <= SEND;		 
		 en      <= 1;
		 tclr    <= 1;		 
	      end
	   end
	   SEND: begin	      
	      if (!spirdy) begin		 
		 spisend <= 0;
	      end
	      else if (!spisend && spirdy) begin
		 tclr  <= 0;
		 en    <= 0;
		 state <= STOP;		 
	      end
	      
	   end
	   STOP: begin
	      if (t) begin
		 cs_n  <= 1;
		 tclr  <= 1;
		 delay <= Dlong;
		 
	      end
	      else if (cs_n) begin
		 state <= WAIT;
		 tclr  <= 0;		 
	      end
	   end
	   WAIT: begin
	      if (t) begin
		 state <= CHECK;
	      end
	   end
	   default: state <= IDLE;
	 endcase // case (state)
	 
      end
   end
   
endmodule // spi_controller
