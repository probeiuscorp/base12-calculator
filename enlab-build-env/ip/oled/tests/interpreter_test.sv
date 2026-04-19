`timescale 1ns/1ps

module interpreter_test (

	 );

   reg clk;
   reg rst_n;
   
   initial begin
      clk = 0;
      forever #10 clk = ~clk;
   end

   reg en;
   wire [7:0] p_adr;   
   reg  [7:0] p_data;

   wire vbat;
   wire vdd;
   wire res;
   wire sclk;
   wire sdo;
   wire cs_n;
   wire dc;

   // Program Memory:
   reg [7:0] pram [31:0];
   always @(*) begin
      p_data = pram[p_adr];
      
   end

   reg intr;
   reg [7:0] i_adr;
   
   interpreter DUT 
  (
   .clk(clk),
   .rst_n(rst_n),
   .en(en),
   .p_adr(p_adr),
   .p_data(p_data),
   .intr(intr),
   .i_adr(i_adr),
   .vbat(vbat),
   .vdd(vdd),
   .res(res),
   .sclk(sclk),
   .sdo(sdo),
   .cs_n(cs_n),
   .dc(dc)
   );

   spi_listener listener
  (
   .sclk(sclk),
   .sdo(sdo),
   .cs_n(cs_n),
   .dc(dc)
   );


      // Command Codes
   localparam NULL_CMD = 8'h00;

   localparam VDD_ON  = 8'h30;
   localparam VDD_OFF = 8'h31;
   localparam VBAT_ON = 8'h32;
   localparam VBAT_OFF= 8'h33;
   localparam RES_ON  = 8'h34;
   localparam RES_OFF = 8'h35;

   localparam DELAY_LONG    = 8'h0F;
   localparam DELAY_SHORT   = 8'h0E;
   localparam DISP_ALL_ON   = 8'hA5;
   localparam DISP_RAM      = 8'hA4;
   localparam DISP_ON       = 8'hAF;
   localparam DISP_OFF      = 8'hAE;
   localparam DISP_CONTRAST = 8'h81;
   localparam DISP_HORIZ_MODE = 8'h20;
   localparam CHARGEPUMP_EN   = 8'h8D;
   localparam MUXRATIO        = 8'hA8;
   localparam DISP_OFFSET     = 8'hD3;
   localparam STARTLINE       = 8'h40;
   localparam SEGMENT         = 8'hA0;
   localparam SCANDIR         = 8'hC0;
   localparam COMPINS         = 8'hDA;
   localparam DISP_NORMAL     = 8'hA6;
   localparam SETOSC          = 8'hD5;


   initial begin
      pram[0]  = VDD_ON;
      pram[1]  = DELAY_SHORT;
      pram[2]  = RES_OFF;
      pram[3]  = DELAY_SHORT;
      pram[4]  = RES_ON;
      pram[5]  = DELAY_SHORT;
      pram[6]  = RES_OFF;
      pram[7]  = DELAY_SHORT;
      pram[8]  = VBAT_ON;
      pram[9]  = DELAY_LONG;
      pram[10]  = DISP_OFF;
      pram[11]  = MUXRATIO;
      pram[12]  = DISP_OFFSET;
      pram[13]  = STARTLINE;
      pram[14]  = SEGMENT;
      pram[15]  = SCANDIR;
      pram[16]  = COMPINS;      
      pram[17]  = DISP_CONTRAST;
      pram[18]  = 8'h7F;
      pram[19] = DISP_RAM;
      pram[20] = DISP_NORMAL;
      pram[21] = SETOSC;
      pram[22] = CHARGEPUMP_EN;
      pram[23] = DISP_ON;
      pram[24] = NULL_CMD;
      pram[25] = DISP_ALL_ON;
      pram[26] = NULL_CMD;
      pram[27] = DISP_RAM;
      pram[28] = NULL_CMD;
      
      rst_n = 0;
      en    = 0;

      intr  = 0;
      i_adr = 25;
      
      $monitor("%t: p_adr=%d p_data=%2h",$time,p_adr,p_data);
      $monitor("%t: vdd=%b",$time,vdd);
      $monitor("%t: vbat=%b",$time,vbat);
      $monitor("%t: res=%b",$time,res);
      $monitor("%t: state=%d",$time,DUT.state);
      $monitor("%t: timer=%b",$time,DUT.t);
      
   end

   wire      t;
   reg 	     tclr;
   reg [31:0] N;

   delayTimer Timer 
  (
   .clk(clk),
   .rst_n(rst_n),
   .clr(tclr),
   .N(N),
   .t(t)
   );

   initial begin
      tclr = 1;
      N = 100_000;
      
   end
   
   always @(posedge clk) begin
      rst_n <= 1;

      if (rst_n)
	en <= 1;

      if (p_adr == 24) begin
	 tclr <= 0;
	 if (t) begin
	    intr <= 1;
	    i_adr <= 25;
	 end	 
      end
      else if (p_adr == 26) begin
	 tclr <= 0;
	 if (t) begin
	    intr <= 1;
	    i_adr <= 27;
	 end	 	 
      end
      else
	intr <= 0;
      
   end


   integer clk_count;
   initial clk_count = 0;
   always @(posedge clk) begin
      clk_count <= clk_count + 1;
      if (p_adr == 28) begin
	 // Things to do at finish
	 $finish;
      end
   end

   
endmodule
