`timescale 1ns/1ps

module oledDriver (
		   input       clk,
		   input       rst_n,

		   input       showchar,
		   input       showbmp,
		   input       clear,
		   
		   input [7:0] charval,
		   input [1:0] char_row,
		   input [3:0] char_col,

		   input [1:0] bmp,
		   
		   output reg  ready,
		   
		   // OLED header signals:
		   output      cs_n,
		   output      sdo,
		   output      sclk,
		   output      dc,
		   output      vbat,
		   output      vdd,
		   output      res

	 );

   reg 	      en;
   
   
   // Program Memory and Signals
   wire [7:0] p_adr;   
   reg  [7:0] p_data;
   reg  [7:0] pram [60:0];
   
   always @(*) begin
      p_data = pram[p_adr];   
   end

   // "Interrupt" signal and address
   reg intr;
   reg [7:0] i_adr;

   // Command Interpreter:
   wire      intr_ready;
   
   interpreter cmdInterp
  (
   .clk(clk),
   .rst_n(rst_n),
   .en(en),
   .ready(intr_ready),
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

   
      // Command Codes
   `include "ip/oled/inc/command_codes.sv"


   integer   ld_data_a_ptr;
   integer   ld_data_b_ptr;
   integer   ld_data_c_ptr;
   integer   ld_data_d_ptr;
   integer   putchar_ptr;
   integer   clear_screen_ptr;
   

   initial begin
      en    = 1;
      ready = 0;
      
      //startup sequence
      pram[0]  = VDD_OFF;
      pram[1]  = DELAY_SHORT;
      pram[2]  = RES_OFF;
      pram[3]  = DELAY_SHORT;
      pram[4]  = RES_ON;
      pram[5]  = DELAY_SHORT;
      pram[6]  = RES_ON;
      pram[7]  = DELAY_SHORT;
      pram[8]  = VBAT_OFF;
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


      // clear display content
      clear_screen_ptr = 8'd23;      
      pram[23] = DISP_HORIZ_MODE;      
      pram[24] = SETCOLADR;
      pram[25] = SETPAGEADR;
      pram[26] = CLR_SCREEN;

      // turn on display
      pram[27] = DISP_ON;
      pram[28] = NULL_CMD;

      // all-on mode
      pram[29] = DISP_ALL_ON;
      pram[30] = NULL_CMD;

      // normal mode
      pram[31] = DISP_RAM;
      pram[32] = NULL_CMD;

      // load bitmap A
      ld_data_a_ptr=8'd33;      
      pram[33] = DISP_HORIZ_MODE;      
      pram[34] = SETCOLADR;
      pram[35] = SETPAGEADR;
      pram[36] = LD_DATA_A;      
      pram[37] = NULL_CMD;

      
      // load bitmap B
      ld_data_b_ptr=8'd38;
      pram[38] = DISP_HORIZ_MODE;      
      pram[39] = SETCOLADR;
      pram[40] = SETPAGEADR;
      pram[41] = LD_DATA_B;
      pram[42] = NULL_CMD;

      // load bitmap C
      ld_data_c_ptr=8'd43;
      pram[43] = DISP_HORIZ_MODE;      
      pram[44] = SETCOLADR;
      pram[45] = SETPAGEADR;
      pram[46] = LD_DATA_C;
      pram[47] = NULL_CMD;

      // load bitmap D
      ld_data_d_ptr=8'd48;
      pram[48] = DISP_HORIZ_MODE;      
      pram[49] = SETCOLADR;
      pram[50] = SETPAGEADR;
      pram[51] = LD_DATA_D;
      pram[52] = NULL_CMD;

      // put char '*' at upper left
      putchar_ptr=8'd53;
      pram[53] = SETCHARROW;
      pram[54] = 0;
      pram[55] = SETCHARCOL;
      pram[56] = 0;
      pram[57] = SETCHAR;
      pram[58] = 8'd42;
      pram[59] = SENDCHAR;
      pram[60] = NULL_CMD;
            
      intr = 0;
      i_adr = 33;
      
   end // initial begin



   always @(*) begin
      pram[54] = {6'd0,char_row}; 
      pram[56] = {4'd0,char_col}; 
      pram[58] = charval;   
   end

   localparam INIT = 0;
   localparam WAIT = 1;
   localparam DONE = 2;
   
   reg [4:0] state;
   

   always @(posedge clk, negedge rst_n) begin
      if (!rst_n) begin
	 // Reset behavior:
	 state <= INIT;
	 ready <= 0;	 
	 intr  <= 0;
	 i_adr <= 28;
      end
      else begin
	 case (state)
	   INIT: begin
	      ready <= 0;
	      intr  <= 0;
	      
	      // Check for end of powerup sequence:
	      if (p_adr > 27) begin
		 state <= WAIT;
		 
	      end
	   end
	   WAIT: begin
	      if (!intr && ready && showchar) begin
		 ready <= 0;
		 intr  <= 1;
		 i_adr <= putchar_ptr;
		 
		 state <= DONE;		 
	      end
	      else if (!intr && ready && clear) begin
		 ready <= 0;
		 intr <= 1;
		 i_adr <= clear_screen_ptr;
		 
		 state <= DONE;		 
	      end
	      else if (!intr && ready && showbmp) begin
		 ready <= 0;
		 intr <= 1;
		 case (bmp)
		   0:  i_adr <= ld_data_a_ptr;
		   1:  i_adr <= ld_data_b_ptr;
		   2:  i_adr <= ld_data_c_ptr;
		   3:  i_adr <= ld_data_d_ptr;
		 endcase // case (bmp)		 		
		 state <= DONE;		 
	      end // if (ready && showbmp)
	      else if (intr_ready) begin
		 intr <= 0;		 
		 ready <= 1;
	      end
	      else begin
		 intr <= 0;
		 ready <= 0;
	      end		
	   end // case: WAIT
	   DONE: begin
	      intr <= 0;	      
	      if (intr_ready)
		state <= WAIT;	      
	   end
	 endcase // case (state)	 
      end
   end
   
endmodule
