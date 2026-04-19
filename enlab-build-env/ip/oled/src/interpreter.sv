`timescale 1ns/1ps
module interpreter 
  (
   input 	    clk,
   input 	    rst_n,
   input 	    en,
   output reg 	    ready,
   
   output reg [7:0] p_adr,
   input [7:0] 	    p_data,

   input 	    intr,
   input [7:0] 	    i_adr,
   
   output reg 	    vbat,
   output reg 	    vdd,
   output reg 	    res,
   output 	    sclk,
   output 	    sdo,
   output 	    cs_n,
   output reg 	    dc
   );


   reg [7:0] state;

   // Long and Short timers:
   localparam Nlong=1000;
   localparam Nshort=400;
   
   
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


   reg 	      send;
   reg [1:0]  numbytes;
   reg [7:0]  cmd [2:0];
   reg [7:0]  charval;   // register for ascii character
   reg [8:0]  fontcol; 
   reg [8:0]  fontrow; 
   reg [8:0]  offset; 
   
   spi_controller //#(.N(10))
   SC1
     (
      .clk(clk),
      .rst_n(rst_n),
      .numbytes(numbytes),
      .cmd(cmd),
      .send(send),
      .rdy(rdy),
      .sclk(sclk),
      .sdo(sdo),
      .cs_n(cs_n)   
      );

   
   // Import command Codes
   `include "ip/oled/inc/command_codes.sv"
     
   // States
   localparam INIT        = 0;
   localparam NEW_CMD     = 1;
   localparam CMD_DONE    = 2;
   localparam DELAY       = 3;
   localparam SPI_SEND       = 4;
   localparam SET_DATA_BYTE  = 5;
   localparam TWO_DATA_BYTES = 6;
   localparam DATA_DONE      = 7;
   localparam CHAR_DATA_BYTE = 8;
   localparam CHAR_COL_BYTE  = 9;
   localparam CHAR_ROW_BYTE  = 10;
   

   reg [7:0]  data_a[511:0];
   reg [7:0]  data_b[511:0];
   reg [7:0]  data_c[511:0];
   reg [7:0]  data_d[511:0];
   reg [7:0]  font[511:0];
   integer    dptr;

   
   initial begin
      state = INIT;
      N     = Nlong;
      tclr  = 1;
      send  = 0;
      
      res   = 1;
      vdd   = 1;
      vbat  = 1;

      charval = 0;
      ready = 0;
      
      dptr = 0;
      $readmemb("ip/oled/bitmaps/usu_logo.mem",data_a);
      $readmemb("ip/oled/bitmaps/3700.mem",data_b);
      $readmemb("ip/oled/bitmaps/utah.mem",data_c);
      $readmemb("ip/oled/bitmaps/usu.mem",data_d);      
      $readmemb("ip/oled/bitmaps/alphabet.mem",font);      
   end


   `include "ip/oled/inc/fontmap.sv"
   
   integer datacount;
   

   always @(posedge clk, negedge rst_n) begin
      if (!rst_n) begin
	 // Reset behavior:
	 state <= INIT;
	 ready <= 0;	 
	 p_adr <= 0;	 
      end
      else begin
	 // Normal behavior:
	 case (state)
	   INIT: begin
	      tclr      <= 1;
	      dc        <= 0;
	      datacount <= 0;
	      ready <= 0;
	      
	      if (en)
		state <= NEW_CMD;	      
	   end
	   NEW_CMD: begin
	      case (p_data)
		NULL_CMD: begin		  
		   if (intr) begin
		      ready <= 0;		      
		      p_adr <= i_adr;		      
		   end
		   else
		     ready <= 1;		   
		end

		// Import command definitions
                `include "ip/oled/inc/signal_commands.sv"
                `include "ip/oled/inc/mode_commands.sv"
                `include "ip/oled/inc/ram_commands.sv"
                `include "ip/oled/inc/init_commands.sv"
                `include "ip/oled/inc/bitmap_commands.sv"
                `include "ip/oled/inc/char_commands.sv"
		
	      endcase // case (p_data)	      
	   end
	   DELAY: begin
	      ready <= 0;

	      if (t)
		state <= CMD_DONE;	      
	   end
	   CMD_DONE: begin	      
	      tclr  <= 1;
	      ready <= 0;
	      state <= NEW_CMD;
	      p_adr++;		 	      
	   end
	   DATA_DONE: begin	      
	      tclr  <= 1;
	      ready <= 0;
	      datacount++;
	      dptr++;
	      
	      state <= NEW_CMD;	      
	   end
	   SET_DATA_BYTE: begin
	      cmd[numbytes-1] <= p_data;
	      ready <= 0;
	      
	      if (rdy) begin
		 send     <= 1;
		 state <= SPI_SEND;
	      end
	   end
	   TWO_DATA_BYTES: begin 
	      cmd[numbytes-2] <= p_data;
	      ready <= 0;
	      p_adr++;	      	      
	      state <= SET_DATA_BYTE;		 	      
	   end
	   CHAR_COL_BYTE: begin
	      cmd[numbytes-2] <= (8'd15-p_data)<<3;
	      cmd[numbytes-1] <= ((8'd15-p_data)<<3) + 7;
	      
	      if (rdy) begin
		 send     <= 1;
		 state <= SPI_SEND;
	      end
	   end
	   CHAR_ROW_BYTE: begin
	      cmd[numbytes-2] <= 8'd3-p_data;
	      cmd[numbytes-1] <= 8'd3-p_data;
	      
	      if (rdy) begin
		 send     <= 1;
		 state <= SPI_SEND;
	      end
	   end
	   CHAR_DATA_BYTE: begin
	      charval <= p_data;
	      state   <= CMD_DONE;	      
	   end
	   SPI_SEND: begin
	      if (!rdy)
		send <= 0;
	      else if (rdy && !send) begin
		 if (!dc)
		   state <= CMD_DONE;
		 else
		   state <= DATA_DONE;		 
	      end
	   end
	   default: begin
	      ready <= 0;
	      p_adr <= 0;
	      state <= INIT;	      
	   end
	 endcase // case (state)
	 
      end
   end
   
endmodule
