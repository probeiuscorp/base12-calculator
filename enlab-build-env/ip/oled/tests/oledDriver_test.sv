`timescale 1ns/1ns

module oledDriver_test (

	 );

   reg clk;

   initial begin
      clk = 0;
      forever #5 clk = ~clk;
   end

   reg       rst_n;
   reg       en;   
   reg [7:0] data;
   integer   clk_count;
   
   wire      rdy;
   wire      sclk;
   wire      sdo;

   reg [7:0] contrast;

   // OLED bitmap array:
   //reg [63:0] din[127:0];
   reg 	      load;
   wire       ready;
   
   // OLED header signals:
   wire       cs_n;
   wire       dc;
   wire       vbat;
   wire       vdd;
   
   
   reg [1:0] state;
   
   localparam INIT=0;
   localparam SEND=1;

   integer   idx,jdx;
   
   initial begin
      data  = $random();
      en    = 0;
      rst_n = 0;
      clk_count = 0;
      state=INIT;
      contrast = 65;
      load = 0;
      /*
      for (idx=0; idx<64; idx++) begin
	 for (jdx=0; jdx<128; jdx++) begin
	    din[idx][jdx]=0;
	 end
      end
       */
   end
   
   oledDriver DUT 
     (
      .clk(clk),
      .rst_n(rst_n),
      
      // OLED control inputs:
      .en(en),
      .contrast(contrast),
      
      // OLED bitmap array:
      //.din(din),
      .load(load),
      .ready(ready),
      
      // OLED header signals:
      .cs_n(cs_n),
      .sdo(sdo),
      .sclk(sclk),
      .dc(dc),
      .vbat(vbat),
      .vdd(vdd)
      );

   
   spi_listener listener
  (
   .sclk(sclk),
   .sdo(sdo),
   .cs_n(cs_n)
   );

   reg sclk_d;
   
   always @(posedge clk) begin

      rst_n  <= 1;
      if (rst_n)
	en <= 1;
      
      if (ready)
	load <= 1;
      
            
   end

/*
   initial begin
      $display("en  vdd vbat cs  sclk dc  sdo bsy rdy  data state spistate");
      $display("--- --- ---- --- ---- --- --- --- ---- ---- ----- --------");      
      $monitor("%b   %b   %b    %b   %b    %b   %b   %b    %b     %2h    %2h    %1h",en,vdd,vbat,DUT.cs,sclk,dc,sdo,DUT.spi_busy,DUT.spirdy,DUT.spidata,DUT.state,DUT.spistate);
   end
  
   initial begin
      $monitor("%t vdd=%b",$time,vdd);
      $monitor("%t vbat=%b",$time,vbat);      
   end
  */
 
   always @(posedge clk) begin
      clk_count <= clk_count + 1;
      if (DUT.idx==1024) begin
	 // Things to do at finish
	 $finish;
      end
   end

   
   
endmodule
