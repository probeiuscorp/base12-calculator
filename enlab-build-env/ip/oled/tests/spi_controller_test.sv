`timescale 1ns/1ps


module spi_controller_test (

	 );


   reg clk;
   reg rst_n;
   
   initial begin
      clk = 0;
      forever #10 clk = ~clk;
   end

   reg [1:0] numbytes;
   reg [7:0] cmd [2:0];
   reg       send;
   wire      rdy;
   wire      sclk;
   wire      sdo;
   wire      cs_n;
   reg 	     dc;
   

   spi_controller #(.N(100))
   DUT
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

   
   spi_listener listener
  (
   .sclk(sclk),
   .sdo(sdo),
   .cs_n(cs_n),
   .dc(dc)
   );

   reg 	     flag;
   
   initial begin
      numbytes = 3;
      cmd[0] = 8'haa;
      cmd[1] = 8'h25;
      cmd[2] = 8'hf9;

      dc = 0;
      rst_n = 0;
      send = 0;
      flag = 0;
   
      $monitor("%t: cs_n=%b",$time,cs_n);
      $monitor("%t: rdy=%b",$time,rdy);
      $monitor("%t: send=%b",$time,send);
      $monitor("%t: state=%d",$time,DUT.state);
      $monitor("%t: SPIstate=%d",$time,DUT.DUT.state);
      $monitor("%t: timer=%d",$time,DUT.t);
      $monitor("%t: spirdy=%b",$time,DUT.spirdy);
      $monitor("%t: spisend=%b",$time,DUT.spisend);
      $monitor("%t: spidata=%b",$time,DUT.spidata);
      
   end

   always @(posedge clk) begin
      rst_n <= 1;
      if (rdy)
	send <= 1;
      else if (send && !rdy) begin
	 flag <= 1;
      end
   end


   integer clk_count;
   initial clk_count = 0;
   always @(posedge clk) begin
      clk_count <= clk_count + 1;
      if (flag && rdy) begin
	 // Things to do at finish
	 $finish;
      end
   end

   
endmodule
