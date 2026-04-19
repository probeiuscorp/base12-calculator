`timescale 1ns/1ps

module spi_listener 
  (
   input sclk,
   input sdo,
   input cs_n,
   input dc
   );

   parameter N=8;
   
   reg [N-1:0] data;
   reg [7:0]   idx;

   initial idx=N-1;
   
   always @(posedge sclk, posedge cs_n) begin
      if (cs_n) begin
	 // Reset behavior:
	 idx=N-1;	 
      end
      else begin
	 // Normal behavior:
	 data[idx] <= sdo;
	 if (idx > 0) begin
	    idx--;
	 end
	 else begin
	    idx <= N-1;
	    
	    if (!dc)
	      $strobe("%t   SPI listener received cmd  %h", $time,data);	    
	    else
	      $strobe("%t   SPI listener received data %h", $time,data);	    
	     
	 end	 
      end
   end
   
endmodule
