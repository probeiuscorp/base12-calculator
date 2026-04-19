`timescale 1ns/1ps

module bitmap (

	 );

   reg [7:0] d[1023:0];

   integer   i;
   
   initial begin
      $readmemb("3700_welcome_mem",d);
      for (i=0;i<1023;i++) begin
	 if ((i % 128) == 0)
	   $write("\n");
	 $write("%2h ",d[i]);
	 
      end 
   end
   
endmodule
