`timescale 1ns/1ps

module spi_test (

	 );

   reg clk;

   initial begin
      clk = 0;
      forever #5 clk = ~clk;
   end

   reg       rst_n;
   reg       en;  
   reg 	     send;
   
   reg [7:0] data;
   integer   clk_count;
   
   wire      rdy;
   wire      sclk;
   wire      sdo;

   reg [1:0] state;
   
   localparam INIT=0;
   localparam SEND=1;

   initial begin
      data  = $random();
      en    = 0;
      rst_n = 0;
      clk_count = 0;
      state=INIT;
      
      //$monitor("%3d: en=%b\trdy=%b,\tsclk=%b\tsdo=%b\tdata=%2h\tstate=%1d\tidx=",clk_count,en,rdy,sclk,sdo,data,DUT.state,DUT.idx);
      
      
   end

   spi #(.N(8)) DUT
     (
      .clk(clk),
      .rst_n(rst_n),
      .en(en),
      .send(send),
      .data(data),
      .rdy(rdy),
      .sclk(sclk),
      .sdo(sdo)
      );

   wire cs_n = ~en;
   reg 	dc;
   
   spi_listener listener
  (
   .sclk(sclk),
   .sdo(sdo),
   .cs_n(cs_n),
   .dc(dc)
   );

   reg [7:0] cmd_list  [0:5] = '{8'hab,8'h12,8'he0,8'h10,8'hcd,8'h34};
   reg [7:0] data_list [0:3] = '{8'h45,8'h3a,8'h9f,8'he4};
   reg [7:0] cmd_bytes [0:4] = '{8'd1,1,2,1,1};
   reg [7:0] data_bytes[0:4] = '{8'd0,1,0,2,1};

/* Expected sequence:
 ab  (cmd0)
 12  (cmd1)
 45  (data1)
 e0  (cmd2/0)
 10  (cmd2/0)
 cd  (cmd3)
 3a  (data3/0)
 9f  (data3/1)
 34  (cmd4)
 e4  (data4)
  */
   
   integer   num_cmds        = 5;
   integer   cmd_idx;
   integer   data_idx;
   
   integer   num_cmd_bytes;
   integer   num_data_bytes;
   integer   cmd_count;
   
   initial begin
      cmd_idx=0;
      data_idx = 0;
      send = 0;
      dc   = 0;
      
      num_cmd_bytes=0;
      num_data_bytes=0;
      cmd_count = 0;
      
//      $monitor("cmd_count=%1d,cmd_idx=%1d,data_idx=%1d,cmd_bytes=%1d,data_bytes=%1d",cmd_count,cmd_idx,data_idx,cmd_bytes[cmd_count],data_bytes[cmd_count]);
//      $monitor("send=%b\trdy=%b\ten=%b",send,rdy,en);
      
   end
   

   reg sclk_d;

   
   always @(posedge clk) begin

      rst_n  <= 1;
      sclk_d <= sclk;

      if (rst_n)
	en <= 1;

      if (en) begin
      case (state)
	INIT: begin	   
	   if (!send && rdy) begin
	      
	      if (num_cmd_bytes < cmd_bytes[cmd_count]) begin
		 data <= cmd_list[cmd_idx+num_cmd_bytes];
		 num_cmd_bytes++;
		 state <= SEND;	  
		 send  <= 1;
		 dc    <= 0;
		 
	      end
	      else if (num_data_bytes < data_bytes[cmd_count]) begin
		 data <= data_list[data_idx+num_data_bytes];
		 num_data_bytes++;
		 state <= SEND;	  
		 send  <= 1;
	         dc    <= 1;
		 
	      end
	      else begin
		 cmd_idx  += num_cmd_bytes;
		 data_idx += num_data_bytes;
		 cmd_count++;	
		 num_data_bytes <= 0;
		 num_cmd_bytes <= 0;
		 
	      end

	      //display("%3d: sending %2hh(%8b)",clk_count,data,data);
	   end
	end
	SEND: begin
	   //if (sclk && !sclk_d) begin
	      //$display("%3d(%3tns):   sdo %b",clk_count,$time,sdo);	    
	   //end
	   if (send && !rdy) begin
	      send <= 0;
	   end
	   else if (!send && rdy) begin
	     // $display("%3d: Updating data and continuing",clk_count);
	  //    data <= $random();
	      state <= INIT;
	      
	   end
	end
      endcase // case (state)
      end
   end


   always @(posedge clk) begin
      clk_count <= clk_count + 1;
      if (cmd_idx > num_cmds) begin
	 // Things to do at finish
	 $finish;
      end
   end

   
   
endmodule
