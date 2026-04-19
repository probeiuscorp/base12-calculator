		SETCOLADR: begin
		   numbytes <= 3;
		   cmd[0] <= SETCOLADR;
		   cmd[1] <= 0;
		   cmd[2] <= 8'd127;
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end		   
		end
		SETPAGEADR: begin
		   numbytes <= 3;
		   cmd[0] <= SETPAGEADR;
		   cmd[1] <= 0;
		   cmd[2] <= 8'd3;
		   if (rdy) begin
		      send  <= 1;
		      state <= SPI_SEND;
		   end		   
		end
		DISP_OFFSET: begin
		   numbytes <= 2;
		   cmd[0]   <= DISP_OFFSET;
		   cmd[1]   <= 8'h00;		   		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		DISP_HORIZ_MODE: begin
		   numbytes <= 2;
		   cmd[0]   <= DISP_HORIZ_MODE;
		   cmd[1]   <= 00;		   		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		CLR_SCREEN: begin
		   if (dptr < 512) begin
		      numbytes <= 1;
		      cmd[0]   <= 8'h00;		      
		      dc       <= 1;
		      
		      if (rdy) begin
			 send  <= 1;
			 state <= SPI_SEND;
		      end
		   end
		   else begin
		      dc    <= 0;		      
		      dptr  <= 0;
		      state <= CMD_DONE;
		   end
		end
		FILL_SCREEN: begin
		   if (dptr < 512) begin
		      numbytes <= 1;
		      cmd[0]   <= 8'hFF;		      
		      dc       <= 1;
		      
		      if (rdy) begin
			 send  <= 1;
			 state <= SPI_SEND;
		      end
		   end
		   else begin
		      dc    <= 0;		      
		      dptr  <= 0;
		      state <= CMD_DONE;
		   end
		end
		STARTLINE: begin
		   numbytes <= 1;
		   cmd[0]   <= STARTLINE;
		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
