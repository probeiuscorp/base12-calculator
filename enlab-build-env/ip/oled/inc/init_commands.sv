		SEGMENT: begin
		   numbytes <= 1;
		   cmd[0]   <= SEGMENT;
		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		SCANDIR: begin
		   numbytes <= 1;
		   cmd[0]   <= SCANDIR;
		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		DISP_CONTRAST: begin
		   numbytes <= 2;
		   cmd[0]   <= DISP_CONTRAST;
		   state    <= SET_DATA_BYTE;
		   p_adr++;
		   
		end
		CHARGEPUMP_EN: begin
		   numbytes <= 2;
		   cmd[0]   <= CHARGEPUMP_EN;
		   cmd[1]   <= 8'h14;		   		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		MUXRATIO: begin
		   numbytes <= 2;
		   cmd[0]   <= MUXRATIO;
		   cmd[1]   <= 8'h3F;		   		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		COMPINS: begin
		   numbytes <= 2;
		   cmd[0]   <= COMPINS;
		   cmd[1]   <= 8'h02;		   		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		SETOSC: begin
		   numbytes <= 2;
		   cmd[0]   <= SETOSC;
		   cmd[1]   <= 8'h80;		   		   
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
