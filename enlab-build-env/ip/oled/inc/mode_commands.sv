		DISP_OFF: begin
		   numbytes <= 1;
		   cmd[0]   <= DISP_OFF;
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		DISP_ALL_ON: begin
		   numbytes <= 1;
		   cmd[0]   <= DISP_ALL_ON;
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		DISP_ON: begin
		   numbytes <= 1;
		   cmd[0]   <= DISP_ON;
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		DISP_RAM: begin
		   numbytes <= 1;
		   cmd[0]   <= DISP_RAM;
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
		DISP_NORMAL: begin
		   numbytes <= 1;
		   cmd[0]   <= DISP_NORMAL;
		   if (rdy) begin
		      send     <= 1;
		      state <= SPI_SEND;
		   end
		end
