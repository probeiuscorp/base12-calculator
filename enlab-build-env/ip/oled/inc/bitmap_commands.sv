		LD_DATA_A: begin
		   if (dptr < 512) begin
		      numbytes <= 1;
		      cmd[0]   <= data_a[dptr];
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
		LD_DATA_B: begin
		   if (dptr < 512) begin
		      numbytes <= 1;
		      cmd[0]   <= data_b[dptr];
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
		LD_DATA_C: begin
		   if (dptr < 512) begin
		      numbytes <= 1;
		      cmd[0]   <= data_c[dptr];
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
		LD_DATA_D: begin
		   if (dptr < 512) begin
		      numbytes <= 1;
		      cmd[0]   <= data_d[dptr];
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
