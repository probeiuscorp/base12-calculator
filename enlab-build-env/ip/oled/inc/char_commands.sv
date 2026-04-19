		SETCHARROW: begin
		   numbytes <= 3;
		   cmd[0] <= SETPAGEADR;
		   p_adr++;
		   
		   state <= CHAR_ROW_BYTE;		   
		end
		SETCHARCOL: begin
		   numbytes <= 3;
		   cmd[0] <= SETCOLADR;
		   p_adr++;
		   
		   state <= CHAR_COL_BYTE;		   
		end
		SETCHAR: begin
		   numbytes <= 0;
		   p_adr++;
		   
		   state <= CHAR_DATA_BYTE;		   
		end
		SENDCHAR: begin
		   if (dptr < 8) begin
		      numbytes <= 1;
		      cmd[0]   <= font[dptr+((8'd15-fontcol)<<3) + ((8'd3-fontrow)<<7)];		      
		      dc       <= 1;
		      
		      if (rdy) begin
			 send  <= 1;
			 state <= SPI_SEND;
		      end
		   end
		   else begin
		      dc      <= 0;		      
		      dptr    <= 0;
		      state   <= CMD_DONE;
		   end
		end
