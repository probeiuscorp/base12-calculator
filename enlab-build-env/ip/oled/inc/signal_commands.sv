		DELAY_LONG: begin		   
		   N <= Nlong;		   		   
		   tclr  <= 0;
		   state <= DELAY;
		end
		DELAY_SHORT: begin		   
		   N <= Nshort;		   		   
		   tclr  <= 0;
		   state <= DELAY;
		end
		VDD_ON: begin
		   vdd   <= 1;
		   state <= CMD_DONE;
		end
		VDD_OFF: begin
		   vdd   <= 0;
		   state <= CMD_DONE;
		end
		VBAT_ON: begin
		   vbat  <= 1;
		   state <= CMD_DONE;
		end
		VBAT_OFF: begin
		   vbat  <= 0;
		   state <= CMD_DONE;
		end
		RES_ON: begin
		   res   <= 1;
		   state <= CMD_DONE;
		end
		RES_OFF: begin
		   res   <= 0;
		   state <= CMD_DONE;
		end
