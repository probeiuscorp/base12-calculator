always @(charval) begin
   case (charval)
     33: begin // !
	fontrow = 2;
	fontcol = 7;	
     end
     34: begin // "
	fontrow = 3;
	fontcol = 6;	
     end
     35: begin // #
	fontrow = 2;
	fontcol = 11;		
     end
     36: begin // $
	fontrow = 2;
	fontcol = 10;	
     end
     37: begin // %
	fontrow = 2;
	fontcol = 13;	
     end
     38: begin // &
	fontrow = 3;
	fontcol = 15;	
     end
     39: begin // '
	fontrow = 3;
	fontcol = 12;		
     end
     40: begin // (
	fontrow = 3;
	fontcol = 0;	
     end
     41: begin // )
	fontrow = 3;
	fontcol = 1;	
     end
     42: begin // *
	fontrow = 2;
	fontcol = 15;	
     end
     43: begin // +
	fontrow = 3;
	fontcol = 4;	
     end
     44: begin // ,
	fontrow = 2;
	fontcol = 4;	
     end
     45: begin // -
	fontrow = 2;
	fontcol = 9;	
     end
     46: begin // .
	fontrow = 2;
	fontcol = 5;	
     end
     47: begin // /
	fontrow = 3;
	fontcol = 13;	
     end
     48: begin // 0
	fontrow = 2;
	fontcol = 3;	
     end
     49: begin // 1
	fontrow = 1;
	fontcol = 10;	
     end
     50: begin // 2
	fontrow = 1;
	fontcol = 11;	
     end
     51: begin // 3
	fontrow = 1;
	fontcol = 12;	
     end
     52: begin // 4
	fontrow = 1;
	fontcol = 13;	
     end
     53: begin // 5
	fontrow = 1;
	fontcol = 14;	
     end
     54: begin // 6
	fontrow = 1;
	fontcol = 15;	
     end
     55: begin // 7
	fontrow = 2;
	fontcol = 0;	
     end
     56: begin // 8
	fontrow = 2;
	fontcol = 1;	
     end
     57: begin // 9
	fontrow = 2;
	fontcol = 2;	
     end
     58: begin // :
	fontrow = 2;
	fontcol = 8;	
     end
     59: begin // ;
	fontrow = 2;
	fontcol = 6;	
     end
     60: begin // <
	fontrow = 3;
	fontcol = 9;	
     end
     61: begin // =
	fontrow = 3;
	fontcol = 5;	
     end
     62: begin // >
	fontrow = 3;
	fontcol = 8;	
     end
     63: begin // ?
	fontrow = 3;
	fontcol = 7;	
     end
     64: begin // @
	fontrow = 2;
	fontcol = 14;	
     end
     65: begin // A
	fontrow = 0;
	fontcol = 0;	
     end
     66: begin // B
	fontrow = 0;
	fontcol = 1;	
     end
     67: begin // C
	fontrow = 0;
	fontcol = 2;	
     end
     68: begin // D
	fontrow = 0;
	fontcol = 3;	
     end
     69: begin // E
	fontrow = 0;
	fontcol = 4;	
     end
     70: begin // F
	fontrow = 0;
	fontcol = 5;	
     end
     71: begin // G
	fontrow = 0;
	fontcol = 6;	
     end
     72: begin // H
	fontrow = 0;
	fontcol = 7;	
     end
     73: begin // I
	fontrow = 0;
	fontcol = 8;	
     end
     74: begin // J
	fontrow = 0;
	fontcol = 9;	
     end
     75: begin // K
	fontrow = 0;
	fontcol = 10;	
     end
     76: begin // L
	fontrow = 0;
	fontcol = 11;	
     end
     77: begin // M
	fontrow = 0;
	fontcol = 12;	
     end
     78: begin // N
	fontrow = 0;
	fontcol = 13;	
     end
     79: begin // O
	fontrow = 0;
	fontcol = 14;	
     end
     80: begin // P
	fontrow = 0;
	fontcol = 15;	
     end
     81: begin // Q
	fontrow = 1;
	fontcol = 0;	
     end
     82: begin // R
	fontrow = 1;
	fontcol = 1;	
     end
     83: begin // S
	fontrow = 1;
	fontcol = 2;	
     end
     84: begin // T
	fontrow = 1;
	fontcol = 3;	
     end
     85: begin // U
	fontrow = 1;
	fontcol = 4;	
     end
     86: begin // V
	fontrow = 1;
	fontcol = 5;	
     end
     87: begin // W
	fontrow = 1;
	fontcol = 6;	
     end
     88: begin // X
	fontrow = 1;
	fontcol = 7;	
     end
     89: begin // Y
	fontrow = 1;
	fontcol = 8;	
     end
     90: begin // Z
	fontrow = 1;
	fontcol = 9;	
     end
     91: begin // [
	fontrow = 3;
	fontcol = 2;	
     end
     92: begin // \
	fontrow = 3;
	fontcol = 14;	
     end
     93: begin // ]
	fontrow = 3;
	fontcol = 3;	
     end
     94: begin // ^
	fontrow = 2;
	fontcol = 12;	
     end
     126: begin // ~
	fontrow = 3;
	fontcol = 11;	
     end
     default: begin
	fontrow = 0;
	fontcol = 0;	
     end
   endcase
end
