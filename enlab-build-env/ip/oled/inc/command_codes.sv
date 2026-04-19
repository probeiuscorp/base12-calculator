// Parameter declarations for all interpreter command codes:

   localparam NULL_CMD = 8'h00;
   
   localparam VDD_ON  = 8'h30;
   localparam VDD_OFF = 8'h31;
   localparam VBAT_ON = 8'h32;
   localparam VBAT_OFF= 8'h33;
   localparam RES_ON  = 8'h34;
   localparam RES_OFF = 8'h35;

   localparam LD_DATA_A = 8'h36;
   localparam LD_DATA_B = 8'h37;
   localparam LD_DATA_C = 8'h38;
   localparam LD_DATA_D = 8'h39;

   localparam CLR_SCREEN = 8'h3A;
   localparam FILL_SCREEN = 8'h3B;

   localparam SETCHARROW = 8'h70;
   localparam SETCHARCOL = 8'h71;
   localparam SETCHAR    = 8'h72;
   localparam SENDCHAR   = 8'h73;

   localparam DELAY_LONG    = 8'h0F;
   localparam DELAY_SHORT   = 8'h0E;
   localparam DISP_ALL_ON   = 8'hA5;
   localparam DISP_RAM      = 8'hA4;
   localparam DISP_ON       = 8'hAF;
   localparam DISP_OFF      = 8'hAE;
   localparam DISP_CONTRAST = 8'h81;
   localparam DISP_HORIZ_MODE = 8'h20;
   localparam CHARGEPUMP_EN   = 8'h8D;
   localparam MUXRATIO        = 8'hA8;
   localparam DISP_OFFSET     = 8'hD3;
   localparam STARTLINE       = 8'h40;
   localparam SEGMENT         = 8'hA0;
   localparam SCANDIR         = 8'hC0;
   localparam COMPINS         = 8'hDA;
   localparam DISP_NORMAL     = 8'hA6;
   localparam SETOSC          = 8'hD5;
   localparam SETCOLADR       = 8'h21;
   localparam SETPAGEADR      = 8'h22;
