
function [8*20-1:0] command_name;
   input [7:0] cmdcode;

   begin
      case (cmdcode)
	NULL_CMD: command_name="NULL_CMD";
	
	VDD_ON : command_name="VDD_ON ";
	VDD_OFF: command_name="VDD_OFF";
	VBAT_ON: command_name="VBAT_ON";
	VBAT_OFF: command_name="VBAT_OFF=";
	RES_ON : command_name="RES_ON ";
	RES_OFF: command_name="RES_OFF";

	LD_DATA_A: command_name="LD_DATA_A";
	LD_DATA_B: command_name="LD_DATA_B";
	LD_DATA_C: command_name="LD_DATA_C";
	LD_DATA_D: command_name="LD_DATA_D";

	CLR_SCREEN: command_name="CLR_SCREEN";
	FILL_SCREEN: command_name="FILL_SCREEN";

	DELAY_LONG   : command_name="DELAY_LONG   ";
	DELAY_SHORT  : command_name="DELAY_SHORT  ";
	DISP_ALL_ON  : command_name="DISP_ALL_ON  ";
	DISP_RAM     : command_name="DISP_RAM     ";
	DISP_ON      : command_name="DISP_ON      ";
	DISP_OFF     : command_name="DISP_OFF     ";
	DISP_CONTRAST: command_name="DISP_CONTRAST";
	DISP_HORIZ_MODE: command_name="DISP_HORIZ_MODE";
	CHARGEPUMP_EN  : command_name="CHARGEPUMP_EN  ";
	MUXRATIO       : command_name="MUXRATIO       ";
	DISP_OFFSET    : command_name="DISP_OFFSET    ";
	STARTLINE      : command_name="STARTLINE      ";
	SEGMENT        : command_name="SEGMENT        ";
	SCANDIR        : command_name="SCANDIR        ";
	COMPINS        : command_name="COMPINS        ";
	DISP_NORMAL    : command_name="DISP_NORMAL    ";
	SETOSC         : command_name="SETOSC         ";
	SETCOLADR      : command_name="SETCOLADR      ";
	SETPAGEADR     : command_name="SETPAGEADR     ";
	default: command_name="NO CMD";
	
      endcase // case (code)
   end
endfunction // command_name
