#include <stdio.h>
#include <stdlib.h>

void convertFile(char * fname);


int main(int argc, char * argv[]) {
  if (argc < 2) {
    printf("Usage: %s <filename>", argv[0]);
  }
  else {
    convertFile(argv[1]);
  }
}


void convertFile(char * fname) {
  FILE * fid=fopen(fname,"r");
  char ascii[128][32];
  char bitmap[128][4][8];
  size_t bufsize = 130*sizeof(char);
  char *buffer = (char *) malloc(bufsize);
  int i,j,pg,row,col;

  for (i=0; i<32; i++) {
    int numchar=getline(&buffer,&bufsize,fid);
    //printf("%d\n",numchar);
    int len=128;
    if (numchar < 128) {
      len = numchar-1;
      for (j=len; j<128; j++)
	ascii[j][i] = '0';
    }
    for (j=0; j<len; j++) {
      if (buffer[j] == '*')
	ascii[j][i] = '1';
      else
	ascii[j][i] = '0';
    }    
  }
  fclose(fid);

  /*
  printf("==============ASCII===========================================\n");
  for (i=0; i<32; i++) {
    for (j=0; j<128; j++) {
      printf("%c",ascii[j][i]);
    }
    printf("\n");
  }
  printf("==============MEM===========================================\n");
  */

  for (pg=0; pg<4; pg=pg+1) {
    for (col=0; col<128; col = col+1) {
      for (row=0;row<8;row=row+1) {
	if (ascii[col][pg*8+row]=='0') {
	  bitmap[col][pg][row] = '0';
	}
	else if (ascii[col][pg*8+row]=='1'){
	  bitmap[col][pg][row] = '1';
	}
	else {
	  printf("%d,%d,%d:%c\n",col,pg,row,ascii[col][pg*8+row]);
	}
      }
    }
  }


  // Print out binary formatted memory contents
  for (pg=0; pg<4; pg++) {
    for (col = 0; col<128; col++) {
      for (row = 0; row<8; row++) {
	printf("%c",bitmap[127-col][3-pg][row]);
      }
      printf("\n");
    }
  }
}
