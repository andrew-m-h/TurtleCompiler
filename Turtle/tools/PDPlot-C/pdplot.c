// PDPlot simulator 
//	James Robert Thomson
//	Uses libgd available from http://www.libgd.org
//

#include "gd.h"
#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <ctype.h>

#define HIGHBYTE 0xFF00
#define LOWBYTE  0x00FF

enum {halt=0x0000,up=0x0A00,down=0x0C00,move=0x0E00,add=0x1000,
      sub=0x1200,neg=0x2200,mul=0x1400,test=0x1600,ret=0x2800,
      loadg=0x0600,loadf=0x0700,storeg=0x0400,storef=0x0500,
      readg=0x0200,readf=0x0300,jsr=0x6800,jmp=0x7000,
      jeq=0x7200,jlt=0x7400,push=0x5600,pop=0x5E00};


int main(int argc, char * argv[]){
  gdImagePtr im;
  int black;
  int white;

  int imagex = 500,imagey=500;

  if(argc < 3) {
    printf("usage: ./pdplot code.p out.png [width px] [height px] < data.d\n");
    exit(0);
  } else if(argc > 3){
    imagex = atoi(argv[3]);
    if(argc > 4)
      imagey = atoi(argv[4]);
  }

  FILE * infile, *outfile;
  infile = fopen(argv[1],"r");
  if(infile == NULL){
    perror("Unable to open input file.");
    exit(1);
  }

  outfile = fopen(argv[2],"wb");
  if(outfile == NULL){
    perror("Unable to open output file.");
    exit(1);
  }

  short int PDPmem[65536]; // Memory
  unsigned short int IP=0,SP,FP,GP; // Set up the internal registers
  unsigned short int x=0,y=0; // Current pen location
  char pen=1,N=0,Z=0; // Whether the pen is up or down, and the test bits

  char c;
  char buffer[8],*ptr;
  int i;

  buffer[7] = 0;

  while(1){
    while(!feof(infile) && isspace(c=fgetc(infile)));
    
    ungetc(c,infile);
    
    ptr = buffer;
    while(!feof(infile) && !isspace(c=fgetc(infile))){
      *ptr++ = c;
      if(ptr == buffer+7){
	printf("Invalid input file\n");
	exit(1);
      }
    }
    *ptr = '\0';

    if(!buffer[0]){
      break;
    }
    PDPmem[IP++]=(short int) strtol(buffer,NULL,10);

    if(feof(infile)){
      break;
    }
  }

  fclose(infile);

  GP = IP-1;
  SP = IP-1;
  FP = IP-1;
  IP = 0;

  im =gdImageCreate(imagex,imagey); // Create the canvas.
  
  white = gdImageColorAllocate(im,255,255,255);
  black = gdImageColorAllocate(im,0,0,0);

  // Main loop begins here
  
  while(1){
    switch(PDPmem[IP] & HIGHBYTE){
    case up:
      pen = 0;
      break;
    case down:
      pen = 1;
      break;
    case move:
      if(pen){
	gdImageLine(im,x,imagey-y-1,PDPmem[SP-1],imagey - PDPmem[SP]-1,black);
      }
      x = PDPmem[SP-1];
      y = PDPmem[SP];
      SP-=2;
      break;
    case add:
      PDPmem[SP-1] += PDPmem[SP];
      SP-=1;
      break;
    case sub:
      PDPmem[SP-1] -= PDPmem[SP];
      SP-=1;
      break;
    case neg:
      PDPmem[SP] *= -1;
      break;
    case mul:
      PDPmem[SP-1] *= (signed short int)PDPmem[SP];
      SP-=1;
      break;
    case test:
      Z = PDPmem[SP] == 0;
      N = PDPmem[SP] < 0;
      break;
    case ret:
      SP=FP-2;
      IP= PDPmem[FP];
      FP = PDPmem[FP-1];
      continue;
    case loadg:
      PDPmem[++SP] = PDPmem[GP+(signed char)(PDPmem[IP] & LOWBYTE)];
      break;
    case loadf:
      PDPmem[++SP] = PDPmem[FP+(signed char)(PDPmem[IP] & LOWBYTE)];
      break;
    case storeg:
      PDPmem[GP+(signed char)(PDPmem[IP] & LOWBYTE)] = PDPmem[SP--];
      break;
    case storef:
      PDPmem[FP+(signed char)(PDPmem[IP] & LOWBYTE)] = PDPmem[SP--];
      break;
    case readg:
      scanf("%hd",PDPmem +GP+(signed char)(PDPmem[IP] & LOWBYTE));
      break;
    case readf:
      scanf("%hd",PDPmem +FP+(signed char)(PDPmem[IP] & LOWBYTE));
      break;
    case jsr:
      PDPmem[++SP] = FP;
      PDPmem[++SP] = IP+2; // Pass this instruction and the jump;
      FP = SP;
      IP = PDPmem[IP+1];
      continue;
    case jmp:
      IP = PDPmem[IP+1];
      continue;
    case jeq:
      if(Z){
	IP = PDPmem[IP+1];
	continue;
      }
      IP++;
      break;
    case jlt:
      if(N){
	IP = PDPmem[IP+1];
	continue;
      }
      IP++;
      break;
    case push:
      PDPmem[++SP]= PDPmem[IP+1];
      IP++;
      break;
    case pop:
      SP -= PDPmem[IP+1];
      IP++;
      break;
    case halt:
      pen = 2;
      break;
    default:
      printf("Invalid instruction: %hd at %hd\n",PDPmem[IP],IP);
      exit(1);
    }
    if(pen == 2){
      break;
    }
    if(IP == 65535){
      printf("Instruction pointer out of bounds\n");
      exit(1);
    }
    IP++;
  }
  
  
  gdImagePng(im,outfile);
  fclose(outfile);

  gdImageDestroy(im);
  return 0;
}
