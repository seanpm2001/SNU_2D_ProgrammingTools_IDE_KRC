// Helper functions for bcpl.h

#include "bcpl.h"
#include "emas.h"	// for TERMINATOR

#include <ctype.h>	// for isdigit()
#include <string.h>	// for strcmp()

// Which file descriptors should BCPL's input and output use?
// We use (FILE *)0 as a synonym for stdin / stdout since
// we can't initialise statically because stdin and stdout
//  are not constants on some systems.
FILE *bcpl_INPUT_fp=(FILE *)0;
FILE *bcpl_OUTPUT_fp=(FILE *)0;

FILE *
bcpl_FINDINPUT(char *file)
{
	IF strcmp(file, ".IN") == 0 DO file="/dev/stdin";
	RESULTIS fopen(file, "r");
}

FILE *
bcpl_FINDOUTPUT(char *file)
{
	IF strcmp(file, ".OUT") == 0 DO file="/dev/stdout";
	IF strcmp(file, ".ERR") == 0 DO file="/dev/stderr";
	RESULTIS fopen(file, "w");
}

// EMAS's READN() gobbles up the following character
// and deposits it in a global variable TERMINATOR. So we do the same.

// KRC only needs positive numbers, and an initial digit is guaranteed,
WORD bcpl_READN()
{  WORD D = 0;
   int CH = RDCH();
   WHILE isdigit(CH) DO {
      D=D*10+(CH-'0');
      CH=RDCH();
   }
   TERMINATOR=CH;
   RESULTIS D;
}

// The character that has been UNRDCH-ed. -1 means none are pending.
static int UNREADCH=-1;

// The standard function for RDCH(c)
int bcpl_RDCH()
{
   IF UNREADCH>=0 DO {
      int CH=UNREADCH;
      UNREADCH=-1;
      RESULTIS CH;
   }
   RESULTIS getc(bcpl_INPUT);
}

// A version of RDCH that echoes what it reads
int echo_RDCH()
{
   int CH;
   IF UNREADCH>=0 DO {
      CH=UNREADCH;
      UNREADCH=-1;
      RESULTIS CH;
   }
   CH=getc(bcpl_INPUT);
   WRCH(CH);
   RESULTIS CH;
}

int bcpl_UNRDCH(int c)
{
        return(UNREADCH=c & 0xff);
}

// The standard function for WRCH(c)
void bcpl_WRCH(WORD C)
{
	putc(C, bcpl_OUTPUT);
}

// _RDCH and _WRCH are the function pointers used to perform
// RDCH() and WRCH() and may be modified to attain special effects.
// Normally in BCPL you would say "WRCH=WHATEVER"
// but in C, WRCH and WRCH() would conflict so
// say _WRCH=WHATEVER to change it,

int (*_RDCH)() = bcpl_RDCH;
int (*_UNRDCH)(int) = bcpl_UNRDCH;
void (*_WRCH)(WORD C) = bcpl_WRCH;

// Other output functions must go through WRCH so that
// callers may redirect it to some other function.
void
bcpl_WRITES(char *s) { while (*s) WRCH(*s++); }

// Helper function writes positive integers
static void
bcpl_WRITEP(WORD n) {
      if (n/10) bcpl_WRITEP(n/10);
      WRCH(n%10 + '0');
}

void
bcpl_WRITEN(WORD n) {
   if (n<0) { WRCH('-'); n=-n; }
   bcpl_WRITEP(n);
}
