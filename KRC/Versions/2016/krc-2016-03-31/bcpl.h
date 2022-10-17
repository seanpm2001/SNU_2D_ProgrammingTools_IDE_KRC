// bcpl.h: map BCPL constructs to C equivalents

#ifndef BCPL_H

#include <stdio.h>	// For printf etc
#include <stdlib.h>	// For exit()
#include <limits.h>	// for __WORDSIZE

#if __WORDSIZE==64

// Type for machine words, used for all integer variables.
typedef long long WORD;	// 64-bit value.
// Printf/scanf format to use with WORDs
#define W "lld"

#else

typedef int WORD;	// 32-bit value.
#define W "d"

#endif

// Trap broken compiler versions here, as it is included by everything
// Definitely doesn't work with gcc-4.9.[012]
#if __GNUC__ == 4 && __GNUC_MINOR__ == 9
// && __GNUC_PATCHLEVEL__  < 3
# error "KRC is broken when compiled with GCC 4.9. Earlier GCCs, clang and TinyC work.".
#endif


/* transliterate BCPL into C */
#define STATIC static
typedef void VOID;	// Not BCPL but...
typedef WORD BOOL;	// Not BCPL but...
#define AND /**/
#define BE /**/
#define FALSE 0
#define TRUE 1
#define RESULTIS return
#define RETURN return;
#define TEST if (			// Use: TEST p THEN x; OR y;
#define THEN )
#define OR else
#define FOR for
#define UNLESS if(!(			// Use: UNLESS x DO foo;
#define IF if ((			// Use: IF x DO bar;
#define UNTIL while(!(			// Use: UNTIL x DO qux;
#define WHILE while((			// Use: WHILE x DO baz;
#define DO ))
#define REPEATUNTIL(p) while(!(p));	// Use: do{...}REPEATUNTIL(p);
#define REPEATWHILE(p) while(p);	// Use: do{...}REPEATWHILE(p);
#define REPEAT while(1);		// Use: do{...}REPEAT;
#define BREAK break;
#define LOOP continue;
#define FINISH exit(0);
#define SWITCHON switch(
#define INTO )
#define CASE case
#define DEFAULT default
#define ENDCASE break;
#define GOTO goto

// Implement BCPL I/O system using stdio
extern FILE *bcpl_INPUT_fp;
extern FILE *bcpl_OUTPUT_fp;
#define bcpl_INPUT (bcpl_INPUT_fp ? bcpl_INPUT_fp : stdin)
#define bcpl_OUTPUT (bcpl_OUTPUT_fp ? bcpl_OUTPUT_fp : stdout)
extern FILE *bcpl_FINDINPUT(char *file);
extern FILE *bcpl_FINDOUTPUT(char *file);
extern WORD bcpl_READN(void);
// RDCH/UNRDCH and WRCH need to be redirectable
extern int bcpl_RDCH(void);
extern int echo_RDCH(void);
extern int bcpl_UNRDCH(int c);
extern void bcpl_WRCH(WORD C);
// and these are the variables that people can redirect if they want to
extern int (*_RDCH)(void);
extern int (*_UNRDCH)(int c);
extern void (*_WRCH)(WORD D);
// Output functions must go through WRCH
extern void bcpl_WRITES(char *s);
extern void bcpl_WRITEN(WORD N);

#define INPUT() bcpl_INPUT
#define OUTPUT() bcpl_OUTPUT
#define SYSIN stdin
#define SYSOUT stdout
#define FINDINPUT(file) bcpl_FINDINPUT(file)
#define SELECTINPUT(f) bcpl_INPUT_fp=(f)
#define ENDREAD() { if (bcpl_INPUT_fp != stdin) fclose(bcpl_INPUT_fp); }
#define READN() bcpl_READN()
#define FINDOUTPUT(file) bcpl_FINDOUTPUT(file)
#define SELECTOUTPUT(f) bcpl_OUTPUT_fp=(f)
#define ENDWRITE() { if (bcpl_OUTPUT != stdout) fclose(bcpl_OUTPUT_fp); }
#define NEWLINE() WRCH('\n')
#define WRCH(c) (*_WRCH)(c)
#define WRITEF(fmt,args...) fprintf(bcpl_OUTPUT, fmt, ##args) //GNU-specific
#define WRITEN(n) bcpl_WRITEN(n)
#define WRITES(s) bcpl_WRITES(s)
#define RDCH() (*_RDCH)()
#define UNRDCH(c) (*_UNRDCH)(c)
// We use ENDSTREAMCH as a TOKEN and EOF as the end-of-file character
// #define ENDSTREAMCH EOF

#define BCPL_H
#endif
