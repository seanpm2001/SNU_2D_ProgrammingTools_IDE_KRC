// LIST PROCESSING PACKAGE (FOR 2960/EMAS)    DAT 23/11/79
// WARNING - MUCH OF THIS CODE IS MACHINE DEPENDENT
#include "listhdr.h"

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

#include <string.h>		// for strlen()
#include <ctype.h>		// for toupper()
#include <unistd.h>		// for sbrk()
#include <sys/time.h>		// for <sys/resource.h>
#include <sys/resource.h>	// for set/getrlimit()
#include <stdlib.h>		// for getenv()
#include <stdio.h>		// for sscanf()

// #define DEBUG_GC 1

#ifndef HEAPSIZE
#define HEAPSIZE 128000
#endif
int SPACE=HEAPSIZE;        //SPACE IS THE NUMBER OF LIST CELLS IN EACH
                           //SEMI-SPACE.  ON 2960/EMAS MUST BE <=128K

static int DICMAX=64000;   //ATOMSPACE is DICMAX/atomsize, see later
static int ATOMSPACE;      //MAX NUMBER OF ATOMS WHICH CAN BE STORED
                           //The actual number of atoms is less
                           //because their names are also stored there
#define    ATOMSIZE 255    //MAX NO OF CHARS IN AN ATOM

// Non-pointer value for the HD of an entry in CONS space,
// indicating that it is an integer, stored in the TL field.
#define FULLWORD (NIL-1)

// Impossible value of pointer or integer used as flag during GC.
#define GONETO ((LIST)(1ULL<<(sizeof(LIST)*8-1))) // just top bit set
 
static LIST CONSBASE, CONSLIMIT, CONSP, OTHERBASE;
static LIST *STACKBASE;
//static struct ATOM *ATOMBASE;
static ATOM ATOMBASE;
static ATOM ATOMP, ATOMLIMIT;
static WORD NOGCS=0, RECLAIMS=0;
BOOL ATGC;
char *USERLIB;

#ifdef INSTRUMENT_KRC_GC
// ARE WE CURRENTLY IN THE GARBAGE COLLECTOR?
BOOL COLLECTING = FALSE;
#endif

static ATOM HASHV[128]; 

static char BUFFER[ATOMSIZE+1]; static WORD BUFP=0;

int ARGC; char **ARGV;	//  Program parameters


// Forward declarations
STATIC WORD HASH(char *S, int LEN);
static void GC(void);
static void COPY(LIST *P);
static void COPYHEADS(void);

void main2();

int
main(int argc, char **argv)
{     int I;
      ARGC=argc; ARGV=argv;

      // Detect when we are run from a #! script on a system that
      // passes all parameters from the #! line as a single one.
      // We get: argv[0]="/path/to/krc
      //         argv[1]="-n -e primes?" 
      //         argv[2]="path/to/script" 
      //         argv[3..]=args passed to the script
      IF argc>1 && argv[1][0]=='-' && strchr(argv[1], ' ') != NULL DO {
	 int nspaces=0; char *cp;
         // Allocate space for new ARGV
	 FOR (cp=argv[1]+1; *cp; cp++) IF *cp==' ' DO nspaces++;
	 // Each space generates one more argument
         ARGV=calloc(argc+nspaces, sizeof(char *));
	 IF ARGV==NULL DO exit(1);

         // Rewrite ARGV splitting up the first arg
	 // If we find "-e ", all the rest is a single expression
         ARGV[0]=argv[0]; ARGC=1;
   	 FOR (cp=argv[1]; *cp; ) {
            // Plant another argument
	    ARGV[ARGC++]=cp;
            // Find end of arg
            IF strncasecmp(cp, "-e ", 3)==0 DO {
	       // After "-e", all the rest is the expr to evaluate
               cp += 2; *cp++='\0'; ARGV[ARGC++]=cp;
	       BREAK;
            }
	    IF strchr(cp, ' ') == NULL DO BREAK; // No more spaces
            cp=strchr(cp, ' '), *cp++ = '\0';
         }
         // Now copy the rest of ARGV: the script name and its args
         FOR (I=2; I<argc; I++) ARGV[ARGC++]=argv[I];
      }

      // Terminal output should be unbuffered
      setvbuf(stdout, NULL, _IONBF, 0);

      // More Unix-ey stuff. The stack's soft limit is set to 8192K
      // on some Linux disributions, which makes your long-running KRC
      // program die pointlessly after the millionth prime number.
      // Avoid this by upping the soft stack limit to the hard maximum.
      {  struct rlimit rlim;
	 if (getrlimit(RLIMIT_STACK, &rlim) == 0) {
	    rlim.rlim_cur = rlim.rlim_max;
	    setrlimit(RLIMIT_STACK, &rlim);
         }
	 // it says that this can also affect stack growth
	 if (getrlimit(RLIMIT_AS, &rlim) == 0) {
	    rlim.rlim_cur = rlim.rlim_max;
	    setrlimit(RLIMIT_AS, &rlim);
         }
      }

      // Handle command line arguments that affect this file
      FOR (I=1; I<ARGC; I++) {
         IF ARGV[I][0]=='-' DO
            SWITCHON ARGV[I][1] INTO {
            CASE 'g': ATGC=TRUE; ENDCASE
            CASE 'h': IF ++I>=ARGC || (SPACE=atoi(ARGV[I]))<=0 DO {
                      WRITES("krc: -h What?\n"); FINISH  }
                      ENDCASE
            CASE 'l': TEST ++I>=ARGC //doesn't logically belong in listpack
                      THEN { WRITES("krc: -l What?\n"); FINISH  }
                      OR USERLIB=ARGV[I];
                      ENDCASE
            CASE 'd': IF ++I>=ARGC || (DICMAX=atoi(ARGV[I]))<=0 DO {
                      WRITES("krc: -d What?\n"); FINISH  }
                      ENDCASE
	    }
      }

// TAKING ADVANTAGE OF THE FACT THAT WE HAVE VIRTUAL MEMORY, WE SET UP
// TWO COPIES OF LIST SPACE IN ORDER TO BE ABLE TO DO GARBAGE COLLECTIO
// BY DOING A GRAPH COPY FROM ONE SPACE TO THE OTHER
      ATOMSPACE=DICMAX/atomsize;
      CONSBASE=(LIST)sbrk(SPACE*sizeof(*CONSBASE));
      if (CONSBASE == (void *)-1) SPACE_ERROR("Not enough memory");
      CONSP=CONSBASE, CONSLIMIT=CONSBASE+SPACE;
      OTHERBASE=(LIST)sbrk(SPACE*sizeof(*CONSBASE));
      if (OTHERBASE == (void *)-1) SPACE_ERROR("Not enough memory");
      ATOMBASE=(ATOM)sbrk(ATOMSPACE*sizeof(*ATOMBASE));
      if (ATOMBASE == (void *)-1) SPACE_ERROR("Not enough memory");
      ATOMP=ATOMBASE; ATOMLIMIT=ATOMBASE+ATOMSPACE;

      main2();
}

// A separate function finds STACKBASE, to avoid inclusion of any
// locals, temporaries and stacked stuff belonging to main().
void main2()
{
      LIST N;  // MARKER TO FIND STACK BASE
      STACKBASE=&N;
      GO();   }    //"GO" IS THE USER'S START ROUTINE

WORD
HAVEPARAM(WORD CH)
{
      WORD I;
      CH = toupper(CH);
      FOR (I=1; I<ARGC; I++)
         IF ARGV[I][0] == '-' && toupper(ARGV[I][1]) == toupper(CH)
	 DO RESULTIS TRUE;
      RESULTIS FALSE;  }

LIST
CONS(LIST X, LIST Y)
{
   IF CONSP>=(CONSLIMIT-1) DO GC();
   HD(CONSP)=X,TL(CONSP)=Y,CONSP=CONSP+1;
   RESULTIS CONSP-1; 
}

#include <setjmp.h>

void GC2(jmp_buf *);
void GC3(jmp_buf *, LIST *STACKEND);

void
GC()
{
   // Put all registers onto the stack so that any pointers into
   // the CONS space will be updated during the GC and put back
   // in the registers when GC3() returns here with longjmp.
   jmp_buf env;
   if (setjmp(env) == 0) GC2(&env);
}

void	// Not static to avoid inlining
GC2(jmp_buf *envp)
{
    // Get the address of the end of the stack
    // including the jmp_buf containing the registers but
    // excluding anything that the real GC() might push onto the stack
    // for its own purposes.
    LIST P;
    GC3(envp, &P);
}

// GARBAGE COLLECTOR - DOES A GRAPH COPY INTO THE OTHER SEMI-SPACE
void	// Not static to avoid inlining
GC3(jmp_buf *envp, LIST *STACKEND)
   {  LIST *P;		// Examine every pointer on the stack
			// P is a pointer to pointer, so incrementing it
			// moved it up by the size of one pointer.
      extern VOID HOLD_INTERRUPTS(), RELEASE_INTERRUPTS(); // In MAIN.c

#ifdef DEBUG_GC
int LASTUSED = 0;
WRITEF("\n<");

#define SHOW(name) do{  \
WRITEF(name":%d",(int)(CONSP-OTHERBASE)-LASTUSED); \
LASTUSED=CONSP-OTHERBASE; \
COPYHEADS(); WRITEF("+%d ",(int)(CONSP-OTHERBASE)-LASTUSED); \
LASTUSED=CONSP-OTHERBASE; }while(0)
#else
#define SHOW(name) do{}while(0)
#endif

#ifdef INSTRUMENT_KRC_GC
COLLECTING = TRUE;
#endif
      HOLD_INTERRUPTS();
      NOGCS = NOGCS+1;
      IF ATGC DO WRITES("<gc called>\n");
      CONSP=OTHERBASE;
      BASES(COPY);    // USER'S STATIC VARIABLES ETC.
SHOW("bases");
      {  WORD I;
         FOR (I=0; I < 128; I++)
         {  ATOM A=HASHV[I];         // VAL FIELDS OF ATOMS
            UNTIL A==0
            DO {  COPY((LIST *)&(VAL(A)));
                  A=LINK(A);  }  }  }
SHOW("atoms");
         
      // Runtime detection of stack growth direction
      TEST STACKBASE < STACKEND
      THEN
         // STACK GROW UPWARDS
         FOR (P=STACKBASE+1; P<STACKEND; P++) {
            IF CONSBASE<=(LIST)*P && (LIST)*P<CONSLIMIT DO {
	       IF ((char *)*P-(char *)CONSBASE)%sizeof(struct LIST)==0
	       DO // AN ALIGNED ADDRESS IN LISTSPACE
	          COPY(P);
	       IF ((char *)*P-(char *)CONSBASE)%sizeof(struct LIST)==sizeof(struct LIST *)
	       DO {
	          // Pointer to a tail cell, which also needs updating
		  *P = (LIST) ((LIST *)*P - 1);
	          COPY(P);
		  *P = (LIST) ((LIST *)*P + 1);
               }
	    }
         }
      OR
	 // STACK GROWS DOWNWARDS
         FOR (P=STACKBASE-1; P>STACKEND; P--) {
            IF CONSBASE<=(LIST)*P && (LIST)*P<CONSLIMIT DO {
	       IF ((char *)*P-(char *)CONSBASE)%sizeof(struct LIST)==0
	       DO // AN ALIGNED ADDRESS IN LISTSPACE
	          COPY(P);
	       IF ((char *)*P-(char *)CONSBASE)%sizeof(struct LIST)==sizeof(struct LIST *)
	       DO {
	          // Pointer to a tail cells, which also needs updating
		  *P = (LIST) ((LIST *)*P - 1);
	          COPY(P);
		  *P = (LIST) ((LIST *)*P + 1);
               }
            }
	    IF P == (LIST *)(envp+1) DO {
SHOW("stack");
#ifdef __GLIBC__
	       // The jmp_buf has 128 bytes to save the signal mask, which
	       // are not set and provide a window onto an area of the
	       // stack which can contain old pointers to now unused parts
	       // of CONSSPACE. Apart from copying old junk pointlessly,
	       // it can makes the interpreter unable to recover from
	       // an out-of-space condition when the junk happens to be
	       // > 90% of the available space.
	       // Here we make P hop over this nasty window to take it to
	       // straight to the machine registers at the start of the
	       // buffer.
	       P = (LIST *)((char *)(&((*envp)->__jmpbuf))
			       +sizeof((*envp)->__jmpbuf));
#endif
            }
         }
SHOW("regs");
#ifdef DEBUG_GC
WRITEF(">\n");
#endif

      COPYHEADS();
      // NOW SWAP SEMI-SPACES
      {  LIST HOLD=CONSBASE;
         CONSBASE=OTHERBASE,CONSLIMIT=OTHERBASE+SPACE,OTHERBASE=HOLD;
      }
      RECLAIMS = RECLAIMS + (CONSLIMIT-CONSP);
#if 0
      IF ATGC DO WRITEF("<%d cells in use>\n",(int)(CONSP-CONSBASE));
#else
      // Don't call printf, as if leaves unaligned pointers into
      // CONS space on the stack.
      IF ATGC DO {
         WRITES("<");
         WRITEN((WORD)(CONSP-CONSBASE));
         WRITES(" cells in use>\n");
      }
#endif
      RELEASE_INTERRUPTS();

      IF CONSP-CONSBASE > (9*SPACE)/10   //ABANDON JOB IF SPACE
      DO SPACE_ERROR("Space exhausted"); //UTILISATION EXCEEDS 90%

#ifdef INSTRUMENT_KRC_GC
COLLECTING = FALSE;
#endif
      longjmp(*envp, 1);
   }

static void
COPY(LIST *P)  // P IS THE ADDRESS OF A LIST FIELD
{
//   DO $( WRITES("COPYING ")
//         PRINTOB(*P)
//         NEWLINE()  $) <>
   WHILE CONSBASE<=*P && *P<CONSLIMIT
   DO {  IF HD(*P)==GONETO
         DO {  *P=TL(*P);
               RETURN }
      {  LIST X=HD(*P);
         LIST Y=TL(*P);
         LIST Z=CONSP;
         HD(*P)=GONETO;
         TL(*P)=Z;
         *P=Z;
         HD(Z)=X, TL(Z)=Y;
         CONSP=CONSP+1;
         IF X==FULLWORD DO RETURN
         P=&(TL(Z));  }  }  }

static void
COPYHEADS()
   {  LIST Z = OTHERBASE;
      UNTIL Z == CONSP
      DO {  COPY(&(HD(Z)));
            Z = Z+1;   }
   }

WORD
ISCONS(LIST X)
#ifdef INSTRUMENT_KRC_GC
{  IF CONSBASE<=X && X<CONSLIMIT DO
   {  IF ((char *)X - (char *)CONSLIMIT) % sizeof(struct LIST) != 0 DO
      { WRITEF("\nMisaligned pointer %p in ISCONS\n", X); RESULTIS FALSE; }
      RESULTIS HD(X)!=FULLWORD;  }
   RESULTIS FALSE;  }
#else
{  RESULTIS CONSBASE<=X && X<CONSLIMIT ? HD(X)!=FULLWORD : FALSE;  }
#endif

WORD
ISATOM(LIST X)
{  RESULTIS ATOMBASE<=(ATOM)X && (ATOM)X<ATOMP;  }

WORD
ISNUM(LIST X)
#ifdef INSTRUMENT_KRC_GC
{  IF CONSBASE<=X && X<CONSLIMIT DO
   {  IF ((char *)X - (char *)CONSLIMIT) % sizeof(struct LIST) != 0 DO
      {  WRITEF("\nMisaligned pointer %p in ISNUM\n", X); RESULTIS FALSE;  }
      RESULTIS HD(X)==FULLWORD;  }
   RESULTIS FALSE;  }
#else
{  RESULTIS CONSBASE<=X&&X<CONSLIMIT ? HD(X)==FULLWORD : FALSE;  }
#endif

LIST
STONUM(WORD N) {RESULTIS CONS(FULLWORD,(LIST)N);} // GCC WARNING EXPECTED

WORD
GETNUM(LIST X) {RESULTIS (WORD)(TL(X));}          // GCC WARNING EXPECTED

ATOM
MKATOM(char *S)              // make an ATOM from a C string
{  RESULTIS MKATOMN(S, strlen(S));  }

ATOM
MKATOMN(char *S, int LEN)    // make an ATOM which might contain NULs
{  ATOM *BUCKET = &(HASHV[HASH(S,LEN)]);
   ATOM *P=BUCKET;
   // N is size of string counted as the number of pointers it occupies
   WORD N;
   UNTIL *P==0 DO    // SEARCH THE APPROPRIATE BUCKET
   {  IF LEN==LEN(*P) && memcmp(S, PRINTNAME(*P), (size_t)LEN) == 0
      DO RESULTIS (ATOM)*P;
      P=&(LINK(*P));  }
   //CREATE NEW ATOM
   // +1 for the BCPL size, +1 for the \0, then round up to element size
   N = (1+LEN+1 + (sizeof(WORD *))-1) / sizeof(WORD *);
   IF (WORD **)ATOMP+OFFSET+N > (WORD **)ATOMLIMIT
   DO {  WRITES("<string space exhausted>\n");
         FINISH }
   *P=ATOMP, LINK(ATOMP)=0, VAL(ATOMP)=NIL;
   NAME(ATOMP)[0]=LEN,
   memcpy(NAME(ATOMP)+1, S, (size_t)LEN),
   NAME(ATOMP)[LEN+1]= '\0';
   ATOMP=(ATOM)((WORD **)ATOMP+OFFSET+N);
   RESULTIS *P;
}

STATIC WORD
HASH(char *S, int LEN)  // TAKES A NAME AND RETURNS A VALUE IN 0..127
{  int H=LEN;
   IF LEN && S[0] DO {
      H=H+S[0]*37; LEN=LEN-1;
      IF LEN && S[1] DO {
         H=H+S[1]; LEN=LEN-1;
         IF LEN && S[2] DO {
            H=H+S[2]; LEN=LEN-1;
            IF LEN && S[3] DO
               H=H+S[3];
   }  }  }

   RESULTIS H&0x7F; }

VOID
BUFCH(WORD CH)
   {  IF BUFP>=ATOMSIZE
      DO { SPACE_ERROR("Atom too big"); }
      BUFFER[BUFP++] = CH; }

ATOM
PACKBUFFER()
{  ATOM RESULT=MKATOMN(BUFFER,BUFP);
   BUFP=0;
   RESULTIS RESULT;  }

// Does string A sort before string B?
BOOL
ALFA_LS(ATOM A, ATOM B)  // A,B ARE ATOMS
{  RESULTIS strcmp(PRINTNAME(A), PRINTNAME(B)) < 0; }

STATIC void
GCSTATS()
{  WRITEF("Cells claimed = %d, no of gc's = %d",
          (int)(RECLAIMS+(CONSP-CONSBASE)/2), (int)NOGCS); }

void
RESETGCSTATS()
{  NOGCS=0, RECLAIMS=-(CONSP-CONSBASE); }

void
FORCE_GC()
   {  RECLAIMS=RECLAIMS-(CONSLIMIT-CONSP);//TO COMPENSATE FOR CALLING
                                          //TOO EARLY
      IF ATGC DO WRITEF("Max cells available = %d\n",SPACE);
      GC();
   }

void
REPORTDIC()
{ WRITEF("string space = %ld bytes",(long)(ATOMSPACE*atomsize));
  WRITEF(", used %ld\n",(long)((ATOMP-ATOMBASE)*atomsize));
}

void
LISTPM()
   {  WORD EMPTY = 0;
      WORD I;
      WRITES("\n LIST POST MORTEM\n");
      GCSTATS();
      WRITEF(", current cells = %d\n",(int)((CONSP-CONSBASE)/2));
      IF BUFP>0
      DO {  WRITES("Buffer: ");
            FOR (I = 0; I<BUFP; I++) { WRCH(BUFFER[I]); }
            NEWLINE();  }
      WRITES("Atom buckets:\n");
      FOR (I=0; I<128; I++)
         TEST HASHV[I] != 0
         THEN {  ATOM P=HASHV[I];
                 WRITEF("%d :\t", (int)I);
                 UNTIL P==0
                 DO {  WRITES(PRINTNAME(P));
                       UNLESS VAL(P)==NIL
                       DO {  WRITES(" = ");
                             PRINTOB(VAL(P));  }
                       P=LINK(P);
                       IF P!=0 DO WRITES("\n\t");
                 }
                 NEWLINE(); }
         OR EMPTY = EMPTY + 1;
      WRITEF("Empty buckets = %d\n", (int)EMPTY);  }

WORD
LENGTH(LIST X)
{  WORD N = 0;
   UNTIL X==NIL
   DO X=TL(X),N=N+1;
   RESULTIS N;  }

WORD
MEMBER(LIST X, LIST A)
{  UNTIL X==NIL || HD(X)==A
   DO X = TL(X);
   RESULTIS X!=NIL;  }

LIST 
APPEND(LIST X, LIST Y) { RESULTIS SHUNT(SHUNT(X,NIL),Y); }

LIST 
REVERSE(LIST X) { RESULTIS SHUNT(X,NIL); }

LIST 
SHUNT(LIST X, LIST Y)
{  UNTIL X==NIL
   DO {  Y=CONS(HD(X),Y);
         X=TL(X);  }
   RESULTIS Y;  }

LIST 
SUB1(LIST X, ATOM A)   //DESTRUCTIVELY REMOVES A FROM X (IF PRESENT)
{   IF X==NIL DO RESULTIS NIL;
    IF HD(X)==(LIST)A DO RESULTIS TL(X);
{  LIST *P=&(TL(X));
   UNTIL (*P==NIL) || HD(*P)==(LIST)A DO P=&(TL(*P));
   UNLESS *P==NIL DO *P=TL(*P);
   RESULTIS X;  }  }

WORD
EQUAL(LIST X, LIST Y)
{ do {
   IF X==Y DO RESULTIS TRUE;
   IF ISNUM(X) && ISNUM(Y)
   DO RESULTIS GETNUM(X)==GETNUM(Y);
   UNLESS ISCONS(X) && ISCONS(Y) && EQUAL(HD(X),HD(Y))
   DO RESULTIS FALSE;
   X=TL(X), Y=TL(Y);
  } while(1);
}

LIST 
ELEM(LIST X, WORD N)
{  UNTIL N==1 DO X=TL(X),N=N-1;
   RESULTIS HD(X);  }

void
PRINTOB(LIST X) //or ATOM
{  TEST X==NIL    THEN WRITES("NIL"); OR
   TEST ISATOM(X) THEN WRITEF("\"%s\"",PRINTNAME((ATOM)X)); OR
   TEST ISNUM(X)  THEN WRITEN(GETNUM(X)); OR
   TEST ISCONS(X)
   THEN {  WRCH('(');
           WHILE ISCONS(X)
           DO {  PRINTOB(HD(X));
                 WRCH('.');
                 X=TL(X);  }
           PRINTOB(X);
           WRCH(')');  }
   OR WRITEF("<%p>", X);
}


#ifdef INSTRUMENT_KRC_GC
// DEBUGGING FUNCTION: ENSURE THAT P IS A VALID POINTER INTO CONS SPACE
// AND BOMB IF NOT.
LIST
ISOKCONS(LIST P)
{
   LIST Q;
   IF COLLECTING DO RESULTIS P;

   TEST CONSBASE<=P && P<CONSLIMIT
   THEN
      // (ONLY EVEN ADDRESSES IN LISTSPACE COUNT)
      TEST ((char *)P - (char *)CONSBASE) % sizeof(struct LIST) == 0
      THEN RESULTIS P;
      OR { WRITEF("\nHD() or TL() called on ODD address %p\n", P); }
   OR { WRITEF("\nHD() or TL() called on %p not in CONS space\n", P); }
   RESULTIS (LIST)0; // Cause segfault in caller
}
#endif
