#include "listhdr.h"
#include "comphdr.h"
#include "redhdr.h"
#include "emas.h"
#include "revision"
#ifdef LINENOISE
# include "linenoise.h"
#endif

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

//#include <ctype.h>	// for toupper()
#include <setjmp.h>
#include <string.h>	// for strcmp()
#include <unistd.h>	// for fork(), stat()
#include <sys/types.h>	// for sys/wait.h, stat()
#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>

// Local function declarations
STATIC VOID DIRCOM(), DISPLAYCOM(), QUITCOM(), OBJECTCOM();
STATIC VOID RESETCOM(), GCCOM(), COUNTCOM(), SAVECOM(), FILECOM(), GETCOM();
STATIC VOID LISTCOM(), NAMESCOM(), LIBCOM(), CLEARCOM(), OPENLIBCOM();
STATIC VOID HELPCOM(), RENAMECOM(), ABORDERCOM(), REORDERCOM(), DELETECOM();
STATIC BOOL STARTDISPLAYCOM();

STATIC VOID PARSELINE(char *line);
STATIC VOID INITIALISE();
STATIC VOID ENTERARGV(int USERARGC, LIST USERARGV);
STATIC VOID SETUP_COMMANDS();
STATIC VOID COMMAND();
STATIC VOID DISPLAYALL(BOOL DOUBLESPACING);
STATIC BOOL MAKESURE();
STATIC VOID FILENAME();
STATIC BOOL OKFILE(FILE *STR, char *FILENAME);
STATIC VOID CHECK_HITS();
STATIC BOOL GETFILE(char *FILENAME);
STATIC VOID FIND_UNDEFS();
STATIC BOOL ISDEFINED(ATOM X);
STATIC VOID SCRIPTLIST(LIST S);
STATIC LIST SUBST(LIST Z,LIST A);
STATIC VOID NEWEQUATION();
STATIC VOID CLEARMEMORY();
STATIC VOID COMMENT();
STATIC VOID EVALUATION();
STATIC LIST SORT(LIST X);
STATIC VOID SCRIPTREORDER();
STATIC WORD NO_OF_EQNS(ATOM A);
STATIC BOOL PROTECTED(ATOM A);
STATIC BOOL PRIMITIVE(ATOM A);
STATIC VOID REMOVE(ATOM A);
STATIC LIST EXTRACT(ATOM A, ATOM B);

STATIC LIST COMMANDS=NIL, SCRIPT=NIL, OUTFILES=NIL;	//BASES
STATIC ATOM LASTFILE=0;					//BASES

STATIC LIST LIBSCRIPT=NIL, HOLDSCRIPT=NIL, GET_HITS=NIL; //BASES
STATIC BOOL SIGNOFF=FALSE, SAVED=TRUE, EVALUATING=FALSE;
STATIC BOOL ATOBJECT=FALSE, ATCOUNT=FALSE; //FLAGS USED IN DEBUGGING SYSTEM
STATIC char PARAMV[256];	// FOR CALLING EMAS

// Global variables owned by main.c
WORD LEGACY=FALSE; //set by -z option
LIST FILECOMMANDS = NIL;
BOOL SKIPCOMMENTS;  //SET BY -s OPTION
char *USERLIB=NULL; //SET BY -l OPTION

// Local variables
STATIC BOOL FORMATTING;		// Are we evaluating with '?' ?

STATIC BOOL QUIET = FALSE; 	// Suppress greetings, prompts etc.?
STATIC char *EVALUATE = NULL;	// Expression to execute in batch mode

// INITIALISATION AND STEERING

VOID ESCAPETONEXTCOMMAND();

// Are we ignoring interrupts?
static BOOL INTERRUPTS_ARE_HELD = FALSE;
// Was an interrupt delivered while we were ignoring them?
static BOOL INTERRUPT_OCCURRED = FALSE;

STATIC VOID
CATCHINTERRUPT(int signum)
{  IF INTERRUPTS_ARE_HELD DO {
      INTERRUPT_OCCURRED = signum;	// Can't be 0
      RETURN
   }
   FIXUP_S();	   //IN CASE INTERRUPT STRUCK WHILE REDUCE
                   //WAS DISSECTING A CONSTANT
   _WRCH=TRUEWRCH;
   CLOSECHANNELS();
   UNLESS QUIET || ABORTED  // die quietly if running as script or ABORT() called
   DO //WRITES("\n**break in - return to KRC command level**\n");
      WRITES("<interrupt>\n");
   ABORTED=FALSE;
   ESCAPETONEXTCOMMAND();  }


VOID
HOLD_INTERRUPTS() {  INTERRUPTS_ARE_HELD = TRUE;  }

VOID
RELEASE_INTERRUPTS()
   {  INTERRUPTS_ARE_HELD = FALSE;
      IF INTERRUPT_OCCURRED DO {
         INTERRUPT_OCCURRED=FALSE;
         CATCHINTERRUPT(INTERRUPT_OCCURRED);
   }  }

         //ESSENTIAL THAT DEFINITIONS OF THE ABOVE SHOULD BE PROVIDED IF
         //THE PACKAGE IS TO BE USED IN AN INTERACTIVE PROGRAM

// Where to jump back to on runtime errors or keyboard interrupts
static jmp_buf nextcommand;

VOID ESCAPETONEXTCOMMAND()
   {  _WRCH=TRUEWRCH;
      IF INPUT()!=SYSIN DO {  ENDREAD() ; SELECTINPUT(SYSIN); }
      CLOSECHANNELS();
      IF EVALUATING
      DO {  IF ATCOUNT DO OUTSTATS();
	    CLEARMEMORY(); //IN CASE SOME POINTERS HAVE BEEN LEFT REVERSED
	    EVALUATING=FALSE;  }
      IF HOLDSCRIPT!=NIL 
      DO {  SCRIPT=HOLDSCRIPT, HOLDSCRIPT=NIL;
	    CHECK_HITS(); }
      INIT_CODEV();
      INIT_ARGSPACE();
      longjmp(nextcommand, 1);  }

// Buffer for signal handling
static struct sigaction act;	// All initialised to 0/NULL is fine.

VOID
GO()
   {  // STACKLIMIT:= @V4 + 30000  //IMPLEMENTATION DEPENDENT,TO TEST FOR RUNAWAY RECURSION
      IF setjmp(nextcommand) == 0 DO
      {  // First-time initialization
	 INIT_CODEV();
	 INIT_ARGSPACE();
	 INITIALISE();
	 // Set up the interrupt handler
         act.sa_handler = CATCHINTERRUPT;
	 act.sa_flags = SA_NODEFER; // Bcos the interrupt handler never returns
         sigaction(SIGINT, &act, NULL);
      } else {
	 // When the GC is called from CONS() from the depths of an
	 // evaluation, it is more likely that stale pointers left in
	 // registers, either still in them or saved on the stack,
	 // will cause now-unused areas of the heap to be preserved.
	 // We mitigate this by calling the GC here, after an interrupt
	 // or an out-of-space condition, when the stack is shallow and
	 // the registers are less likely to contain values pointing
	 // inside the CONS space.
	 BOOL HOLDATGC=ATGC; ATGC=FALSE;
	 FORCE_GC();
	 ATGC=HOLDATGC;
      }
      // Both initially and on longjump, continue here.
      IF EVALUATE && !SIGNOFF DO {
	 SIGNOFF=TRUE;  // Quit on errors or interrupts
	 PARSELINE(EVALUATE);
	 TEST EXPFLAG THEN EVALUATION(); OR
	   WRITES("-e takes an expression followed by ? or !\n");
	 IF ERRORFLAG
         DO SYNTAX_ERROR("malformed expression after -e\n");
      }
      UNTIL SIGNOFF DO COMMAND();
      QUITCOM();
//    FINISH //moved inside QUITCOM()
   }


// PARSELINE: A version of READLINE that gets its input from a string

static char *input_line;

// Alternative version of RDCH that gets its chars from a string
static int
str_RDCH(void)
{
   IF input_line==NULL DO RESULTIS EOF;
   IF *input_line=='\0' DO {  input_line=NULL;
				RESULTIS '\n';  }
   RESULTIS *input_line++;
}

static int
str_UNRDCH(int c)
{
   TEST input_line==NULL && c=='\n'
   THEN input_line="\n";
   OR *(--input_line)=c;
   RESULTIS c;
}

// SAME AS READLINE, BUT GETS ITS INPUT FROM A C STRING
STATIC VOID
PARSELINE(char *line)
{  input_line=line;
   _RDCH=str_RDCH, _UNRDCH=str_UNRDCH;
   READLINE();
   _RDCH=bcpl_RDCH, _UNRDCH=bcpl_UNRDCH;
}

// ----- END OF PARSELINE

STATIC char TITLE[] = "Kent Recursive Calculator 1.0";

// Where to look for "prelude" and other files KRC needs
#ifndef LIBDIR
#define LIBDIR "/usr/lib/krc"
#endif
//but use krclib in current directory if present, see below

STATIC VOID
INITIALISE()
   {  BOOL LOADPRELUDE=TRUE;	// Do we need to read the prelude?
      BOOL OLDLIB=FALSE;        // Use legacy prelude?
      char *USERSCRIPT=NULL;	// Script given on command line
      LIST USERARGV=NIL;        // Reversed list of args after script name
      int  USERARGC=0;	        // How many items in USERARGV?
//    BOOL  LISTSCRIPT=FALSE;	// List the script as we read it?
      int  I;

      IF !isatty(0) DO QUIET=TRUE;

      SETUP_PRIMFNS_ETC();
      FOR (I=1; I<ARGC; I++) {
         TEST ARGV[I][0]=='-' THEN
            SWITCHON ARGV[I][1] INTO {
	    CASE 'n': LOADPRELUDE=FALSE;
		      ENDCASE
            CASE 's': SKIPCOMMENTS=TRUE;
                      ENDCASE
            CASE 'c': ATCOUNT=TRUE; ENDCASE
            CASE 'o': ATOBJECT=TRUE; ENDCASE
            CASE 'd':		// Handled in listpack.c
            CASE 'l':		// Handled in listpack.c
            CASE 'h': ++I;	// Handled in listpack.c
            CASE 'g':		// Handled in listpack.c
		      ENDCASE
            CASE 'e': IF ++I>=ARGC || ARGV[I][0] == '-'
		      DO {  WRITES("krc: -e What?\n"); FINISH  }
		      IF EVALUATE
		      DO {  WRITES("krc: Only one -e flag allowed\n"); FINISH  }
		      EVALUATE=ARGV[I];
                      QUIET=TRUE;
                      ENDCASE
            case 'z': LISTBASE=1;
                      LEGACY=TRUE;
                      WRITES("LISTBASE=1\n");
                      ENDCASE
            case 'L': OLDLIB=1; ENDCASE
//          case 'v': LISTSCRIPT=TRUE; ENDCASE
            // Other parameters may be detected using HAVEPARAM()
            case 'C': case 'N': case 'O': //used only by testcomp, disabled
	    DEFAULT:  WRITEF("krc: invalid option -%c\n",ARGV[I][1]);
                      FINISH
		      ENDCASE
         } OR {
	    // Filename of script to load, or arguments for script
	    IF USERSCRIPT==NULL DO USERSCRIPT=ARGV[I]; //was TEST...OR
	    USERARGV=CONS((LIST)MKATOM(ARGV[I]), USERARGV), USERARGC++;
      }  }
      TEST EVALUATE THEN ENTERARGV(USERARGC, USERARGV);
      OR IF USERARGC>1 DO { WRITES("krc: too many arguments\n"); FINISH }
      TEST LOADPRELUDE THEN
           TEST USERLIB THEN GETFILE(USERLIB); //-l option was used
           OR { struct stat buf;
                TEST stat("krclib",&buf)==0
                THEN GETFILE(OLDLIB?"krclib/lib1981":"krclib/prelude");
                OR GETFILE(OLDLIB?LIBDIR "/lib1981":LIBDIR "/prelude"); }
      OR // TEST USERLIB || OLDLIB THEN
         // { WRITES("krc: invalid combination -n and -l or -L\n"); FINISH } OR
         WRITES("\"PRELUDE\" suppressed\n");
      SKIPCOMMENTS=FALSE;  //effective only for prelude
      LIBSCRIPT=SORT(SCRIPT),SCRIPT=NIL;
      IF USERSCRIPT DO {
//      IF LISTSCRIPT DO _RDCH=echo_RDCH;
	GETFILE(USERSCRIPT);
        SAVED=TRUE;
//      IF LISTSCRIPT DO _RDCH=bcpl_RDCH;
	LASTFILE=MKATOM(USERSCRIPT);
      }
      SETUP_COMMANDS();
      RELEASE_INTERRUPTS();
      IF !QUIET DO WRITEF("%s\nrevised %s\n%s\n",TITLE,revision,
//                        "http://krc-lang.org",
                          "/h for help");
   }

// Given the (reverse-order) list of atoms made from command-line arguments
// supplied after the name of the script file, create their an entry in the
// script called "argv" for the krc program to access them.
// We create it as a list of strings (i.e. a list of atoms) for which
// the code for a three-element list of string is:
// ( (0x0.NIL).  :- 0 parameters, no comment
//   ( 0.      :- memo field unset
//     LOAD.(QUOTE."one").LOAD.(QUOTE."two").LOAD.(QUOTE."three").
//     FORMLIST.0x03.STOP.NIL ).
//   NIL )
STATIC VOID
ENTERARGV(int USERARGC, LIST USERARGV)
{  
   ATOM A=MKATOM("argv");
   LIST CODE=CONS((LIST)FORMLIST_C,
		  CONS((LIST)USERARGC,
                       CONS((LIST)STOP_C, NIL)));
   FOR ( ;USERARGV != NIL; USERARGV=TL(USERARGV))
      CODE=CONS((LIST)LOAD_C,
                CONS(CONS((LIST)QUOTE, HD(USERARGV)),CODE));
   VAL(A) = CONS(CONS((LIST)0, NIL),
                 CONS(CONS((LIST)0,CODE),
                      NIL));
   ENTERSCRIPT(A);
}

VOID
SPACE_ERROR(char *MESSAGE)
{  _WRCH=TRUEWRCH;
   CLOSECHANNELS();
   TEST EVALUATING
   THEN {  WRITEF("\n**%s**\n**evaluation abandoned**\n",MESSAGE);
           ESCAPETONEXTCOMMAND();  } OR
   TEST MEMORIES==NIL
   THEN
   {  WRITEF("\n%s - recovery impossible\n", MESSAGE);
      FINISH  }
   OR CLEARMEMORY();  //LET GO OF MEMOS AND TRY TO CARRY ON
}

VOID
BASES(VOID (*F)(LIST *)) {
extern LIST S;	// In reducer.c
      F(&COMMANDS);
      F(&FILECOMMANDS);
      F(&SCRIPT);
      F(&LIBSCRIPT);
      F(&HOLDSCRIPT);
      F(&GET_HITS);
      F((LIST *)&LASTFILE);
      F(&OUTFILES);
      F(&MEMORIES);
      F(&S);
      F(&TOKENS);
      F((LIST *)&THE_ID);
      F(&THE_CONST);
      F(&LASTLHS);
      F(&TRUTH);
      F(&FALSITY);
      F(&INFINITY);
      COMPILER_BASES(F);
      REDUCER_BASES(F);
}

STATIC VOID
SETUP_COMMANDS()
   {
#define F(S,R) { COMMANDS=CONS(CONS((LIST)MKATOM(S),(LIST)R),COMMANDS); }
#define FF(S,R) { FILECOMMANDS=CONS((LIST)MKATOM(S),FILECOMMANDS); F(S,R); }
      F("delete",DELETECOM);
      F("d",DELETECOM); //SYNONYM
      F("reorder",REORDERCOM);
      FF("save",SAVECOM);
      FF("get",GETCOM);
      FF("list",LISTCOM);
      FF("file",FILECOM);
      FF("f",FILECOM);
      F("dir",DIRCOM);
      F("quit",QUITCOM);
      F("q",QUITCOM); //SYNONYM
      F("names",NAMESCOM);
      F("lib",LIBCOM);
      F("aborder",ABORDERCOM);
      F("rename",RENAMECOM);
      F("openlib",OPENLIBCOM);
      F("clear",CLEARCOM);
      F("help",HELPCOM);
      F("h",HELPCOM); //SYNONYM
      F("object",OBJECTCOM);  //THESE LAST COMMANDS ARE FOR USE IN
      F("reset",RESETCOM);    //DEBUGGING THE SYSTEM
      F("gc",GCCOM);
      F("dic",REPORTDIC);
      F("count",COUNTCOM);
      F("lpm",LISTPM);
#undef FF
#undef F
   }

STATIC VOID
DIRCOM()
   {  int status;
      switch (fork()) {
      case 0: execlp("ls", "ls", NULL); break;
      case -1: break;
      default: wait(&status);
   }  }

VOID
CLOSECHANNELS()
   {  IF !EVALUATING && OUTPUT()!=SYSOUT DO ENDWRITE();
      UNTIL OUTFILES==NIL
      DO {  SELECTOUTPUT((FILE *)TL(HD(OUTFILES)));
            IF FORMATTING DO NEWLINE();
            ENDWRITE();
            OUTFILES=TL(OUTFILES); }
      SELECTOUTPUT(SYSOUT);
   }

FILE *
FINDCHANNEL(char *F)
{  LIST P=OUTFILES;
   UNTIL P==NIL || strcmp((char *)HD(HD(P)),F) == 0
   DO P=TL(P);
   TEST P==NIL
   THEN {  FILE *OUT = FINDOUTPUT(F);
           IF OUT != NULL
           DO OUTFILES=CONS(CONS((LIST)F,(LIST)OUT),OUTFILES);
           RESULTIS OUT; }
   OR RESULTIS (FILE *)TL(HD(P));
}

// COMMAND INTERPRETER
// EACH COMMAND IS TERMINATED BY A NEWLINE
// <COMMAND>::= /<EMPTY> |    (DISPLAYS WHOLE SCRIPT)
//              /DELETE <THINGY>* |   
//                  (IF NO <THINGY>'S ARE SPECIFIED IT DELETES WHOLE SCRIPT)
//              /DELETE <NAME> <PART>* |
//              /REORDER <THINGY>* |
//              /REORDER <NAME> <PART>* |
//              /ABORDER |
//              /SAVE "<FILENAME>" |
//              /GET "<FILENAME>" |
//              /LIST "<FILENAME>" |
//              /FILE  |
//              /QUIT  |
//              /NAMES |
//              /OPEN|
//              /CLEAR |
//              /LIB   |
//              <NAME> |     (DISPLAYS EQNS FOR THIS NAME)
//              <NAME> .. <NAME> |    (DISPLAYS A SECTION OF THE SCRIPT)
//              <EXP>? |     (EVALUATE AND PRINT)
//              <EXP>! |     (SAME BUT WITH UNFORMATTED PRINTING)
//              <EQUATION>    (ADD TO SCRIPT)
// <THINGY> ::= <NAME> | <NAME> .. <NAME> | <NAME> ..
// <PART> ::= <INT> | <INT>..<INT> | <INT>..

//STATIC char *HELP[] = { //replaced by HELPCOM() see below
//"/                  Displays the whole script",
//"/delete NAMES      Deletes the named functions. /d deletes everything",
//"/delete NAME PARTS Deletes the numbered equations from function NAME",
//"/reorder NAME NAMES Moves the equations for NAMES after those for NAME",
//"/reorder NAME PARTS Redefines the order of NAME's equations",
//"/aborder           Sorts the script into alphabetical order",
//"/rename FROMs,TOs  Changes the names of one or more functions",
//"/save FILENAME     Saves the script in the named file",
//"/get FILENAME      Adds the contents of a file to the script",
//"/list FILENAME     Displays the contents of a disk file",
//"/file (or /f)      Shows the current default filename",
//"/file FILENAME     Changes the default filename",
//"/dir               List filenames in current directory/folder",
//"/quit (or /q)      Ends this KRC session",
//"/names             Displays the names defined in your script",
//"/openlib           Allows you to modify equations in the prelude/library",
//"/clear             Clears the memo fields for all variables",
//"/lib               Displays the names defined in the prelude/library",
//"NAME               Displays the equations defined for the function NAME",
//"NAME..NAME         Displays a section of the script",
//"EXP?               Evaluates an expression and pretty-print the result",
//"EXP!               The same but with unformatted output",
//"EQUATION           Adds an equation to the script",
//"   NAMES ::= NAME | NAME..NAME | NAME..   PARTS ::= INT | INT..INT | INT..",
//NULL,
//};
//
//STATIC VOID
//SHOWHELP()
//{
//	char **h;
//	for (h=HELP; *h; h++) printf("%s\n", *h);
//}

#define KRCPAGER "less -F -X -P'%F (press q to quit)' "
#define HELPLOCAL KRCPAGER "krclib/help/"
#define HELP KRCPAGER LIBDIR "/help/"
#define BUFLEN 80

STATIC VOID
HELPCOM()
{ struct stat buf;
  char strbuf[BUFLEN+1],*topic;
  int local=stat("krclib",&buf)==0,r;
  TEST HAVE(EOL)
  THEN { TEST local
         THEN r=system(HELPLOCAL "menu");
         OR r=system(HELP "menu");
         RETURN }
  topic = HAVEID()?PRINTNAME(THE_ID):NULL;
  UNLESS topic && HAVE(EOL)
  DO { WRITES("/h What? `/h' for options\n");
       RETURN }
  strncpy(strbuf,local?HELPLOCAL:HELP,BUFLEN);
  strncat(strbuf,topic,BUFLEN-strlen(strbuf));
  r=system(strbuf); }

STATIC VOID
COMMAND()
   {
      static char prompt[]="krc> ";
#ifdef LINENOISE
      char *line=linenoise(QUIET ? "" : prompt);
      if (line && line[0] == '\0') return;      // Otherwise the interpreter exits
      PARSELINE(line);                          // Handles NULL->EOF OK
      IF HAVE(EOL) DO { free(line); RETURN }   //IGNORE BLANK LINES
      if (line) {
         linenoiseHistoryAdd(line);
         free(line);
      }
#else
      IF !QUIET DO PROMPT(prompt); // ON EMAS PROMPTS REMAIN IN EFFECT UNTIL CANCELLED
      READLINE();
      IF HAVE(EOL) DO RETURN //IGNORE BLANK LINES
      SUPPRESSPROMPTS();  // CANCEL PROMPT (IN CASE COMMAND READS DATA)
#endif
      TEST HAVE((TOKEN)EOF)
      THEN SIGNOFF=TRUE; OR
      TEST HAVE((TOKEN)'/')
      THEN TEST HAVE(EOL)
           THEN DISPLAYALL(FALSE); OR
           // TEST HAVE((TOKEN)'@') && HAVE(EOL)
           // THEN LISTPM(); OR  //FOR DEBUGGING THE SYSTEM
           {  LIST P=COMMANDS;
              TEST HAVEID()
              THEN THE_ID=MKATOM(SCASECONV(PRINTNAME(THE_ID)));
                 //ALWAYS ACCEPT COMMANDS IN EITHER CASE
              OR P=NIL;
              UNTIL P==NIL || THE_ID==(ATOM)HD(HD(P)) DO P=TL(P);
              TEST P==NIL
              THEN //SHOWHELP();
                   WRITES("command not recognised\nfor help type /h\n");
              OR ((void (*)())TL(HD(P)))();    // SEE "SETUP_COMMANDS()"
           } OR
      TEST STARTDISPLAYCOM() THEN DISPLAYCOM(); OR
      TEST COMMENTFLAG>0 THEN COMMENT(); OR
      TEST EQNFLAG THEN NEWEQUATION();
      OR EVALUATION();
      IF ERRORFLAG DO SYNTAX_ERROR("**syntax error**\n");
   }

STATIC BOOL
STARTDISPLAYCOM()
{ LIST HOLD=TOKENS;
  WORD  R=HAVEID() && (HAVE(EOL) || HAVE((TOKEN)DOTDOT_SY));
  TOKENS=HOLD;
  RESULTIS R;
}

STATIC VOID
DISPLAYCOM()
{  TEST HAVEID()
   THEN TEST HAVE(EOL)
        THEN DISPLAY(THE_ID,TRUE,FALSE); OR
        TEST HAVE((TOKEN)DOTDOT_SY)
        THEN {  ATOM A = THE_ID; LIST X=NIL;
                ATOM B = HAVE(EOL) ? (ATOM)EOL :	// BUG?
                        HAVEID() && HAVE(EOL) ? THE_ID :
                        0;
                TEST B==0 THEN SYNTAX();
                OR X=EXTRACT(A,B);
                UNTIL X==NIL
                DO {  DISPLAY((ATOM)HD(X),FALSE,FALSE);
                      X=TL(X);  }  } //could insert extra line here between groups
        OR SYNTAX();
   OR SYNTAX();
}

STATIC VOID
DISPLAYALL(BOOL DOUBLESPACING)  // "SCRIPT" IS A LIST OF ALL USER DEFINED
                                // NAMES IN ALPHABETICAL ORDER
   {  LIST P=SCRIPT;
      IF P==NIL DO WRITES("Script=empty\n");
      UNTIL P==NIL DO { UNLESS PRIMITIVE((ATOM)HD(P))
                        //don't display builtin fns (relevant only in /openlib)
                        DO DISPLAY((ATOM)HD(P),FALSE,FALSE);
                        P=TL(P);  
                        IF DOUBLESPACING && P != NIL
                        //extra line between groups
                        DO NEWLINE(); }
   }

STATIC BOOL
PRIMITIVE(ATOM A)
{ IF TL(VAL(A))==NIL DO RESULTIS FALSE; //A has comment but no eqns
  RESULTIS HD(TL(HD(TL(VAL(A)))))==(LIST)CALL_C; }

STATIC VOID
QUITCOM()
   {  IF TOKENS!=NIL DO CHECK(EOL);
      IF ERRORFLAG DO RETURN
      IF MAKESURE()
      DO { WRITES("krc logout\n");
           FINISH  }
   }

STATIC BOOL
MAKESURE()
{  IF SAVED || SCRIPT==NIL DO RESULTIS TRUE;
   WRITES("Are you sure? ");
{  WORD CH=RDCH(), C;
   UNRDCH(CH);
   UNTIL (C=RDCH())=='\n' || C == EOF DO LOOP
   IF CH=='y' || CH=='Y' DO RESULTIS TRUE;
   WRITES("Command ignored\n");
   RESULTIS FALSE;
}  }

STATIC VOID
OBJECTCOM()
{  ATOBJECT=TRUE;  }

STATIC VOID
RESETCOM()
{  ATOBJECT=FALSE,ATCOUNT=FALSE,ATGC=FALSE;  }

STATIC VOID
GCCOM()
   {  ATGC=TRUE;
      FORCE_GC();  }

STATIC VOID
COUNTCOM()
{  ATCOUNT=TRUE;  }

STATIC VOID
SAVECOM()
   {  FILENAME();
      IF ERRORFLAG DO RETURN
      IF SCRIPT==NIL
      DO {  WRITES("Cannot save empty script\n");
            RETURN  }
   {  
      FILE *OUT = FINDOUTPUT("T#SCRIPT");
      SELECTOUTPUT(OUT);
      DISPLAYALL(TRUE);
      ENDWRITE();
      SELECTOUTPUT(SYSOUT);
      // Copy T#SCRIPT back to the save file.
      {  int status;
         switch (fork()) {
         case 0:  execlp("mv", "mv", "T#SCRIPT", PRINTNAME(THE_ID), (char *)0);
         default: wait(&status);
		  if (status == 0) SAVED=TRUE;
		  else /* Drop into... */
         case -1:    WRITES("File saved in T#SCRIPT.\n"); break;
		  break;
}  }  }  }

STATIC VOID
FILENAME()
{  TEST HAVE(EOL)
   THEN TEST LASTFILE==0
        THEN {  WRITES("(No file set)\n") ; SYNTAX();  }
        OR THE_ID=LASTFILE;
   OR TEST HAVEID() && HAVE(EOL)
      THEN LASTFILE=THE_ID;
      OR {  IF HAVECONST() && HAVE(EOL) && !ISNUM(THE_CONST)
            DO WRITES("(Warning - quotation marks no longer expected around filenames in file commands - DT, Nov 81)\n");
            SYNTAX(); }
}

STATIC VOID
FILECOM()
{  TEST HAVE(EOL)
   THEN TEST LASTFILE==0
        THEN WRITES("No files used\n");
        OR WRITEF("File = %s\n",PRINTNAME(LASTFILE));
   OR FILENAME();
}

STATIC BOOL
OKFILE(FILE *STR, char *FILENAME)
{  IF STR!=NULL DO RESULTIS TRUE;
   WRITEF("Cannot open \"%s\"\n",FILENAME);
   RESULTIS FALSE; }

STATIC VOID
GETCOM()
   {  BOOL CLEAN = SCRIPT==NIL;
      FILENAME();
      IF ERRORFLAG DO RETURN
      HOLDSCRIPT=SCRIPT,SCRIPT=NIL,GET_HITS=NIL;
      GETFILE(PRINTNAME(THE_ID));
      CHECK_HITS();
      SCRIPT=APPEND(HOLDSCRIPT,SCRIPT),SAVED=CLEAN,HOLDSCRIPT=NIL;
   }

STATIC VOID
CHECK_HITS()
{  UNLESS GET_HITS==NIL
   DO {  WRITES("Warning - /get has overwritten or modified:\n");
         SCRIPTLIST(REVERSE(GET_HITS));
         GET_HITS=NIL;  }
}

STATIC BOOL
GETFILE(char *FILENAME)
   {  FILE *IN = FINDINPUT(FILENAME);
      UNLESS OKFILE(IN,FILENAME) DO RESULTIS FALSE;
      SELECTINPUT(IN);
   {  int line=0; //to locate line number of error in file
      do{line++;
         READLINE();
	 IF ferror(IN) DO {
	    ERRORFLAG=TRUE;
	    BREAK;
         }
         IF HAVE(EOL) DO LOOP;  
         IF HD(TOKENS)==ENDSTREAMCH
         DO BREAK
         TEST COMMENTFLAG
         THEN { line+=(COMMENTFLAG-1);
                COMMENT(); }
         OR NEWEQUATION();
         IF ERRORFLAG
         DO { SYNTAX_ERROR("**syntax error in file ");
              WRITEF("%s at line %d\n",FILENAME,line); }
      } REPEAT
      ENDREAD();
      SELECTINPUT(SYSIN);
      LASTLHS=NIL;
      RESULTIS TRUE;  }}

STATIC VOID
LISTCOM()
   {  FILENAME();
      IF ERRORFLAG DO RETURN
   {  char *FNAME=PRINTNAME(THE_ID);
      FILE *IN=FINDINPUT(FNAME);
      UNLESS OKFILE(IN,FNAME) DO RETURN
      SELECTINPUT(IN);
   {  WORD CH=RDCH();
      UNTIL CH==EOF
      DO  {  WRCH(CH); CH=RDCH();  }
      ENDREAD();
      SELECTINPUT(SYSIN);
}  }  }

STATIC VOID
NAMESCOM()
   {  CHECK(EOL);
      IF ERRORFLAG DO RETURN
      TEST SCRIPT==NIL
      THEN DISPLAYALL(FALSE);
      OR  {  SCRIPTLIST(SCRIPT); FIND_UNDEFS();  }
   }

STATIC VOID
FIND_UNDEFS()  //SEARCHES THE SCRIPT FOR NAMES USED BUT NOT DEFINED
   {  LIST S=SCRIPT, UNDEFS=NIL;
      UNTIL S==NIL
      DO {  LIST EQNS = TL(VAL((ATOM)HD(S)));
            UNTIL EQNS==NIL
            DO {  LIST CODE = TL(HD(EQNS));
                  WHILE ISCONS(CODE)
                  DO {  LIST A = HD(CODE);
                        IF ISATOM(A) && !ISDEFINED((ATOM)A) && !MEMBER(UNDEFS,A)
                        DO UNDEFS=CONS(A,UNDEFS);
                        CODE=TL(CODE);  }
                  EQNS=TL(EQNS);  }
            S=TL(S);  }
      UNLESS UNDEFS==NIL
      DO {  WRITES("\nNames used but not defined:\n");
            SCRIPTLIST(REVERSE(UNDEFS));  }
   }

STATIC BOOL
ISDEFINED(ATOM X)
{  RESULTIS VAL(X)==NIL||TL(VAL(X))==NIL ? FALSE : TRUE;  }

STATIC VOID
LIBCOM()
   {  CHECK(EOL);
      IF ERRORFLAG DO RETURN
      TEST LIBSCRIPT==NIL
      THEN WRITES("library = empty\n");
      OR SCRIPTLIST(LIBSCRIPT);  }
 
STATIC VOID
CLEARCOM()
   {  CHECK(EOL);
      IF ERRORFLAG DO RETURN
      CLEARMEMORY();  }

STATIC VOID
SCRIPTLIST(LIST S)
   {  WORD COL=0,I=0;
#define LINEWIDTH 68  //THE MINIMUM OF VARIOUS DEVICES
      UNTIL S==NIL
      DO {  char *N = PRINTNAME((ATOM)HD(S));
            IF PRIMITIVE((ATOM)HD(S)) DO {S=TL(S); LOOP}
            COL=COL+strlen(N)+1;
            IF COL>LINEWIDTH
            DO  {  COL=0 ; NEWLINE();  }
            WRITES(N);
            WRCH(' ');
            I=I+1,S=TL(S);  }
      IF COL+6>LINEWIDTH DO NEWLINE();
      WRITEF(" (%" W ")\n",I);
   }

STATIC VOID
OPENLIBCOM()
   {  CHECK(EOL);
      IF ERRORFLAG DO RETURN
      SAVED=SCRIPT==NIL;
      SCRIPT=APPEND(SCRIPT,LIBSCRIPT);
      LIBSCRIPT=NIL;
   }

STATIC VOID
RENAMECOM()
   {  LIST X=NIL,Y=NIL,Z=NIL;
      WHILE HAVEID() DO X=CONS((LIST)THE_ID,X);
      CHECK((TOKEN)',');
      WHILE HAVEID() DO Y=CONS((LIST)THE_ID,Y);
      CHECK(EOL);
      IF ERRORFLAG DO RETURN
      {  //FIRST CHECK LISTS ARE OF SAME LENGTH
         LIST X1=X,Y1=Y;
         UNTIL X1==NIL||Y1==NIL DO Z=CONS(CONS(HD(X1),HD(Y1)),Z),X1=TL(X1),Y1=TL(Y1);
         UNLESS X1==NIL && Y1==NIL && Z!=NIL DO { SYNTAX(); RETURN  }  }
      {  // NOW CHECK LEGALITY OF RENAME
         LIST Z1=Z,POSTDEFS=NIL,DUPS=NIL;
         UNTIL Z1==NIL
         DO {  IF MEMBER(SCRIPT,HD(HD(Z1)))
               DO POSTDEFS=CONS(TL(HD(Z1)),POSTDEFS);
               IF ISDEFINED((ATOM)TL(HD(Z1))) && (!MEMBER(X,TL(HD(Z1))) || !MEMBER(SCRIPT,TL(HD(Z1))) )
               DO POSTDEFS=CONS(TL(HD(Z1)),POSTDEFS);
               Z1=TL(Z1);  }
         UNTIL POSTDEFS==NIL
         DO {  IF MEMBER(TL(POSTDEFS),HD(POSTDEFS)) &&
                 !MEMBER(DUPS,HD(POSTDEFS)) DO DUPS=CONS(HD(POSTDEFS),DUPS);
               POSTDEFS=TL(POSTDEFS); }
         UNLESS DUPS==NIL
         DO {  WRITES("/rename illegal because of conflicting uses of ");
               UNTIL DUPS==NIL
               DO  {  WRITES(PRINTNAME((ATOM)HD(DUPS)));
                      WRCH(' ');
                      DUPS=TL(DUPS);  }
               NEWLINE();
               RETURN  }  }
      HOLD_INTERRUPTS();
      CLEARMEMORY();
    //PREPARE FOR ASSIGNMENT TO VAL FIELDS
   {  LIST X1=X,XVALS=NIL,TARGETS=NIL;
      UNTIL X1==NIL
      DO {  IF MEMBER(SCRIPT,HD(X1))
            DO XVALS=CONS(VAL((ATOM)HD(X1)),XVALS),TARGETS=CONS(HD(Y),TARGETS);
            X1=TL(X1),Y=TL(Y);  }
      //NOW CONVERT ALL OCCURRENCES IN THE SCRIPT
   {  LIST S=SCRIPT;
      UNTIL S==NIL
      DO {  LIST EQNS=TL(VAL((ATOM)HD(S)));
            WORD NARGS=(WORD)HD(HD(VAL((ATOM)HD(S))));
            UNTIL EQNS==NIL
            DO {  LIST CODE=TL(HD(EQNS));
                  IF NARGS>0
                  DO {  LIST LHS=HD(HD(EQNS));
			WORD I;
                        FOR (I=2; I<=NARGS; I++)
                           LHS=HD(LHS);
                        HD(LHS)=SUBST(Z,HD(LHS)); }
                  WHILE ISCONS(CODE)
                  DO HD(CODE)=SUBST(Z,HD(CODE)),CODE=TL(CODE);
                  EQNS=TL(EQNS);  }
            IF MEMBER(X,HD(S)) DO VAL((ATOM)HD(S))=NIL;
            HD(S)=SUBST(Z,HD(S));
            S=TL(S);  }
      //NOW REASSIGN VAL FIELDS
      UNTIL TARGETS==NIL
      DO {  VAL((ATOM)HD(TARGETS))=HD(XVALS);
            TARGETS=TL(TARGETS),XVALS=TL(XVALS);  }
      RELEASE_INTERRUPTS();
   }  }  }

STATIC LIST
SUBST(LIST Z,LIST A)
{  UNTIL Z==NIL
   DO {  IF A==HD(HD(Z))
         DO  {  SAVED=FALSE; RESULTIS TL(HD(Z));  }
         Z=TL(Z); }
   RESULTIS A;  }

STATIC VOID
NEWEQUATION()
   {  WORD EQNO = -1;
      IF HAVENUM()
      DO {  EQNO=100*THE_NUM+THE_DECIMALS;
            CHECK((TOKEN)')');  }
   {  LIST X=EQUATION();
      IF ERRORFLAG DO RETURN
   {  ATOM SUBJECT=(ATOM)HD(X);
      WORD NARGS=(WORD)HD(TL(X));
      LIST EQN=TL(TL(X));
      IF ATOBJECT DO  {  PRINTOB(EQN) ; NEWLINE();  }
      TEST VAL(SUBJECT)==NIL
      THEN {  VAL(SUBJECT)=CONS(CONS((LIST)NARGS,NIL),CONS(EQN,NIL));
              ENTERSCRIPT(SUBJECT);  } OR
      TEST PROTECTED(SUBJECT)
      THEN RETURN  OR
      TEST TL(VAL(SUBJECT))==NIL  //SUBJECT CURRENTLY DEFINED ONLY BY A COMMENT
      THEN {  HD(HD(VAL(SUBJECT)))=(LIST)NARGS;
              TL(VAL(SUBJECT))=CONS(EQN,NIL);  } OR
//    TEST NARGS==0 //SIMPLE DEF SILENTLY OVERWRITING EXISTING EQNS - REMOVED DT 2015
//    THEN {  VAL(SUBJECT)=CONS(CONS(0,TL(HD(VAL(SUBJECT)))),CONS(EQN,NIL));
//            CLEARMEMORY(); } OR
      TEST NARGS!=(WORD)HD(HD(VAL(SUBJECT)))
      THEN {  WRITEF("Wrong no of args for \"%s\"\n",PRINTNAME(SUBJECT));
              WRITES("Equation rejected\n");
              RETURN  } OR
      TEST EQNO==-1  //UNNUMBERED EQN
      THEN {  LIST EQNS=TL(VAL(SUBJECT));
              LIST P=PROFILE(EQN);
              do{IF EQUAL(P,PROFILE(HD(EQNS)))
                 DO {  LIST CODE=TL(HD(EQNS));
                       TEST HD(CODE)==(LIST)LINENO_C //IF OLD EQN HAS LINE NO,
                       THEN {  TL(TL(CODE))=TL(EQN);  //NEW EQN INHERITS
                               HD(HD(EQNS))=HD(EQN);  }
                       OR HD(EQNS)=EQN;
                       CLEARMEMORY();
                       BREAK  }
                 IF TL(EQNS)==NIL
                 DO {  TL(EQNS)=CONS(EQN,NIL);
                       BREAK  }
                 EQNS=TL(EQNS);
              } REPEAT
           } 
      OR {  LIST EQNS = TL(VAL(SUBJECT));  //NUMBERED EQN
            WORD N = 0;
            IF EQNO % 100!=0 || EQNO==0 //IF EQN HAS NON STANDARD LINENO
            DO TL(EQN)=CONS((LIST)LINENO_C,CONS((LIST)EQNO,TL(EQN))); //MARK WITH NO.
            do{N=HD(TL(HD(EQNS)))==(LIST)LINENO_C ? (WORD)HD(TL(TL(HD(EQNS)))) :
                   (N/100+1)*100;
               IF EQNO==N
               DO {  HD(EQNS)=EQN;
                     CLEARMEMORY();
                     BREAK  }
               IF EQNO<N
               DO {  LIST HOLD=HD(EQNS);
                     HD(EQNS)=EQN;
                     TL(EQNS)=CONS(HOLD,TL(EQNS));
                     CLEARMEMORY();
                     BREAK  }
               IF TL(EQNS)==NIL
               DO {  TL(EQNS)=CONS(EQN,NIL);
                     BREAK  }
               EQNS=TL(EQNS);
            } REPEAT
         } 
      SAVED=FALSE;
   }  }  }

STATIC VOID
CLEARMEMORY() //CALLED WHENEVER EQNS ARE DESTROYED,REORDERED OR
                  //INSERTED (OTHER THAN AT THE END OF A DEFINITION)
{  UNTIL MEMORIES==NIL //MEMORIES HOLDS A LIST OF ALL VARS WHOSE MEMO
   DO {  LIST X=VAL((ATOM)HD(MEMORIES));  //FIELDS HAVE BEEN SET
         UNLESS X==NIL DO HD(HD(TL(X)))=0; //UNSET MEMO FIELD
         MEMORIES=TL(MEMORIES);  }  }

VOID
ENTERSCRIPT(ATOM A)    //ENTERS "A" IN THE SCRIPT
{  TEST SCRIPT==NIL
   THEN SCRIPT=CONS((LIST)A,NIL);
   OR {  LIST S=SCRIPT;
         UNTIL TL(S)==NIL
         DO S=TL(S);
         TL(S) = CONS((LIST)A,NIL);  }
}

STATIC VOID
COMMENT()
   {  ATOM SUBJECT=(ATOM)TL(HD(TOKENS));
      LIST COMMENT=HD(TL(TOKENS));
      IF VAL(SUBJECT)==NIL
      DO {  VAL(SUBJECT)=CONS(CONS(0,NIL),NIL);
            ENTERSCRIPT(SUBJECT); }
      IF PROTECTED(SUBJECT) DO RETURN
      TL(HD(VAL(SUBJECT)))=COMMENT;
      IF COMMENT==NIL && TL(VAL(SUBJECT))==NIL
      DO REMOVE(SUBJECT);
      SAVED=FALSE;
   }

STATIC VOID
EVALUATION()
   {  LIST CODE=EXP();
      WORD CH=(WORD)HD(TOKENS);
      LIST E=0;  //STATIC SO INVISIBLE TO GARBAGE COLLECTOR
      UNLESS HAVE((TOKEN)'!') DO CHECK((TOKEN)'?');
      IF ERRORFLAG DO RETURN;
      CHECK(EOL);
      IF ATOBJECT DO {  PRINTOB(CODE) ; NEWLINE();  }
      E=BUILDEXP(CODE);
      IF ATCOUNT DO RESETGCSTATS();
      INITSTATS();
      EVALUATING=TRUE;
      FORMATTING=CH=='?';
      PRINTVAL(E,FORMATTING);
      IF FORMATTING DO NEWLINE();
      CLOSECHANNELS();
      EVALUATING=FALSE;
      IF ATCOUNT DO OUTSTATS();
   }

STATIC VOID
ABORDERCOM()
{  SCRIPT=SORT(SCRIPT),SAVED=FALSE;  }

STATIC LIST
SORT(LIST X)
{  IF X==NIL || TL(X)==NIL DO RESULTIS X;
   {  LIST A=NIL, B=NIL, HOLD=NIL;  //FIRST SPLIT X
      UNTIL X==NIL DO HOLD=A, A=CONS(HD(X),B), B=HOLD, X=TL(X);
      A=SORT(A),B=SORT(B);
      UNTIL A==NIL||B==NIL  //NOW MERGE THE TWO HALVES BACK TOGETHER
      DO TEST ALFA_LS((ATOM)HD(A),(ATOM)HD(B))
	 THEN X=CONS(HD(A),X), A=TL(A);
	 OR   X=CONS(HD(B),X), B=TL(B);
      IF A==NIL DO A=B;
      UNTIL A==NIL DO X=CONS(HD(A),X), A=TL(A);
      RESULTIS REVERSE(X);  }
}

STATIC VOID
REORDERCOM()
{  TEST ISID(HD(TOKENS)) && (ISID(HD(TL(TOKENS))) || HD(TL(TOKENS))==(LIST)DOTDOT_SY)
   THEN SCRIPTREORDER(); OR
   TEST HAVEID() && HD(TOKENS)!=EOL
   THEN {  LIST NOS = NIL;
           WORD MAX = NO_OF_EQNS(THE_ID);
           WHILE HAVENUM()
           DO {  WORD A=THE_NUM;
                 WORD B = HAVE(DOTDOT_SY) ?
                         HAVENUM()? THE_NUM : MAX :  A;
		 WORD I;
                 FOR (I=A; I<=B; I++)
                    IF !MEMBER(NOS,(LIST)I) && 1<=I && I<=MAX
                    DO NOS=CONS((LIST)I,NOS);
                    //NOS OUT OF RANGE ARE SILENTLY IGNORED
              }
           CHECK(EOL);
           IF ERRORFLAG DO RETURN
           IF VAL(THE_ID)==NIL
           DO {  DISPLAY(THE_ID,FALSE,FALSE);
                 RETURN  }
           IF PROTECTED(THE_ID) DO RETURN
           {  WORD I;
	      FOR (I=1; I<= MAX; I++)
              UNLESS MEMBER(NOS,(LIST)I)
              DO NOS=CONS((LIST)I,NOS);
              // ANY EQNS LEFT OUT ARE TACKED ON AT THE END
	   }
           // NOTE THAT "NOS" ARE IN REVERSE ORDER
        {  LIST NEW = NIL;
           LIST EQNS = TL(VAL(THE_ID));
           UNTIL NOS==NIL
           DO {  LIST EQN=ELEM(EQNS,(WORD)HD(NOS));
                 REMOVELINENO(EQN);
                 NEW=CONS(EQN,NEW);
                 NOS=TL(NOS);  }
           //  NOTE THAT THE EQNS IN "NEW" ARE NOW IN THE CORRECT ORDER
           TL(VAL(THE_ID))=NEW;
           DISPLAY(THE_ID,TRUE,FALSE);
           SAVED=FALSE;
           CLEARMEMORY();
        }  } 
   OR SYNTAX();
}

STATIC VOID
SCRIPTREORDER()
   {  LIST R=NIL;
      WHILE HAVEID()
      DO TEST HAVE(DOTDOT_SY)
         THEN {  ATOM A=THE_ID, B=0; LIST X=NIL;
                 TEST HAVEID() THEN B=THE_ID; OR
                 IF HD(TOKENS)==EOL DO B=(ATOM)EOL;
                 TEST B==0 THEN SYNTAX(); OR X=EXTRACT(A,B);
                 IF X==NIL DO SYNTAX();
                 R=SHUNT(X,R);  }
         OR TEST MEMBER(SCRIPT,(LIST)THE_ID)
            THEN R=CONS((LIST)THE_ID,R);
            OR {  WRITEF("\"%s\" not in script\n",PRINTNAME(THE_ID));
                  SYNTAX();  }
      CHECK(EOL);
      IF ERRORFLAG DO RETURN
   {  LIST R1 = NIL;
      UNTIL TL(R)==NIL
      DO {  UNLESS MEMBER(TL(R),HD(R)) DO SCRIPT=SUB1(SCRIPT,(ATOM)HD(R)), R1=CONS(HD(R),R1);
            R=TL(R);  }
      SCRIPT=APPEND(EXTRACT((ATOM)HD(SCRIPT),(ATOM)HD(R)),APPEND(R1,TL(EXTRACT((ATOM)HD(R),(ATOM)EOL))));
      SAVED=FALSE;
   }  }

STATIC WORD
NO_OF_EQNS(ATOM A)
{  RESULTIS VAL(A)==NIL ? 0 : LENGTH(TL(VAL(A)));  }

STATIC BOOL
PROTECTED(ATOM A)
  //LIBRARY FUNCTIONS ARE RECOGNISABLE BY NOT BEING PART OF THE SCRIPT
{  IF MEMBER(SCRIPT,(LIST)A) DO RESULTIS FALSE;
   IF MEMBER(HOLDSCRIPT,(LIST)A)
   DO {  UNLESS MEMBER(GET_HITS,(LIST)A) DO GET_HITS=CONS((LIST)A,GET_HITS);
         RESULTIS FALSE;  }
   WRITEF("\"%s\" is predefined and cannot be altered\n",PRINTNAME(A));
   RESULTIS TRUE;  }

STATIC VOID
REMOVE(ATOM A)   // REMOVES "A" FROM THE SCRIPT
   {  SCRIPT=SUB1(SCRIPT,A);
      VAL(A)=NIL;
   }

STATIC LIST
EXTRACT(ATOM A, ATOM B)         //RETURNS A SEGMENT OF THE SCRIPT
{  LIST S=SCRIPT, X=NIL;
   UNTIL S==NIL || HD(S)==(LIST)A DO S=TL(S);
   UNTIL S==NIL || HD(S)==(LIST)B DO X=CONS(HD(S),X),S=TL(S);
   UNLESS S==NIL DO X=CONS(HD(S),X);
   IF S==NIL && B!=(ATOM)EOL DO X=NIL;
   IF X==NIL DO WRITEF("\"%s..%s\" not in script\n",
                      PRINTNAME(A),B==(ATOM)EOL?"":PRINTNAME(B));
   RESULTIS REVERSE(X);  }

STATIC VOID
DELETECOM()
   {  LIST DLIST = NIL;
      WHILE HAVEID()
      DO TEST HAVE(DOTDOT_SY)
         THEN {  ATOM A=THE_ID, B=(ATOM)EOL;
                 TEST HAVEID()
                 THEN B=THE_ID; OR
                 UNLESS HD(TOKENS)==EOL DO SYNTAX();
                 DLIST=CONS(CONS((LIST)A,(LIST)B),DLIST);  } OR
         {  WORD MAX = NO_OF_EQNS(THE_ID);
            LIST NLIST = NIL;
            WHILE HAVENUM()
            DO {  WORD A = THE_NUM;
                  WORD B = HAVE(DOTDOT_SY) ?
                          HAVENUM()?THE_NUM:MAX : A;
		  WORD I;
                  FOR (I=A; I<=B; I++)
                     NLIST=CONS((LIST)I,NLIST);
               }
            DLIST=CONS(CONS((LIST)THE_ID,NLIST),DLIST);
         }
      CHECK(EOL);
      IF ERRORFLAG DO RETURN
   {  WORD DELS = 0;
      IF DLIST==NIL   //DELETE ALL
      DO {
	 TEST SCRIPT==NIL THEN DISPLAYALL(FALSE); OR
         {  UNLESS MAKESURE() DO RETURN
            UNTIL SCRIPT==NIL
            DO {  DELS=DELS + NO_OF_EQNS((ATOM)HD(SCRIPT));
                  VAL((ATOM)HD(SCRIPT))=NIL;
                  SCRIPT=TL(SCRIPT);  }  }  }
      UNTIL DLIST == NIL
      DO TEST ISATOM(TL(HD(DLIST))) || TL(HD(DLIST))==EOL //"NAME..NAME"
         THEN {  LIST X=EXTRACT((ATOM)HD(HD(DLIST)),(ATOM)TL(HD(DLIST)));
                 DLIST=TL(DLIST);
                 UNTIL X==NIL
                 DO DLIST=CONS(CONS(HD(X),NIL),DLIST), X=TL(X);  } OR
         {  ATOM NAME = (ATOM)HD(HD(DLIST));
            LIST NOS = TL(HD(DLIST));
            LIST NEW = NIL;
            DLIST=TL(DLIST);
            IF VAL(NAME) == NIL
            DO {  DISPLAY(NAME,FALSE,FALSE);
                  LOOP }
            IF PROTECTED(NAME) DO LOOP
            TEST NOS==NIL
            THEN {  DELS=DELS+NO_OF_EQNS(NAME);
                    REMOVE(NAME);
                    LOOP  }
            OR {
		WORD I;
		FOR (I=NO_OF_EQNS(NAME); I>=1; I=I-1)
                  TEST MEMBER(NOS,(LIST)I)
                  THEN DELS=DELS+1;
                  OR {  LIST EQN=ELEM(TL(VAL(NAME)),I);
                        REMOVELINENO(EQN);
                        NEW=CONS(EQN,NEW);  }  }
            TL(VAL(NAME))=NEW;
            IF NEW==NIL &&
               TL(HD(VAL(NAME)))==NIL   //COMMENT FIELD
            DO REMOVE(NAME);  } 
      WRITEF("%" W " equations deleted\n",DELS);
      IF DELS>0 DO {  SAVED=FALSE; CLEARMEMORY();  }
   }  }
