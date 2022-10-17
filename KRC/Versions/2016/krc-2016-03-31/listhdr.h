// HEADER FOR LIST  PROCESSING  PACKAGE    DAT 23/11/79

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

#include "bcpl.h"

// Include code to check validity of pointers handed to HD() and TL
// #define INSTRUMENT_KRC_GC

/* An element in LIST space */
typedef struct LIST {
	struct LIST *hd;
	struct LIST *tl;
} *LIST;
// HD can contain:
// - the memory address of another cell in the CONS space
// - the memory address of a cell in the ATOM space
// - improbable pointer values of HD() for special values:
//   FULLWORD (see above) or GONETO (see listpack.c)
//#define NIL ((LIST)0) //causes problems
#define NIL ((LIST)0x40000000) //from oldbcpl/listhdr, may need changing
#ifdef INSTRUMENT_KRC_GC
extern LIST ISOKCONS(LIST);
#define HD(p) (ISOKCONS(p)->hd)
#define TL(p) (ISOKCONS(p)->tl)
#else
#define HD(p) ((p)->hd)
#define TL(p) ((p)->tl)
#endif


/* An element in ATOM space */
typedef struct ATOM {
	struct ATOM *link;
	struct LIST *val;
	char        name[];
} *ATOM;
// link points to the next item in the linked list of values,
//	or has value 0 if it is the end of this list.
// val  points to the item's value in the CONS space.
//      NOTE: VAL is used on items in both ATOM and CONS spaces.
// name is a combined BCPL- and C-like string, i.e. the length in the
//	first byte, then the string itself followed by a nul character.
#define LINK(p) ((p)->link)
#define  VAL(p) ((p)->val)
#define NAME(p) ((p)->name)
#define LEN(p) ((p)->name[0]&0xff)
#define OFFSET 2	// Offset of "name" field in pointer words

#define atomsize (sizeof(struct ATOM)) //unit of allocation for ATOMSPACE

// The C string version of the name
#define PRINTNAME(X) (NAME(X)+1)

WORD	HAVEPARAM(WORD CH);
extern int ARGC;
extern char **ARGV;
         //FOR PICKING UP SYSTEM PARAMETERS PASSED TO PROGRAM

extern BOOL ATGC;


// PACKAGE SPECIFICATIONS:

// "GO" "BASES" "SPACE.ERROR"      MUST BE DEFINED BY THE USER
// ALL THE OTHER FUNCTIONS ETC ARE DEFINED BY THE PACKAGE

// "GO()"   IS THE MAIN ROUTINE OF THE USERS PROGRAM (NECESSARY BECAUSE
// THE PACKAGE HAS ITS OWN  "START" ROUTINE)
// "BASES" IS USED TO INFORM THE PACKAGE WHICH OF THE USERS OFF-STACK
// VARIABLES ARE BASES FOR GARBAGE COLLECTION - IT SHOULD BE DEFINED
// THUS -  "LET BASES(F) BE $( F(@A); F(@B); ... $)" WHERE A, B ETC.
// ARE THE RELEVANT VARIABLES.  SEE NOTE 1 BELOW.
// "SPACE.ERROR()" DEFINES THE ACTION THE USER WISHES TO TAKE WHEN LIST
// SPACE IS EXHAUSTED (E.G. PRINT A MESSAGE AND CALL FINISH)
extern void GO(void);
extern void BASES(void (*f)(LIST *));
extern void SPACE_ERROR(char *MESSAGE);

// "CONS(X,Y)" CREATES A LIST CELL, Z SAY, WITH X AND Y FOR ITS FIELDS
// AND "HD!Z", "TL!Z" GIVE ACCESS TO THE FIELDS
LIST	CONS(LIST X, LIST Y);

// "STONUM(N)" STORES AWAY THE NUMBER N AS A LIST OBJECT AND "GETNUM(X)"
// GETS IT OUT AGAIN.  SEE NOTE 2 BELOW.
LIST 	STONUM(WORD N);
WORD	GETNUM(LIST X);

// "MKATOM(S)" CREATES AN ATOM FROM BCPL STRING S  - ATOMS ARE STORED
// UNIQUELY, MKATOM USES A HASHING ALGORITHM TO ACCOMPLISH THIS 
// EFFICIENTLY.  "PRINTNAME(X)" RECOVERS THE BCPL STRING. THERE IS A LIST
// VALUED FIELD "VAL!A" ASSOCIATED WITH EACH ATOM A, INITIALLY 
// CONTAINING "NIL".
ATOM 	MKATOM(char *s);
ATOM 	MKATOMN(char *s, int len);

// "BUFCH(CH)" PUTS THE CHARACTER CH INTO A BUFFER, "PACKBUFFER()"
// EMPTIES THE BUFFER AND RETURNS AN ATOM FORMED FROM THE CHARACTERS
// WHICH HAD BEEN PLACED IN IT(BY CALLING "MKATOM")
void	BUFCH(WORD CH);
ATOM	PACKBUFFER(void);

// THE FUNCTIONS "ISCONS(X)", "ISATOM(X)", "ISNUM(X)" DISTINGUISH
// THE THREE DIFFERENT KINDS OF CONSTRUCTED LIST OBJECT.
// NOTE THAT THE SPECIAL OBJECT "NIL" IS NEITHER AN ATOM NOR A LIST.
// (SO REMEMBER THAT "NIL" HAS NO PRINTNAME AND NO "VAL" FIELD.)
// THERE IS A FIFTH KIND OF VALUE WHICH CAN BE STORED IN A LIST FIELD
// NAMELY A SMALL INTEGER (WHERE "SMALL" IS AN IMPLEMENTATION DEPENDENT
// ADJECTIVE MEANING SMALL ENOUGH NOT TO BE CONFUSED WITH ONE OF THE
// THREE ABOVE MENTIONED TYPES OF LIST OBJECT - SEE NOTE 3, BELOW).
WORD	ISCONS(LIST X);
WORD	ISATOM(LIST X);
WORD	ISNUM(LIST X);

// "ALFA.LS(A,B)" TESTS ATOMS FOR ALPHABETICAL ORDER
// "LENGTH(X)" GIVES THE LENGTH OF LIST X
// "MEMBER(X,A)" SAYS IF "A" IS = AN ELEMENT OF X
// "APPEND(X,Y)" APPENDS (A COPY OF) LIST X TO THE FRONT OF LIST Y
// "EQUAL(X,Y)" DETERMINES IF LIST OBJECTS X AND Y ARE ISOMORPHIC
// "ELEM(X,N)" RETURNS THE N'TH ELEMENT OF LIST X
// "PRINTOB(X)" PRINTS AN ARBITRARY LIST OBJECT X
// "FORCE.GC()" FORCES A GARBAGE COLLECTION
// "REVERSE(X)" REVERSES THE LIST X
// "SHUNT(X,Y)" APPENDS REVERSE(X) TO THE LIST Y
// "SUB1(X,A)" REMOVES A FROM THE LIST X (DESTRUCTIVELY) IF PRESENT
BOOL	ALFA_LS(ATOM A, ATOM B);
WORD	LENGTH(LIST X);
WORD	MEMBER(LIST X, LIST A);
LIST 	APPEND(LIST X, LIST Y);
WORD	EQUAL(LIST X, LIST Y);
LIST 	ELEM(LIST X, WORD N);
void	PRINTOB(LIST X);
void	RESETGCSTATS(void);
void	FORCE_GC(void);
void	REPORTDIC(void);
void	LISTPM(void);
LIST 	REVERSE(LIST X);
LIST 	SHUNT(LIST X, LIST Y);
LIST 	SUB1(LIST X, ATOM A);

// NOTES FOR 2960/EMAS IMPLEMENTATION AT UKC:

// NOTE 1
// AT GARBAGE COLLECTION TIME THE BCPL STACK IS SEARCHED AND ANY VALUE
// WITHIN THE ADDRESS RANGE OF LIST OBJECTS IS TREATED AS A BASE AND 
// POSSIBLY RELOCATED.  IT IS THEREFORE ESSENTIAL THAT THERE SHOULD BE
// NO INTEGERS ON THE STACK LARGE ENOUGH TO BE CONFUSED WITH A BCPL
// ADDRESS - INTEGERS LESS THAN 8 MEG ARE SAFE.

// NOTE 2
// THE NUMBERS STORED AND RECOVERED BY MKNUM AND GETNUM ARE 32 BIT 
// INTEGERS - TAKE CARE NOT TO LEAVE THEM ON THE STACK.

// NOTE 3
// "SMALL" HERE MEANS LESS THAN 8 MEG

