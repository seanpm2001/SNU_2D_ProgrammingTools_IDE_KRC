//KRC COMPILER HEADER FILE

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

//LEX ANALYSER MANIFESTS
#define TOKEN LIST	// in-core tokens can be CONSes
#define	IDENT		(TOKEN)0
#define	CONST		(TOKEN)1
#define	EOL		(TOKEN)'\n'
#define	BADTOKEN 	(TOKEN)256
#define	PLUSPLUS_SY	(TOKEN)257
#define	GE_SY		(TOKEN)258
#define	LE_SY		(TOKEN)259
#define	NE_SY		(TOKEN)260
#define EQ_SY           (TOKEN)261
#define DOTDOT_SY       (TOKEN)262
#define BACKARROW_SY    (TOKEN)263
#define DASHDASH_SY     (TOKEN)264
#define STARSTAR_SY     (TOKEN)265
#define ENDSTREAMCH     (TOKEN)EOF //-1

//LEX ANALYSER GLOBALS
extern LIST	TOKENS, THE_CONST;              // BASES
extern ATOM	THE_ID;                         // BASES
extern WORD	THE_NUM, THE_DECIMALS;		// DECIMALS are never used
extern WORD	EXPFLAG, ERRORFLAG, EQNFLAG;
extern TOKEN	MISSEDTOK;
extern WORD CASECONV(WORD CH);
extern WORD	COMMENTFLAG;
// SUPPRESSPROMPTS();  //EMAS COMMAND
extern LIST	FILECOMMANDS;
extern BOOL	LEGACY;
extern void	WRITETOKEN(TOKEN T);

// KRC EXPRESSION REPRESENTATIONS
// THE INTERNAL REPRESENTATIONS OF EXPRESSIONS IS AS FOLLOWS
// EXP::= ID|CONST|APPLN|OTHER
// ID::= ATOM
// CONST::= NUM|CONS(QUOTE,ATOM)|NIL
// APPLN::= CONS(EXP,EXP)
// OTHER::= CONS(OPERATOR,OPERANDS)
// NOTE THAT THE INTERNAL FORM OF ZF EXESSIONS IS CONS(ZF_OP,BODY) :-
//  BODY::= CONS(EXP,NIL) | CONS(QUALIFIER,BODY)
//  QUALIFIER::= EXP | CONS(GENERATOR,CONS(ID,EXP))

// OPERATOR VALUES:
typedef enum {
   // QUASI OPERATORS
   ALPHA=-2, INDIR=-1, QUOTE=0,
   // INFIX OPERATORS
   COLON_OP=1, APPEND_OP=2, LISTDIFF_OP=3, OR_OP=4, AND_OP=5,
      // SUBGROUP: RELATIONAL OPERATORS
      GR_OP=6, GE_OP=7, NE_OP=8, EQ_OP=9, LE_OP=10, LS_OP=11,
   PLUS_OP=12,MINUS_OP=13,TIMES_OP=14,DIV_OP=15,REM_OP=16,
   EXP_OP=17,DOT_OP=18,
   // OTHER OPERATORS
   DOTDOT_OP=19, COMMADOTDOT_OP=20, ZF_OP=21, GENERATOR=22,
   LENGTH_OP=23, NEG_OP=24, NOT_OP=25,
   QUOTE_OP=26    //USED TO CONVERT AN INFIX INTO A FUNCTION 
} OPERATOR;

//INTERNAL REPRESENTATION OF KRC EQUATIONS
//VAL FIELD OF ATOM ::= CONS(CONS(NARGS,COMMENT),LISTOF(EQN))
// COMMENT ::= NIL | CONS(ATOM,COMMENT)
// EQN ::= CONS(LHS,CODE)
//(IF NARGS=0 THERE IS ONLY ONE EQUATION IN THE LIST AND ITS LHS FIELD
// IS USED TO REMEMBER THE VALUE OF THE VARIABLE)
// LHS ::= ID | CONS(LHS,FORMAL)
// FORMAL ::= ID | CONST | CONS(COLON_OP,CONS(FORMAL,FORMAL))
// CODE ::= INSTR*
// INSTR ::= LOAD_C <ID|CONST|MONOP> |
//           LOADARG_C INT |
//           APPLY_C |
//           APPLYINFIX_C DIOP |
//           IF_C |
//           FORMLIST_C INT |
//           MATCH_C INT CONST
//           MATCHARG_C INT INT |
//           MATCHPAIR_C INT |
//           STOP_C |
//           LINENO_C INT |
//           CONTINUE.INFIX_C DIOP |
//           CONT.GENERATOR_C INT|
//           FORMZF_C INT|
//           CALL_C BCPL_FN

//INSTRUCTION CODES
typedef enum {
       LOAD_C=0, LOADARG_C=1, APPLY_C=2, APPLYINFIX_C=3,
       IF_C=4, FORMLIST_C=5, MATCH_C=6, MATCHARG_C=7,
       MATCHPAIR_C=8, STOP_C=9, LINENO_C=10, CALL_C=11,
       CONTINUE_INFIX_C=12, FORMZF_C=13, CONT_GENERATOR_C=14,
       //THE LINENO COMMAND HAS NO EFFECT AT EXECUTION TIME, IT IS USED
       //TO GIVE AN EQUATION A NON STANDARD LINE NUMBER FOR INSERTION 
       //PURPOSES
} INSTRUCTION;

//EXTERNAL SYNTAX FOR KRC EXPRESSIONS AND EQUATIONS
// EQUATION ::= LHS=EXP | LHS=EXP,EXP
// LHS ::= ID FORMAL*
// FORMAL ::= ID | CONST | (PATTERN) | [PATTERN-LIST?]
// PATTERN ::= FORMAL:PATTERN | FORMAL
// EXP ::= PREFIX EXP | EXP INFIX EXP | SIMPLE SIMPLE*
// SIMPLE ::= ID | CONST | (EXP) | [EXP-LIST?] | [EXP..EXP] | [EXP..] |
//            [EXP,EXP..EXP] | [EXP,EXP..] | {EXP;QUALIFIERS}
// QUALIFIERS ::= QUALIFIER | QUALIFIER;QUALIFIERS
// QUALIFIER ::= EXP | NAME-LIST<-EXP
// CONST ::= INT | "STRING"

// PREFIX AND INFIX OPERATORS, IN ORDER OF INCREASING BINDING POWER:

//    (PREFIX)               (INFIX)            (REMARKS)
//                          :  ++  --           right associative
//                             |
//                             &
//      \
//                     >  >=  ==  \=  <=  <     continued relations allowed
//                            +  -              left associative
//      -      
//                          *  /  %             left associative 
//                           **  .              (** is right associative)
//      #
//Notes - "%" is remainder operator, "." is functional composition and "#" takes the length of lists

//COMPILER GLOBALS

// DEFINED IN KRC_LEX
extern void	READLINE(void);
extern BOOL	HAVE(TOKEN);
extern WORD	HAVEID(void);
extern void	SYNTAX(void);
extern void	CHECK(TOKEN);
extern WORD	HAVECONST(void);
extern WORD	HAVENUM(void);
extern void	SYNTAX_ERROR(char *);

// DEFINED IN KRC_COMPILER
extern void INIT_CODEV(void);
extern LIST EQUATION(void);
extern LIST PROFILE(LIST);
extern void PRINTEXP(LIST, WORD);
extern LIST EXP(void);
extern void REMOVELINENO(LIST);
extern WORD ISID(LIST);
extern VOID DISPLAY(ATOM ID, BOOL WITHNOS, BOOL DOUBLESPACING);
extern VOID DISPLAYEQN(ATOM ID, WORD NARGS, LIST EQN);
extern VOID DISPLAYRHS(LIST LHS,WORD NARGS, LIST CODE);

//DEFINED IN KRC_REDUCER
extern void PRINTATOM(ATOM A, WORD FORMAT);

//OTHERS
extern void (*TRUEWRCH)(WORD C);
extern LIST TRUTH, FALSITY, INFINITY, LASTLHS;     //BASES

// GC helpers
extern VOID COMPILER_BASES(VOID (*F)(LIST *));
extern VOID REDUCER_BASES(VOID (*F)(LIST *));
