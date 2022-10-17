//KRC COMPILER

// Note: What is now '{' here was '{ ' in the BCPL.

#include "bcpl.h"
#include "listhdr.h"
#include "comphdr.h"

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

// Local function declarations
STATIC BOOL ISOP(LIST X);
STATIC BOOL ISINFIX(LIST X);
STATIC BOOL ISRELOP(LIST X);
STATIC WORD DIPRIO(OPERATOR OP);
STATIC OPERATOR MKINFIX(TOKEN T);
STATIC VOID PRINTZF_EXP(LIST X);
STATIC BOOL ISLISTEXP(LIST E);
STATIC BOOL ISRELATION(LIST X);
STATIC BOOL ISRELATION_BEGINNING(LIST A,LIST X);
STATIC WORD LEFTPREC(OPERATOR OP);
STATIC WORD RIGHTPREC(OPERATOR OP);
STATIC BOOL ROTATE(LIST E);
STATIC BOOL PARMY(LIST X);
STATIC LIST REST(LIST C);
STATIC LIST SUBTRACT(LIST X, LIST Y);
STATIC VOID EXPR(WORD N);
STATIC BOOL STARTFORMAL(TOKEN T);
STATIC BOOL STARTSIMPLE(TOKEN T);
STATIC VOID COMBN(VOID);
STATIC VOID SIMPLE(VOID);
STATIC VOID COMPILENAME(ATOM N);
STATIC WORD QUALIFIER(VOID);
STATIC VOID PERFORM_ALPHA_CONVERSIONS();
STATIC BOOL ISGENERATOR(LIST T);
STATIC VOID ALPHA_CONVERT(LIST VAR, LIST P);
STATIC LIST SKIPCHUNK(LIST P);
STATIC VOID CONV1(LIST T, LIST VAR, LIST VAR1);
STATIC LIST FORMAL(VOID);
STATIC LIST INTERNALISE(LIST VAL);
STATIC LIST PATTERN(VOID);
STATIC VOID COMPILELHS(LIST LHS, WORD NARGS);
STATIC VOID COMPILEFORMAL(LIST X, WORD I);
STATIC VOID PLANT0(INSTRUCTION OP);
STATIC VOID PLANT1(INSTRUCTION OP, LIST A);
STATIC VOID PLANT2(INSTRUCTION OP, LIST A, LIST B);
STATIC LIST COLLECTCODE(VOID);


// Global variables
void (*TRUEWRCH)(WORD C) = bcpl_WRCH;
LIST LASTLHS=NIL;
LIST TRUTH, FALSITY, INFINITY;


// SETUP_INFIXES() - Interesting elements start at [1]
// The indices correspond to the OPERATOR values in comphdr.h
STATIC TOKEN INFIXNAMEVEC[] = {
	(TOKEN)0,
	(TOKEN) ':',
	PLUSPLUS_SY,
	DASHDASH_SY,
	(TOKEN) '|',
	(TOKEN) '&',
	(TOKEN) '>',
	GE_SY,
	NE_SY,
        EQ_SY, //WAS (TOKEN) '=', CHANGED DT MAY 2015
	LE_SY,
	(TOKEN) '<',
	(TOKEN) '+',
	(TOKEN) '-',
	(TOKEN) '*',
	(TOKEN) '/',
	(TOKEN) '%',
	STARSTAR_SY,
	(TOKEN)	'.',
};
STATIC WORD INFIXPRIOVEC[] = { 0, 0,0,0,1,2,3,3,3,3,3,3,4,4,5,5,5,6,6 };

        // BASES FOR GARBAGE COLLECTION
STATIC LIST CODEV = NIL;// store for opcodes and ther params, which
			// may be operators, various CONStructs or the
			// addresses of C functions.
STATIC LIST ENV[100];   // Appears to be a store for formal parameters
STATIC WORD ENVP;

VOID
INIT_CODEV() {
   ENVP=-1;
   CODEV=NIL;
}


STATIC BOOL ISOP(LIST X) { RESULTIS X==(LIST)ALPHA || X==(LIST)INDIR ||
                              ((LIST)QUOTE<=X && X<=(LIST)QUOTE_OP);  }

STATIC BOOL ISINFIX(LIST X) { RESULTIS (LIST)COLON_OP<=X && X<=(LIST)DOT_OP; }

STATIC BOOL ISRELOP(LIST X) { RESULTIS (LIST)GR_OP<=X && X<=(LIST)LS_OP; }

// Return the priority of an operator from its index in INFIX*
STATIC WORD DIPRIO(OPERATOR OP)
{  RESULTIS OP==-1 ? -1 : INFIXPRIOVEC[OP];  }

STATIC OPERATOR
MKINFIX(TOKEN T)// TAKES A TOKEN , RETURNS AN OPERATOR
                                // OR -1 IF T NOT THE NAME OF AN INFIX
{  WORD I=1;
   IF T==(TOKEN)'=' DO RESULTIS EQ_OP; //legacy, accept "=" for "=="
   UNTIL I>DOT_OP || INFIXNAMEVEC[I]==T DO I=I+1;
   IF I>DOT_OP DO RESULTIS -1;
   RESULTIS I;   }

VOID
PRINTEXP(LIST E, WORD N)    // N IS THE PRIORITY LEVEL
{  TEST E==NIL
   THEN WRITES("[]"); OR
   TEST ISATOM(E)
   THEN WRITES(PRINTNAME((ATOM)E)); OR
   TEST ISNUM(E)
   THEN { WORD X=GETNUM(E);
          TEST X<0 && N>5 
          THEN { WRCH('('); WRITEN(X); WRCH(')'); }
          OR WRITEN(X); }
   OR {  UNLESS ISCONS(E)
         DO {  TEST E==(LIST)NOT_OP THEN WRITES("'\\'"); OR
               TEST E==(LIST)LENGTH_OP THEN WRITES("'#'");
               OR WRITEF("<internal value:%p>",E);
               RETURN }
      {  LIST OP=HD(E);		// Maybe could be OPERATOR
         TEST !ISOP(OP) && N<=7
         THEN {  PRINTEXP(OP,7);
                 WRCH(' ');
                 PRINTEXP(TL(E),8);  }  OR
         TEST OP==(LIST)QUOTE
         THEN { PRINTATOM((ATOM)TL(E),TRUE); } OR
         TEST OP==(LIST)INDIR || OP==(LIST)ALPHA
         THEN PRINTEXP(TL(E),N); OR
         TEST OP==(LIST)DOTDOT_OP || OP==(LIST)COMMADOTDOT_OP
         THEN {  WRCH('[');
                 E=TL(E);
                 PRINTEXP(HD(E),0);
                 IF OP==(LIST)COMMADOTDOT_OP
                 DO {  WRCH(',');
                       E=TL(E);
                       PRINTEXP(HD(E),0);  }
                 WRITES("..");
                 UNLESS TL(E)==INFINITY DO PRINTEXP(TL(E),0);
                 WRCH(']');  } OR
         TEST OP==(LIST)ZF_OP
         THEN {  WRCH('{');
                 PRINTZF_EXP(TL(E));
                 WRCH('}');  } OR
         TEST OP==(LIST)NOT_OP && N<=3
         THEN {  WRCH('\\');
                 PRINTEXP(TL(E),3); } OR
         TEST OP==(LIST)NEG_OP && N<=5
         THEN {  WRCH('-');
                 PRINTEXP(TL(E),5);  } OR
         TEST OP==(LIST)LENGTH_OP && N<=7
         THEN {  WRCH('#');
                 PRINTEXP(TL(E),7);  } OR
         TEST OP==(LIST)QUOTE_OP
         THEN {  WRCH('\'');
		 TEST TL(E)==(LIST)LENGTH_OP THEN WRCH('#'); OR
		 TEST TL(E)==(LIST)NOT_OP THEN WRCH('\\'); OR
		 WRITETOKEN(INFIXNAMEVEC[(WORD)TL(E)]);
		 WRCH('\''); }  OR
         TEST ISLISTEXP(E)
         THEN {  WRCH('[');
                 UNTIL E==NIL
                 DO {  PRINTEXP(HD(TL(E)),0);
                       UNLESS TL(TL(E))==NIL DO WRCH(',');
                       E=TL(TL(E));  }
                 WRCH(']');  } OR
         TEST OP==(LIST)AND_OP && N<=3 && ROTATE(E) && ISRELATION(HD(TL(E)))
	      && ISRELATION_BEGINNING(TL(TL(HD(TL(E)))),TL(TL(E)))
         THEN {  //CONTINUED RELATIONS
                 PRINTEXP(HD(TL(HD(TL(E)))),4);
                 WRCH(' ');
                 WRITETOKEN(INFIXNAMEVEC[(WORD)HD(HD(TL(E)))]);
                 WRCH(' ');
                 PRINTEXP(TL(TL(E)),2);  } OR
         TEST ISINFIX(OP) && INFIXPRIOVEC[(WORD)OP]>=N
         THEN {  PRINTEXP(HD(TL(E)),LEFTPREC((OPERATOR)OP));
                 UNLESS OP==(LIST)COLON_OP DO WRCH(' '); //DOT.OP should be spaced, DT 2015
                 WRITETOKEN(INFIXNAMEVEC[(WORD)OP]);
                 UNLESS OP==(LIST)COLON_OP DO WRCH(' ');
                 PRINTEXP(TL(TL(E)),RIGHTPREC((OPERATOR)OP));  }
          OR {  WRCH('(');
                PRINTEXP(E,0);
                WRCH(')');   }
   }  }  }

STATIC VOID
PRINTZF_EXP(LIST X)
{  LIST Y=X;
   UNTIL TL(Y)==NIL DO Y=TL(Y);
   PRINTEXP(HD(Y),0);  //BODY
// PRINT "SUCH THAT" AS BAR IF A GENERATOR DIRECTLY FOLLOWS
   TEST ISCONS(HD(X)) && HD(HD(X))==(LIST)GENERATOR THEN WRCH('|'); OR WRCH(';');
   UNTIL TL(X)==NIL
   DO {  LIST QUALIFIER=HD(X);
         TEST ISCONS(QUALIFIER) && HD(QUALIFIER)==(LIST)GENERATOR
         THEN {  PRINTEXP(HD(TL(QUALIFIER)),0);
                 WHILE ISCONS(TL(X)) && //DEALS WITH REPEATED GENERATORS
#ifdef INSTRUMENT_KRC_GC
		       ISCONS(HD(TL(X))) &&
#endif
                       HD(HD(TL(X)))==(LIST)GENERATOR &&
                       EQUAL(TL(TL(HD(TL(X)))),TL(TL(QUALIFIER)))
                 DO {  X=TL(X);
                       QUALIFIER=HD(X);
                       WRCH(',');
                       PRINTEXP(HD(TL(QUALIFIER)),0); }
                 WRITES("<-");
                 PRINTEXP(TL(TL(QUALIFIER)),0);  }
         OR PRINTEXP(QUALIFIER,0);
         X=TL(X);
         UNLESS TL(X)==NIL DO WRCH(';');  }
}

STATIC BOOL
ISLISTEXP(LIST E)
{  WHILE ISCONS(E) && HD(E)==(LIST)COLON_OP
   DO {  LIST E1=TL(TL(E));
         WHILE ISCONS(E1) && HD(E1)==(LIST)INDIR
         DO E1=TL(E1);
         TL(TL(E))=E1;
         E=E1;  }
   RESULTIS E==NIL;   }

STATIC BOOL
ISRELATION(LIST X) { RESULTIS ISCONS(X) && ISRELOP(HD(X)); }

STATIC BOOL
ISRELATION_BEGINNING(LIST A,LIST X)
{   RESULTIS (ISRELATION(X) && EQUAL(HD(TL(X)),A)) ||
             (ISCONS(X) && HD(X)==(LIST)AND_OP &&
             ISRELATION_BEGINNING(A,HD(TL(X))));   }

STATIC WORD
LEFTPREC(OPERATOR OP)
{    RESULTIS OP==COLON_OP||OP==APPEND_OP||OP==LISTDIFF_OP||
              OP==AND_OP||OP==OR_OP||OP==EXP_OP||ISRELOP((LIST)OP) ?
             INFIXPRIOVEC[OP] + 1 : INFIXPRIOVEC[OP];  }

        // RELOPS ARE NON-ASSOCIATIVE
        // COLON, APPEND, AND, OR ARE RIGHT-ASSOCIATIVE
        // ALL OTHER INFIXES ARE LEFT-ASSOCIATIVE

STATIC WORD
RIGHTPREC(OPERATOR OP)
{      RESULTIS OP==COLON_OP || OP==APPEND_OP || OP==LISTDIFF_OP ||
                OP==AND_OP || OP==OR_OP || OP==EXP_OP ?
             INFIXPRIOVEC[OP] : INFIXPRIOVEC[OP] + 1;  }

STATIC BOOL
ROTATE(LIST E)
                    //PUTS NESTED AND'S INTO RIGHTIST FORM TO ENSURE
                    //DETECTION OF CONTINUED RELATIONS
{  WHILE ISCONS(HD(TL(E))) && HD(HD(TL(E)))==(LIST)AND_OP
   DO {  LIST X=TL(HD(TL(E))), C=TL(TL(E));
         LIST A=HD(X), B=TL(X);
         HD(TL(E))=A, TL(TL(E))=CONS((LIST)AND_OP,CONS(B,C)); }
   RESULTIS TRUE;  }

//DECOMPILER

VOID
DISPLAY(ATOM ID, BOOL WITHNOS, BOOL DOUBLESPACING)
                // THE VAL FIELD OF EACH USER DEFINED NAME
                // CONTAINS - CONS(CONS(NARGS,COMMENT),<LIST OF EQNS>)
   {  IF VAL(ID)==NIL
      DO {  WRITEF("\"%s\" - not defined\n",PRINTNAME(ID));
            RETURN }
   {  LIST X = HD(VAL(ID)), EQNS = TL(VAL(ID));
      WORD NARGS = (WORD)(HD(X));
      LIST COMMENT = TL(X);
      WORD N = LENGTH(EQNS), I;
      LASTLHS=NIL;
      UNLESS COMMENT==NIL
      DO {  LIST C=COMMENT;
            WRITEF("    %s :-",PRINTNAME(ID));
            UNTIL C==NIL
            DO {  WRITES(PRINTNAME((ATOM)HD(C)));
                  C = TL(C);
                  UNLESS C==NIL 
                  DO {  NEWLINE();
                        IF DOUBLESPACING DO NEWLINE(); }
               }
            WRITES(";\n");
            IF DOUBLESPACING DO NEWLINE();  }
      IF COMMENT!=NIL && N==1 && HD(TL(HD(EQNS)))==(LIST)CALL_C 
	 DO RETURN
      FOR (I=1; I<=N; I++)
         {  TEST WITHNOS && (N>1 || COMMENT!=NIL)
            THEN WRITEF("%2" W ") ",I);
            OR WRITES("    ");
            REMOVELINENO(HD(EQNS));
            DISPLAYEQN(ID,NARGS,HD(EQNS));
            IF DOUBLESPACING DO NEWLINE();
            EQNS=TL(EQNS);
   }  }  }

STATIC VOID
SHCH(WORD CH)
{  TRUEWRCH(' '); }

VOID
DISPLAYEQN(ATOM ID, WORD NARGS, LIST EQN)    //EQUATION DECODER
   {  LIST LHS = HD(EQN), CODE = TL(EQN);
      TEST NARGS==0
      THEN {  WRITES(PRINTNAME(ID)); LASTLHS=(LIST)ID;  }
      OR {  TEST EQUAL(LHS,LASTLHS)
            THEN _WRCH=SHCH;
            OR LASTLHS=LHS;
            PRINTEXP(LHS,0);
            _WRCH=TRUEWRCH;  }
      WRITES(" = ");
      TEST HD(CODE)==(LIST)CALL_C THEN WRITES("<primitive function>");
      OR DISPLAYRHS(LHS,NARGS,CODE);
      NEWLINE();
   }

VOID
DISPLAYRHS(LIST LHS, WORD NARGS, LIST CODE)
{  LIST V[100];
   WORD I = NARGS, J; BOOL IF_FLAG = FALSE;
   WHILE I>0 //UNPACK FORMAL PARAMETERS INTO V
   DO {  I = I-1;
	 V[I] = TL(LHS);
	 LHS = HD(LHS); }
   I = NARGS-1;
   do
   {  SWITCHON (WORD)(HD(CODE)) INTO
      {  CASE LOAD_C: CODE=TL(CODE);
                      I=I+1;
                      V[I]=HD(CODE);
                      ENDCASE
         CASE LOADARG_C: CODE=TL(CODE);
                         I=I+1;
                         V[I]=V[(WORD)(HD(CODE))];
                         ENDCASE
         CASE APPLY_C: I=I-1;
                       V[I]=CONS(V[I],V[I+1]);
                       ENDCASE
         CASE APPLYINFIX_C: CODE=TL(CODE);
                            I=I-1;
                            V[I]=CONS(HD(CODE),CONS(V[I],V[I+1]));
                            ENDCASE
         CASE CONTINUE_INFIX_C: CODE=TL(CODE);
                                V[I-1]=CONS(HD(CODE),
                                          CONS(V[I-1],V[I]));
                         //NOTE THAT 2ND ARG IS LEFT IN PLACE ABOVE
                         //NEW EXPRESSION
                                ENDCASE
         CASE IF_C: IF_FLAG=TRUE;
                    ENDCASE
         CASE FORMLIST_C: CODE=TL(CODE);
                          I=I+1;
                          V[I]=NIL;
                          FOR (J=1; J<=(WORD)(HD(CODE)); J++)
                             {  I=I-1;
                                V[I]=CONS((LIST)COLON_OP,CONS(V[I],V[I+1]));
                             }
                          ENDCASE
         CASE FORMZF_C: CODE=TL(CODE);
                        I=I-(WORD)(HD(CODE));
                        V[I]=CONS(V[I],NIL);
                        FOR (J=(WORD)(HD(CODE)); J>=1; J=J-1)
                           V[I] = CONS(V[I+J],V[I]);
                        V[I] = CONS((LIST)ZF_OP,V[I]);
                        ENDCASE
         CASE CONT_GENERATOR_C:
                CODE = TL(CODE);
                FOR (J=1; J<=(WORD)(HD(CODE)); J++)
                   V[I-J] = CONS((LIST)GENERATOR,CONS(V[I-J],
                                    TL(TL(V[I]))));
                ENDCASE
         CASE MATCH_C:
         CASE MATCHARG_C:
                       CODE=TL(CODE);
                       CODE=TL(CODE);
                       ENDCASE
         CASE MATCHPAIR_C: CODE=TL(CODE);
                        {  LIST X = V[(WORD)HD(CODE)];
                           I=I+2;
                           V[I-1]=HD(TL(X)), V[I]=TL(TL(X));  }
                           ENDCASE
         CASE STOP_C: PRINTEXP(V[I],0);
                      UNLESS IF_FLAG DO RETURN
                      WRITES(", ");
                      PRINTEXP(V[I-1],0);
                      RETURN
         DEFAULT: WRITES("IMPOSSIBLE INSTRUCTION IN \"DISPLAYRHS\"\n");
      } //END OF SWITCH
      CODE=TL(CODE);
   } REPEAT;
}

LIST
PROFILE(LIST EQN) //EXTRACTS THAT PART OF THE CODE WHICH 
                       //DETERMINES WHICH CASES THIS EQUATION APPLIES TO
{  LIST CODE=TL(EQN);
   IF HD(CODE)==(LIST)LINENO_C
   DO CODE=TL(TL(CODE));
{  LIST C=CODE;
   WHILE PARMY(HD(C)) DO C=REST(C);
{  LIST HOLD=C;
   UNTIL HD(C)==(LIST)IF_C||HD(C)==(LIST)STOP_C DO C=REST(C);
   TEST HD(C)==(LIST)IF_C
   THEN RESULTIS SUBTRACT(CODE,C);
   OR RESULTIS SUBTRACT(CODE,HOLD);
}  }  }

STATIC BOOL
PARMY(LIST X)
{  RESULTIS X==(LIST)MATCH_C||X==(LIST)MATCHARG_C||X==(LIST)MATCHPAIR_C;
}

STATIC LIST
REST(LIST C)   //REMOVES ONE COMPLETE INSTRUCTION FROM C
{  LIST X=HD(C);
   C=TL(C);
   IF X==(LIST)APPLY_C||X==(LIST)IF_C||X==(LIST)STOP_C DO RESULTIS C;
   C=TL(C);
   UNLESS X==(LIST)MATCH_C||X==(LIST)MATCHARG_C DO RESULTIS C;
   RESULTIS TL(C);  }

STATIC LIST
SUBTRACT(LIST X, LIST Y)  //LIST SUBTRACTION
{  LIST Z=NIL;
   UNTIL X==Y
   DO Z = CONS(HD(X),Z), X = TL(X);
   RESULTIS Z; //NOTE THE RESULT IS REVERSED - FOR OUR PURPOSES THIS
}              //DOES NOT MATTER

VOID
REMOVELINENO(LIST EQN)
  //CALLED WHENEVER THE DEFINIENDUM IS SUBJECT OF A
  //DISPLAY,REORDER OR (PARTIAL)DELETE COMMAND - HAS THE EFFECT OF
  //RESTORING THE STANDARD LINE NUMBERING
   { IF HD(TL(EQN))==(LIST)LINENO_C
   DO TL(EQN)=TL(TL(TL(EQN)));
}

//COMPILER FOR KRC EXPRESSIONS AND EQUATIONS

LIST
EXP()
{  INIT_CODEV();
   EXPR(0);
   PLANT0(STOP_C);
   RESULTIS COLLECTCODE();
}

LIST
EQUATION()      //RETURNS A TRIPLE: CONS(SUBJECT,CONS(NARGS,EQN))
{  LIST SUBJECT = 0, LHS = 0;
   WORD NARGS = 0;
   INIT_CODEV();
   TEST HAVEID()
   THEN {  SUBJECT=(LIST)THE_ID,LHS=(LIST)THE_ID;
           WHILE STARTFORMAL(HD(TOKENS))
           DO {  LHS=CONS(LHS,FORMAL());
                 NARGS=NARGS+1;  }
        } OR
   TEST HD(TOKENS)==(LIST)'=' && LASTLHS!=NIL
   THEN {  SUBJECT=LASTLHS,LHS=LASTLHS;
           WHILE ISCONS(SUBJECT)
           DO SUBJECT=HD(SUBJECT),NARGS=NARGS+1;
        }
   OR {  SYNTAX(), WRITES("missing LHS\n");
         RESULTIS NIL;  }
   COMPILELHS(LHS,NARGS);
{  LIST CODE=COLLECTCODE();
   CHECK((TOKEN)'=');
   EXPR(0);
   PLANT0(STOP_C);
{  LIST EXPCODE=COLLECTCODE();
   TEST HAVE((TOKEN)',') //CHANGE FROM EMAS/KRC TO ALLOW GUARDED SIMPLE DEF
   THEN {  EXPR(0);
           PLANT0(IF_C);
           CODE=APPEND(CODE,APPEND(COLLECTCODE(),EXPCODE));  }
   OR CODE=APPEND(CODE,EXPCODE);
   UNLESS HD(TOKENS)==ENDSTREAMCH DO CHECK(EOL);
   UNLESS ERRORFLAG DO LASTLHS=LHS;
   IF NARGS==0 DO LHS=0;//IN THIS CASE THE LHS FIELD IS USED TO REMEMBER
       //THE VALUE OF THE VARIABLE - 0 MEANS NOT YET SET
   RESULTIS CONS(SUBJECT,CONS((LIST)NARGS,CONS(LHS,CODE))); // OK
}  }  }

STATIC VOID
EXPR(WORD N)  //N IS THE PRIORITY LEVEL
   {  TEST N<=3 &&(HAVE((TOKEN)'\\') || HAVE((TOKEN)'~'))
      THEN {  PLANT1(LOAD_C,(LIST)NOT_OP);
              EXPR(3);
              PLANT0(APPLY_C);  } OR
      TEST N<=5 && HAVE((TOKEN)'+') THEN EXPR(5); OR
      TEST N<=5 && HAVE((TOKEN)'-')
      THEN {  PLANT1(LOAD_C,(LIST)NEG_OP);
              EXPR(5);
              PLANT0(APPLY_C);  } OR
      TEST HAVE((TOKEN)'#')
      THEN {  PLANT1(LOAD_C,(LIST)LENGTH_OP);
              COMBN();
              PLANT0(APPLY_C);  } OR
      TEST STARTSIMPLE(HD(TOKENS))
      THEN COMBN();
      OR { SYNTAX(); RETURN }
   {  OPERATOR OP=MKINFIX(HD(TOKENS));
      WHILE DIPRIO(OP)>=N
      DO {  WORD I, AND_COUNT=0; //FOR CONTINUED RELATIONS
            TOKENS=TL(TOKENS);
            EXPR(RIGHTPREC(OP));
            IF ERRORFLAG DO RETURN;
            WHILE ISRELOP((LIST)OP) && ISRELOP((LIST)MKINFIX(HD(TOKENS)))
            DO {  //CONTINUED RELATIONS
                  AND_COUNT=AND_COUNT+1;
                  PLANT1(CONTINUE_INFIX_C,(LIST)OP);
                  OP=MKINFIX(HD(TOKENS));
                  TOKENS=TL(TOKENS);
                  EXPR(4);
                  IF ERRORFLAG DO RETURN  }
            PLANT1(APPLYINFIX_C,(LIST)OP);
            FOR (I=1; I<=AND_COUNT; I++)
	       PLANT1(APPLYINFIX_C,(LIST)AND_OP);
                        //FOR CONTINUED RELATIONS
            OP=MKINFIX(HD(TOKENS));  }
}  }

STATIC VOID
COMBN()
{ SIMPLE();
  WHILE STARTSIMPLE(HD(TOKENS))
  DO { SIMPLE();
       PLANT0(APPLY_C); }
}

STATIC BOOL
STARTFORMAL(TOKEN T)
{  RESULTIS ISCONS(T) ? (HD(T)==IDENT || HD(T)==(LIST)CONST) :
   T==(TOKEN)'(' || T==(TOKEN)'[' || T == (TOKEN)'-';  }

STATIC BOOL
STARTSIMPLE(TOKEN T)
{  RESULTIS ISCONS(T) ? (HD(T)==IDENT || HD(T)==(LIST)CONST) :
   T==(TOKEN)'(' || T==(TOKEN)'[' || T==(TOKEN)'{' || T==(TOKEN)'\'';  }

STATIC VOID
SIMPLE()
{  TEST HAVEID()
   THEN COMPILENAME(THE_ID); OR
   TEST HAVECONST()
   THEN PLANT1(LOAD_C,(LIST)INTERNALISE(THE_CONST)); OR
   TEST HAVE((TOKEN)'(')
   THEN {  EXPR(0); CHECK((TOKEN)')');  } OR
   TEST HAVE((TOKEN)'[')
   THEN TEST HAVE((TOKEN)']')
        THEN PLANT1(LOAD_C,NIL);
        OR {  WORD N=1;
              EXPR(0);
              IF HAVE((TOKEN)',')
              DO {  EXPR(0);
                    N=N+1;  }
              TEST HAVE(DOTDOT_SY)
              THEN {  TEST HD(TOKENS)==(TOKEN)']'
                      THEN PLANT1(LOAD_C,INFINITY);
                      OR EXPR(0);
                      IF N==2 DO PLANT0(APPLY_C);
                      PLANT1(APPLYINFIX_C,
			 (LIST)(N==1 ? DOTDOT_OP : COMMADOTDOT_OP));  } // OK
              OR {  WHILE HAVE((TOKEN)',')
                    DO {  EXPR(0);
                          N=N+1;  }
                    PLANT1(FORMLIST_C,(LIST)N);  } // OK
              CHECK((TOKEN)']');  } OR
    TEST HAVE((TOKEN)'{')  // ZF EXPRESSIONS	BUG?
    THEN {  WORD N = 0;
            LIST HOLD = TOKENS;
            PERFORM_ALPHA_CONVERSIONS();
            EXPR(0);
            //TEST HD(TOKENS)==BACKARROW_SY  //IMPLICIT ZF BODY
                      //NO LONGER LEGAL
            //THEN TOKENS=HOLD; OR
            CHECK((TOKEN)';');
            do N = N + QUALIFIER(); REPEATWHILE(HAVE((TOKEN)';'));
            PLANT1(FORMZF_C,(LIST)N); // OK
            CHECK((TOKEN)'}'); }  OR
   TEST HAVE((TOKEN)'\'') //OPERATOR DENOTATION
   THEN {  TEST HAVE((TOKEN)'#') THEN PLANT1(LOAD_C,(LIST)LENGTH_OP); OR
	   TEST HAVE((TOKEN)'\\') || HAVE((TOKEN)'~') THEN PLANT1(LOAD_C,(LIST)NOT_OP);
           OR {  OPERATOR OP=MKINFIX((TOKEN)(HD(TOKENS)));
                 TEST ISINFIX((LIST)OP) THEN TOKENS=TL(TOKENS);
                 OR SYNTAX(); //MISSING INFIX OR PREFIX OPERATOR
                 PLANT1(LOAD_C,(LIST)QUOTE_OP);
                 PLANT1(LOAD_C,(LIST)OP);
                 PLANT0(APPLY_C); }
           CHECK((TOKEN)'\'');  }
   OR SYNTAX(); //MISSING identifier|constant|(|[|{
}

STATIC VOID
COMPILENAME(ATOM N)
   {  WORD I=0;
      UNTIL I>ENVP || ENV[I]==(LIST)N
      DO I=I+1;
      TEST I>ENVP
      THEN PLANT1(LOAD_C,(LIST)N);
      OR PLANT1(LOADARG_C,(LIST)I); //OK
   }

STATIC WORD
QUALIFIER()
{  TEST ISGENERATOR(TL(TOKENS))  //WHAT ABOUT MORE GENERAL FORMALS?
   THEN {  WORD N=0;
           do {
              HAVEID();
              PLANT1(LOAD_C,(LIST)THE_ID);
              N = N+1;
           } REPEATWHILE(HAVE((TOKEN)','));
           CHECK(BACKARROW_SY);
           EXPR(0);
           PLANT1(APPLYINFIX_C,(LIST)GENERATOR);
           IF N>1 DO PLANT1(CONT_GENERATOR_C,(LIST)(N-1)); // OK
           RESULTIS N; }
   OR {  EXPR(0) ; RESULTIS 1;  }
}

STATIC VOID
PERFORM_ALPHA_CONVERSIONS()
  //ALSO RECOGNISES THE "SUCH THAT" BAR AND CONVERTS IT TO ';'
  //TO DISTINGUISH IT FROM "OR"
   {  LIST P=TOKENS;
      UNTIL HD(P)==(TOKEN)'}' || HD(P)==(TOKEN)']' || HD(P)==EOL
      DO {  IF HD(P)==(TOKEN)'[' || HD(P)==(TOKEN)'{'
            DO {  P = SKIPCHUNK(P);
                  LOOP;  }
            IF HD(P)==(TOKEN)'|' && ISID(HD(TL(P))) && ISGENERATOR(TL(TL(P)))
            DO HD(P) = (TOKEN)';' ;
            IF ISID(HD(P)) && ISGENERATOR(TL(P))
            DO ALPHA_CONVERT(HD(P),TL(P));
            P=TL(P);  }  }

BOOL
ISID(LIST X) { RESULTIS ISCONS(X) && HD(X)==IDENT; }

STATIC BOOL
ISGENERATOR(LIST T)
{    RESULTIS !ISCONS(T) ? FALSE :
     HD(T)==BACKARROW_SY ||
     (HD(T)==(TOKEN)',' && ISID(HD(TL(T))) && ISGENERATOR(TL(TL(T))));
}

STATIC VOID
ALPHA_CONVERT(LIST VAR, LIST P)
   {  LIST T=TOKENS;
      LIST VAR1=CONS((LIST)ALPHA,TL(VAR));
      LIST EDGE=T;
      UNTIL HD(EDGE)==(TOKEN)';' || HD(EDGE)==BACKARROW_SY || HD(EDGE)==EOL
      DO EDGE=SKIPCHUNK(EDGE);
      UNTIL T==EDGE
      DO {  CONV1(T,VAR,VAR1);
            T=TL(T);  }
      T=P;
      UNTIL HD(T)==(TOKEN)';' || HD(T)==EOL DO T=SKIPCHUNK(T);
      EDGE=T;
      UNTIL HD(EDGE)==(TOKEN)'}' || HD(EDGE)==(TOKEN)']' || HD(EDGE)==EOL
      DO EDGE=SKIPCHUNK(EDGE);
      UNTIL T==EDGE
      DO {  CONV1(T,VAR,VAR1);
            T=TL(T);  }
      TL(VAR)=VAR1;
   }

STATIC LIST
SKIPCHUNK(LIST P)
{  WORD KET = HD(P)==(TOKEN)'{' ? '}' : HD(P)==(TOKEN)'[' ? ']' : -1;
   P=TL(P);
   IF KET==-1 DO RESULTIS P;
   UNTIL HD(P)==(LIST)KET || HD(P)==EOL // OK
   DO P = SKIPCHUNK(P);
   UNLESS HD(P)==EOL DO P=TL(P);
   RESULTIS(P);
}

STATIC VOID
CONV1(LIST T, LIST VAR, LIST VAR1)
{  IF EQUAL(HD(T),VAR) && HD(T)!=VAR DO TL(HD(T))=VAR1;  }

STATIC
LIST FORMAL()
{  TEST HAVEID() THEN RESULTIS (LIST)THE_ID; OR
   TEST HAVECONST() THEN RESULTIS INTERNALISE(THE_CONST); OR
   TEST HAVE((TOKEN)'(')
   THEN {  LIST P=PATTERN();
           CHECK((TOKEN)')');
           RESULTIS P;  } OR
   TEST HAVE((TOKEN)'[')
   THEN {  LIST PLIST=NIL,P=NIL;
           IF HAVE((TOKEN)']') DO RESULTIS NIL;
           do PLIST=CONS(PATTERN(),PLIST);
           REPEATWHILE(HAVE((TOKEN)','));  //NOTE THEY ARE IN REVERSE ORDER
           CHECK((TOKEN)']');
           UNTIL PLIST==NIL
           DO {  P=CONS((TOKEN)COLON_OP,CONS(HD(PLIST),P));
                 PLIST=TL(PLIST);  } //NOW THEY ARE IN CORRECT ORDER
           RESULTIS P;  } OR
   TEST HAVE((TOKEN)'-') && HAVENUM()
   THEN {  THE_NUM = -THE_NUM;
           RESULTIS STONUM(THE_NUM);  }
   OR {  SYNTAX(); //MISSING identifier|constant|(|[
         RESULTIS NIL;
}  }

STATIC LIST
INTERNALISE(LIST VAL)
{     RESULTIS VAL==TL(TRUTH) ? TRUTH :
               VAL==TL(FALSITY) ? FALSITY :
               ISATOM(VAL) ? CONS((LIST)QUOTE,VAL) : VAL;  }

STATIC LIST
PATTERN()
{  LIST P=FORMAL();
   IF HAVE((TOKEN)':')
   DO P=CONS((LIST)COLON_OP,CONS(P,PATTERN()));
   RESULTIS P;  }

STATIC VOID
COMPILELHS(LIST LHS, WORD NARGS)
   {  WORD I;
      ENVP=NARGS-1;
      FOR (I=1; I<=NARGS; I++)
      {  ENV[NARGS-I]=TL(LHS);
         LHS=HD(LHS);  }
      FOR (I=0; I<=NARGS-1; I++) COMPILEFORMAL(ENV[I],I);
   }

STATIC VOID
COMPILEFORMAL(LIST X, WORD I)
{  TEST ISATOM(X)  //IDENTIFIER
   THEN {  WORD J=0;
           UNTIL J>=I || ENV[J]==X
           DO J=J+1;  // IS THIS A REPEATED NAME?
           TEST J>=I
           THEN RETURN   // NO, NO CODE COMPILED
           OR PLANT2(MATCHARG_C,(LIST)I,(LIST)J);  } OR
   TEST ISNUM(X) || X==NIL || (ISCONS(X) && HD(X)==(LIST)QUOTE)
   THEN PLANT2(MATCH_C,(LIST)I,X); OR
   TEST ISCONS(X) && HD(X)==(TOKEN)COLON_OP && ISCONS(TL(X))
   THEN {  PLANT1(MATCHPAIR_C,(LIST)I); // OK
           ENVP=ENVP+2;
        {  WORD A=ENVP-1,B=ENVP;
           ENV[A]=HD(TL(X)), ENV[B]=TL(TL(X));
           COMPILEFORMAL(ENV[A],A);
           COMPILEFORMAL(ENV[B],B);
        }  }
   OR WRITES("Impossible event in \"COMPILEFORMAL\"\n");
}

// PLANT stores INSTRUCTIONs and their operands in the code vector
// OP is always an instruction code (*_C);
// A and B can be operators (*_OP), INTs, CONSTs, IDs (names) or
// the address of a C function - all are mapped to LIST type.

// APPLY_C IF_C STOP_C
STATIC VOID
PLANT0(INSTRUCTION OP)
   {  CODEV=CONS((LIST)OP, CODEV); }

// everything else
STATIC VOID
PLANT1(INSTRUCTION OP, LIST A)
   { CODEV=CONS((LIST)OP, CODEV);
     CODEV=CONS(A, CODEV); }

// MATCH_C MATCHARG_C
STATIC VOID
PLANT2(INSTRUCTION OP, LIST A, LIST B)
   { CODEV=CONS((LIST)OP, CODEV);
     CODEV=CONS(A, CODEV);
     CODEV=CONS(B, CODEV); }

STATIC LIST
COLLECTCODE()          //FLUSHES THE CODE BUFFER
{  LIST TMP=CODEV;
   CODEV=NIL;
   RESULTIS REVERSE(TMP);
}

// Mark elements in CODEV and ENV for preservation by the GC.
// This routine should be called by your BASES() function.
VOID
COMPILER_BASES(VOID (*F)(LIST *))
{  WORD I;

   F(&CODEV);
   // ENVP indexes the last used element and starts as -1.
   FOR (I=0; I<=ENVP ; I++) F(&ENV[I]);
}
