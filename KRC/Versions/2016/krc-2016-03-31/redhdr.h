// Global function declarations
VOID	SETUP_PRIMFNS_ETC(VOID);
VOID	PRINTVAL(LIST E, WORD FORMAT);
LIST	BUILDEXP(LIST CODE);

// GLOBAL FUNCTIONS IN REDUCER
VOID	INIT_ARGSPACE(VOID);
VOID	ESCAPETONEXTCOMMAND();
VOID	INITSTATS();
VOID	OUTSTATS();
VOID	FIXUP_S(VOID);
char *	SCASECONV(char *S);

// GLOBAL FUNCTIONS IN MAIN
VOID	CLOSECHANNELS();
FILE *	FINDCHANNEL(char *F);
VOID	ENTERSCRIPT(ATOM A);

// GLOBAL VARIABLES IN REDUCER
extern LIST MEMORIES;
extern WORD LISTBASE;
extern WORD ABORTED;

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

