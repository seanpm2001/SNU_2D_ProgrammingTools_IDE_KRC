// Header file for EMAS emulation stubs

extern char *emas_PROMPT;
extern int TERMINATOR;	// The character gobbled after a READN()

#define PROMPT(S)         emas_PROMPT=S
#define SUPPRESSPROMPTS() emas_PROMPT=""

// Other stuff;
//
// APTOVEC(function, size)
//	Allocates size+1 bytes on the stack and calls the function
//	passing the address of the vector and the "size" parameter.
// FILES(DESCRIPTOR(), PARAMS)
//      Lists the names of your files
