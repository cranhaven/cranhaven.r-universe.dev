/* errAbort.c - our error handler.
 *
 * This maintains two stacks - a warning message printer
 * stack, and a "abort handler" stack.
 *
 * Note that the abort function always calls the warn handler first.
 * This is so that the message gets sent.
 *
 * By default the warnings will go to stderr, and
 * aborts will exit the program.  You can push a
 * function on to the appropriate stack to change
 * this behavior.  The top function on the stack
 * gets called.
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

// developer: this include is for an occasionally useful means of getting stack info without
// crashing
// however, it is not supported on cygwin.  Conditionally compile this in when desired.
//#define BACKTRACE_EXISTS
#ifdef BACKTRACE_EXISTS
#include <execinfo.h>
#endif///def BACKTRACE_EXISTS
#include "common.h"
#include "errAbort.h"

#include <R_ext/Print.h>

#define maxWarnHandlers 20
#define maxAbortHandlers 12

void noWarnAbort(void)
/* Abort with without informative message. */
{
Rf_error("%s", "unexpected error in Rtwobitlib");
}

void errnoAbort(char *format, ...)
/* Prints error message from UNIX errno first, then does errAbort. */
{
char fbuf[1024];
va_list args;
va_start(args, format);
Rprintf("%s\n", strerror(errno));
vsnprintf(fbuf, sizeof(fbuf), format, args);
va_end(args);
Rf_error("%s", fbuf);
}

