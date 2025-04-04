/* ErrAbort.h - our error handler. 
 *
 * This maintains two stacks - a warning message printer
 * stack, and a "abort handler" stack.
 *
 * By default the warnings will go to stderr, and
 * aborts will exit the program.  You can push a
 * function on to the appropriate stack to change
 * this behavior.  The top function on the stack
 * gets called.
 *
 * Most functions in this library will call errAbort()
 * if they run out of memory.  
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#ifndef ERRABORT_H
#define ERRABORT_H

void errnoAbort(char *format, ...)
/* Prints error message from UNIX errno first, then does errAbort. */
#if defined(__GNUC__)
__attribute__((format(printf, 1, 2)))
#endif
;

typedef void (*AbortHandler)(void);
/* Function that can abort. */

void noWarnAbort(void);
/* Abort without message. */

typedef void (*WarnHandler)(char *format, va_list args);
/* Function that can warn. */

#endif /* ERRABORT_H */
