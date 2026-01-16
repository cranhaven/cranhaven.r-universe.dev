
#ifndef __DATA
#define __DATA 1


#include <R.h>
#include <Rinternals.h>


/**** Macros/Defines ****/

/* indices (C based) of the segment endpoints */
#define SEGID_X          0
#define SEGID_Q          1


/**** Prototypes ****/

extern SEXP C_midq(SEXP, SEXP, SEXP);
extern SEXP C_eval_midq(SEXP, SEXP, SEXP);


#endif     /* __DATA */
