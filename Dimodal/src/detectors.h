
#ifndef __DETECTORS
#define __DETECTORS 1


#include <R.h>
#include <Rinternals.h>


/**** Macros/Defines ****/

/* indices (C based) in the find_runs result */
#define RUNID_RUNS           0
#define RUNID_SKIP           1
#define RUNID_STATS          2
#define RUNID_STAT_NRUN      0
#define RUNID_STAT_MAXRUN    1
#define RUNID_STAT_NX        2


/**** Prototypes ****/

extern SEXP C_find_runs(SEXP, SEXP);
extern SEXP C_find_peaks(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_find_flats(SEXP, SEXP, SEXP, SEXP, SEXP ,SEXP);
extern SEXP C_find_level_sections(SEXP, SEXP, SEXP);

/* Exposed to R for the test bench, but not published. */
extern SEXP bist_flats(void);

/* Internal functions for other C code. */
SEXP impl_runs(SEXP, double);


#endif     /* __DETECTORS */
