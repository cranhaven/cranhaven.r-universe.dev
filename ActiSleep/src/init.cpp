
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 Throws error "symbol not found in flat namespace" if commented code is
 uncommented
 */

/* .C calls */
// extern void SegmentBinNeg(void *, void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentBinNegKeep(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentExponential(void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentExponentialKeep(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentNormal(void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentNormalKeep(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentPoisson(void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentPoissonKeep(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentVariance(void *, void *, void *, void *, void *, void *, void *, void *, void *);
// extern void SegmentVarianceKeep(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
//  {"SegmentBinNeg",          (DL_FUNC) &SegmentBinNeg,           9},
//  {"SegmentBinNegKeep",      (DL_FUNC) &SegmentBinNegKeep,      11},
//  {"SegmentExponential",     (DL_FUNC) &SegmentExponential,      8},
//  {"SegmentExponentialKeep", (DL_FUNC) &SegmentExponentialKeep, 10},
//  {"SegmentNormal",          (DL_FUNC) &SegmentNormal,           8},
//  {"SegmentNormalKeep",      (DL_FUNC) &SegmentNormalKeep,      10},
//  {"SegmentPoisson",         (DL_FUNC) &SegmentPoisson,          8},
//  {"SegmentPoissonKeep",     (DL_FUNC) &SegmentPoissonKeep,     10},
//  {"SegmentVariance",        (DL_FUNC) &SegmentVariance,         9},
//  {"SegmentVarianceKeep",    (DL_FUNC) &SegmentVarianceKeep,    11},
  {NULL, NULL, 0}
};

void R_init_ActiSleep(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
