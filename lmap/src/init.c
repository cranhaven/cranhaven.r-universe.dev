

#include <stdlib.h>
#include <R_ext/Rdynload.h>


extern void Cmulnomrowresmduneg( int* rn, int* rc, double* rg, int* rp, double* rx, int* rm, double* rb, double* rv, double* ru, double* rtheta, int* rmaxiter, double* rdcrit, int* rmaxinner, double* rfcrit, double* rdeviance );
extern void Cmulvarbincolresmduneg( int* rn, int* rr, double* ry, int* rm, double* ru, int* rp, double* rx, double* rb, int* rmains, double* rmu, int* rmaxiter, double* rdcrit, int* rmaxinner, double* rfcrit, double* rdeviance );
extern void Cmulvarbinmduneg( int* rn, int* rr, double* ry, int* rm, double* ru, double* rv, int* rmains, double* rmu, int* rmaxiter, double* rdcrit, int* rmaxinner, double* rfcrit, double* rdeviance );
extern void Cmulvarbinresmduneg( int* rn, int* rr, double* ry, int* rm, int* rpu, double* rxu, double* rbu, int* rpv, double* rxv, double* rbv, int* rmains, double* rmu, int* rmaxiter, double* rdcrit, int* rmaxinner, double* rfcrit, double* rdeviance );
extern void Cmulvarbinrowresmduneg( int* rn, int* rr, double* ry, int* rp, double* rx, int* rm, double* rb, double* rv, int* rmains, double* rmu, int* rmaxiter, double* rdcrit, int* rmaxinner, double* rfcrit, double* rdeviance );
extern void Cmulvarbinwgtmduneg( int* rn, int* rr, double* ry, double* rw, int* rm, double* ru, double* rv, int* rmains, double* rmu, int* rmaxiter, double* rdcrit, int* rmaxinner, double* rfcrit, double* rdeviance );

static const R_CMethodDef CEntries[] = {
  {"Cmulnomrowresmduneg",      ( DL_FUNC ) &Cmulnomrowresmduneg,         15},
  {"Cmulvarbincolresmduneg",      ( DL_FUNC ) &Cmulvarbincolresmduneg,         15},
  {"Cmulvarbinmduneg",      ( DL_FUNC ) &Cmulvarbinmduneg,         13},
  {"Cmulvarbinresmduneg",      ( DL_FUNC ) &Cmulvarbinresmduneg,         17},
  {"Cmulvarbinrowresmduneg",      ( DL_FUNC ) &Cmulvarbinrowresmduneg,         15},
  {"Cmulvarbinwgtmduneg",      ( DL_FUNC ) &Cmulvarbinwgtmduneg,         14},
  {NULL, NULL, 0}
};

void R_init_fmdu( DllInfo *dll )
{
  R_registerRoutines( dll, CEntries, NULL, NULL, NULL );
  R_useDynamicSymbols( dll, FALSE );
}
