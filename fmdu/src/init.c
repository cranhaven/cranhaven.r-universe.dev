

#include <stdlib.h>
#include <R_ext/Rdynload.h>
#define R

extern void Ccolresmdu( int* rn, int* rm, double* rdelta, int* rp, double* rx, int* rfx, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Ccolresmduneg( int* rn, int* rm, double* rdelta, int* rp, double* rx, int* rfx, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Ccolreswgtmdu( int* rn, int* rm, double* rdelta, double* rw, int* rp, double* rx, int* rfx, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Ccolreswgtmduneg( int* rn, int* rm, double* rdelta, double* rw, int* rp, double* rx, int* rfx, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Cmdu( int* rn, int* rm, double* rdelta, int* rp, double* rx, int* rfx, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Cmduneg( int* rn, int* rm, double* rdelta, int* rp, double* rx, int* rfx, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Cresmdu( int* rn, int* rm, double* rdelta, int* rp, int* rhx, double* rqx, double* rbx, int* rhy, double* rqy, double* rby, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Cresmduneg( int* rn, int* rm, double* rdelta, int* rp, int* rhx, double* rqx, double* rbx, int* rhy, double* rqy, double* rby, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Creswgtmdu( int* rn, int* rm, double* rdelta, double* rw, int* rp, int* rhx, double* rqx, double* rbx, int* rhy, double* rqy, double* rby, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Creswgtmduneg( int* rn, int* rm, double* rdelta, double* rw, int* rp, int* rhx, double* rqx, double* rbx, int* rhy, double* rqy, double* rby, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Crowresmdu( int* rn, int* rm, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Crowresmduneg( int* rn, int* rm, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Crowreswgtmdu( int* rn, int* rm, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Crowreswgtmduneg( int* rn, int* rm, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Cwgtmdu( int* rn, int* rm, double* rdelta, double* rw, int* rp, double* rx, int* rfx, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Cwgtmduneg( int* rn, int* rm, double* rdelta, double* rw, int* rp, double* rx, int* rfx, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );
extern void Cexternal( int* rn, int* rm, double* rdelta, double* rw, int* rp, double* rfixed, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );


static const R_CMethodDef CEntries[] = {
  {"Ccolresmdu",      ( DL_FUNC ) &Ccolresmdu,         14},
  {"Ccolresmduneg",      ( DL_FUNC ) &Ccolresmduneg,         14},
  {"Ccolreswgtmdu",      ( DL_FUNC ) &Ccolreswgtmdu,         15},
  {"Ccolreswgtmduneg",      ( DL_FUNC ) &Ccolreswgtmduneg,         15},
  {"Cmdu",      ( DL_FUNC ) &Cmdu,         13},
  {"Cmduneg",      ( DL_FUNC ) &Cmduneg,         13},
  {"Cresmdu",      ( DL_FUNC ) &Cresmdu,         15},
  {"Cresmduneg",      ( DL_FUNC ) &Cresmduneg,         15},
  {"Creswgtmdu",      ( DL_FUNC ) &Creswgtmdu,         16},
  {"Creswgtmduneg",      ( DL_FUNC ) &Creswgtmduneg,         16},
  {"Crowresmdu",      ( DL_FUNC ) &Crowresmdu,         14},
  {"Crowresmduneg",      ( DL_FUNC ) &Crowresmduneg,         14},
  {"Crowreswgtmdu",      ( DL_FUNC ) &Crowreswgtmdu,         15},
  {"Crowreswgtmduneg",      ( DL_FUNC ) &Crowreswgtmduneg,         15},
  {"Cwgtmdu",      ( DL_FUNC ) &Cwgtmdu,         14},
  {"Cwgtmduneg",      ( DL_FUNC ) &Cwgtmduneg,         14},
  {"Cexternal",      ( DL_FUNC ) &Cexternal,         12},
  {NULL, NULL, 0}
};

void R_init_fmdu( DllInfo *dll )
{
  R_registerRoutines( dll, CEntries, NULL, NULL, NULL );
  R_useDynamicSymbols( dll, FALSE );
}
