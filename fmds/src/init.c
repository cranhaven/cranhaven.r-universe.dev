

#include <stdlib.h>
#include <R_ext/Rdynload.h>

extern void Caddconst( int* rn, double* rdelta, double* rac );
extern void Cfastaddconst( int* rn, double* rdelta, double* rac );


extern void Cbcxmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cbcxwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdbcxmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdbcxwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarbcxmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarbcxwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );


extern void CRexplain( int* rn, int* rp, double* rz, double* rq, double* rw, double* ra, double* re, int* rl, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );


extern void Cfasterstress( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnsamples, int* rsamplesize, int* rseed, double* stress, double* se );

extern void Cfastermds( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfastermdsneg( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfasterwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfasterfxdmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfasterordmds( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );



// extern void CRsuperfastmds1( int* rn, double* rdist, int* rp, double* rz, int* rminiter, double* rminrate, double* rfvalue, int* seed, int* recho );
// extern void CRsuperfastmds2( int* rn, double* rdelta, int* rp, double* rz, int* rminiter, double* rminrate, double* rfvalue, int* seed, int* recho );
// extern void CRsuperfastmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rminiter, double* rminrate, double* rfvalue, int* seed, int* recho );
// extern void CRmegafastmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rsubsize, int* rnsteps, double* rminrate, int* rseed );
// extern void CRultrafastmds1( int* rn, double* rdist, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
// extern void CRultrafastmds2( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
// extern void CRultrafastmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
// extern void CRultrafastmds4( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
// extern void CRultrafastweightedmds( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
// extern void CRultrafastordinalmds2( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );


extern void Clinmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Clinwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdlinmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdlinwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarlinmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarlinwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );

extern void Cmdist( int* rn, int* rm, double* ra, int* rlevel, int* rscale, double* rd );


extern void Cmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cmdsneg( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cwgtmdsneg( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdmdsneg( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdwgtmdsneg( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarmdsneg( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarwgtmdsneg( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cpenvarmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rrlambda, double* rllambda, double* rglambda, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );


extern void Cordmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* rapproach, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cordwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rapproach, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdordmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rapproach, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdordwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rapproach, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarordmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rapproach, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarordwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rapproach, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );


extern void Cpcoa( int* rn, double* rd, int* rp, double* rac, double* rz );
extern void Cfastpcoa( int* rn, double* rd, int* rp, double* rac, double* rz );
extern void Cfasterpcoa( int* rn, int* rm, double* rx, int* rp, int* rk, double* rz, int* rseed );
extern void Crespcoa( int* rn, double* rd, int* rh, double* rq, int* rp, double* rac, double* rb, double* rz );


extern void Cpowmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cpowwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdpowmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdpowwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarpowmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarpowwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );


extern void CRpredict( int* rn, int* rp, double* rz, double* rdelta, double* rw, double* rx, double* rd, int* rlevel, int* rmaxiter, double* rfdif, double* rfvalue, int* recho );

extern void Crcvpenvarmds( int* rnrepeats, int* rnfolds, int* rnlambda, double* rlambda, double* ralpha, int* rgrouped,
                           int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, int* recho,
                           double* rmserror, double* rstddev, double* rstderror );



extern void Crdop( int* rn, double* rdelta, int* rk, double* rlambda, double* rscores );
extern void Cerdop( int* rn, double* rdelta, int* rk, double* rlambda, double* rw, double* ralpha, double* rbeta );



extern void Csplmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Csplwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdsplmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cfxdsplwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarsplmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );
extern void Cvarsplwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho );

extern void Csimmds1( int* rn, double* rdist, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimmds2( int* rn, double* rdelta, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds1( int* rn, double* rdist, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds2( int* rn, double* rdelta, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimmds1local( int* rn, double* rdist, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimmds2local( int* rn, double* rdelta, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimmds3local( int* rn, int* rm, double* rx, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds1local( int* rn, double* rdist, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds2local( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds3local( int* rn, int* rm, double* rx, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimmds2interval( int* rn, double* rdelta, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds2interval( int* rn, double* rdelta, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimmds2localinterval( int* rn, double* rdelta, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdmds2localinterval( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds1( int* rn, double* rdist, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds2( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds3( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds1( int* rn, double* rdist, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds2( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds3( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds1local( int* rn, double* rdist, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds2local( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds3local( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds1local( int* rn, double* rdist, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds2local( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds3local( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds2interval( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds2interval( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed );
extern void Csimwgtmds2localinterval( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimfxdwgtmds2localinterval( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed );
extern void Csimlmkmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnepochs, double* rminrate, int* rnlandmarks, int* rseed );
extern void Csimlinmds2( int* rn, double* rdelta, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed );



static const R_CMethodDef CEntries[] = {
  {"Caddconst",      ( DL_FUNC ) &Caddconst,         3},
  {"Cfastaddconst",      ( DL_FUNC ) &Cfastaddconst,         3},
  {"Cbcxmds",      ( DL_FUNC ) &Cbcxmds,         10},
  {"Cbcxwgtmds",      ( DL_FUNC ) &Cbcxwgtmds,         11},
  {"Cfxdbcxmds",      ( DL_FUNC ) &Cfxdbcxmds,         11},
  {"Cfxdbcxwgtmds",      ( DL_FUNC ) &Cfxdbcxwgtmds,         12},
  {"Cvarbcxmds",      ( DL_FUNC ) &Cvarbcxmds,         12},
  {"Cvarbcxwgtmds",      ( DL_FUNC ) &Cvarbcxwgtmds,         13},
  {"CRexplain",      ( DL_FUNC ) &CRexplain,         12},

  {"Cfasterstress",      ( DL_FUNC ) &Cfasterstress,         10},

  {"Cfastermds",      ( DL_FUNC ) &Cfastermds,         7},

  {"Cfastermdsneg",      ( DL_FUNC ) &Cfastermdsneg,         7},
  {"Cfasterwgtmds",      ( DL_FUNC ) &Cfasterwgtmds,         8},
  {"Cfasterfxdmds",      ( DL_FUNC ) &Cfasterfxdmds,         8},
  {"Cfasterordmds",      ( DL_FUNC ) &Cfasterordmds,         7},
  // {"CRsuperfastmds1",      ( DL_FUNC ) &CRsuperfastmds1,         9},
  // {"CRsuperfastmds2",      ( DL_FUNC ) &CRsuperfastmds2,         9},
  // {"CRsuperfastmds3",      ( DL_FUNC ) &CRsuperfastmds3,         10},
  // {"CRmegafastmds3",      ( DL_FUNC ) &CRmegafastmds3,         9},
  // {"CRultrafastmds1",      ( DL_FUNC ) &CRultrafastmds1,         7},
  // {"CRultrafastmds2",      ( DL_FUNC ) &CRultrafastmds2,         7},
  //
  // {"CRultrafastmds3",      ( DL_FUNC ) &CRultrafastmds3,         8},
  // {"CRultrafastmds4",      ( DL_FUNC ) &CRultrafastmds4,         8},
  // {"CRultrafastweightedmds",      ( DL_FUNC ) &CRultrafastweightedmds,         9},
  // {"CRultrafastordinalmds2",      ( DL_FUNC ) &CRultrafastordinalmds2,         7},
  {"Clinmds",      ( DL_FUNC ) &Clinmds,         11},
  {"Clinwgtmds",      ( DL_FUNC ) &Clinwgtmds,         12},
  {"Cfxdlinmds",      ( DL_FUNC ) &Cfxdlinmds,         12},
  {"Cfxdlinwgtmds",      ( DL_FUNC ) &Cfxdlinwgtmds,         13},
  {"Cvarlinmds",      ( DL_FUNC ) &Cvarlinmds,         13},
  {"Cvarlinwgtmds",      ( DL_FUNC ) &Cvarlinwgtmds,         14},

  {"Cmdist",      ( DL_FUNC ) &Cmdist,         6},
  {"Cmds",      ( DL_FUNC ) &Cmds,         11},
  {"Cmdsneg",      ( DL_FUNC ) &Cmdsneg,         10},
  {"Cwgtmds",      ( DL_FUNC ) &Cwgtmds,         11},
  {"Cwgtmdsneg",      ( DL_FUNC ) &Cwgtmdsneg,         11},
  {"Cfxdmds",      ( DL_FUNC ) &Cfxdmds,         11},
  {"Cfxdmdsneg",      ( DL_FUNC ) &Cfxdmdsneg,         11},
  {"Cfxdwgtmds",      ( DL_FUNC ) &Cfxdwgtmds,         12},
  {"Cfxdwgtmdsneg",      ( DL_FUNC ) &Cfxdwgtmdsneg,         12},
  {"Cvarmds",      ( DL_FUNC ) &Cvarmds,         12},

  {"Cvarmdsneg",      ( DL_FUNC ) &Cvarmdsneg,         12},
  {"Cvarwgtmds",      ( DL_FUNC ) &Cvarwgtmds,         13},
  {"Cvarwgtmdsneg",      ( DL_FUNC ) &Cvarwgtmdsneg,         13},
  {"Cpenvarmds",      ( DL_FUNC ) &Cpenvarmds,         15},
  {"Cordmds",      ( DL_FUNC ) &Cordmds,         11},
  {"Cordwgtmds",      ( DL_FUNC ) &Cordwgtmds,         12},
  {"Cfxdordmds",      ( DL_FUNC ) &Cfxdordmds,         12},
  {"Cfxdordwgtmds",      ( DL_FUNC ) &Cfxdordwgtmds,         13},
  {"Cvarordmds",      ( DL_FUNC ) &Cvarordmds,         13},
  {"Cvarordwgtmds",      ( DL_FUNC ) &Cvarordwgtmds,         14},

  {"Cpcoa",      ( DL_FUNC ) &Cpcoa,         5},
  {"Cfastpcoa",      ( DL_FUNC ) &Cfastpcoa,         5},
  {"Cfasterpcoa",      ( DL_FUNC ) &Cfasterpcoa,         7},
  {"Crespcoa",      ( DL_FUNC ) &Crespcoa,         8},
  {"Cpowmds",      ( DL_FUNC ) &Cpowmds,         10},
  {"Cpowwgtmds",      ( DL_FUNC ) &Cpowwgtmds,         11},
  {"Cfxdpowmds",      ( DL_FUNC ) &Cfxdpowmds,         11},
  {"Cfxdpowwgtmds",      ( DL_FUNC ) &Cfxdpowwgtmds,         12},
  {"Cvarpowmds",      ( DL_FUNC ) &Cvarpowmds,         12},
  {"Cvarpowwgtmds",      ( DL_FUNC ) &Cvarpowwgtmds,         13},
  {"CRpredict",      ( DL_FUNC ) &CRpredict,         12},

  {"Crcvpenvarmds",      ( DL_FUNC ) &Crcvpenvarmds,         20},
  {"Crdop",      ( DL_FUNC ) &Crdop,         5},
  {"Cerdop",      ( DL_FUNC ) &Cerdop,         7},
  {"Csplmds",      ( DL_FUNC ) &Csplmds,         15},
  {"Csplwgtmds",      ( DL_FUNC ) &Csplwgtmds,         16},
  {"Cfxdsplmds",      ( DL_FUNC ) &Cfxdsplmds,         16},
  {"Cfxdsplwgtmds",      ( DL_FUNC ) &Cfxdsplwgtmds,         17},
  {"Cvarsplmds",      ( DL_FUNC ) &Cvarsplmds,         17},
  {"Cvarsplwgtmds",      ( DL_FUNC ) &Cvarsplwgtmds,         18},

  {"Csimmds1",      ( DL_FUNC ) &Csimmds1,         7},
  {"Csimmds2",      ( DL_FUNC ) &Csimmds2,         7},
  {"Csimmds3",      ( DL_FUNC ) &Csimmds3,         8},
  {"Csimfxdmds1",      ( DL_FUNC ) &Csimfxdmds1,         8},
  {"Csimfxdmds2",      ( DL_FUNC ) &Csimfxdmds2,         8},
  {"Csimfxdmds3",      ( DL_FUNC ) &Csimfxdmds3,         9},
  {"Csimmds1local",      ( DL_FUNC ) &Csimmds1local,         8},
  {"Csimmds2local",      ( DL_FUNC ) &Csimmds2local,         8},
  {"Csimmds3local",      ( DL_FUNC ) &Csimmds3local,         9},
  {"Csimfxdmds1local",      ( DL_FUNC ) &Csimfxdmds1local,         9},

  {"Csimfxdmds2local",      ( DL_FUNC ) &Csimfxdmds2local,         9},
  {"Csimfxdmds3local",      ( DL_FUNC ) &Csimfxdmds3local,         10},
  {"Csimmds2interval",      ( DL_FUNC ) &Csimmds2interval,         7},
  {"Csimfxdmds2interval",      ( DL_FUNC ) &Csimfxdmds2interval,         8},
  {"Csimmds2localinterval",      ( DL_FUNC ) &Csimmds2localinterval,         8},
  {"Csimfxdmds2localinterval",      ( DL_FUNC ) &Csimfxdmds2localinterval,         9},

  {"Csimlinmds2",      ( DL_FUNC ) &Csimlinmds2,         7},

  {"Csimwgtmds1",      ( DL_FUNC ) &Csimwgtmds1,         8},
  {"Csimwgtmds2",      ( DL_FUNC ) &Csimwgtmds2,         8},
  {"Csimwgtmds3",      ( DL_FUNC ) &Csimwgtmds3,         9},
  {"Csimfxdwgtmds1",      ( DL_FUNC ) &Csimfxdwgtmds1,         9},

  {"Csimfxdwgtmds2",      ( DL_FUNC ) &Csimfxdwgtmds2,         9},
  {"Csimfxdwgtmds3",      ( DL_FUNC ) &Csimfxdwgtmds3,         10},
  {"Csimwgtmds1local",      ( DL_FUNC ) &Csimwgtmds1local,         9},
  {"Csimwgtmds2local",      ( DL_FUNC ) &Csimwgtmds2local,         9},
  {"Csimwgtmds3local",      ( DL_FUNC ) &Csimwgtmds3local,         10},
  {"Csimfxdwgtmds1local",      ( DL_FUNC ) &Csimfxdwgtmds1local,         10},
  {"Csimfxdwgtmds2local",      ( DL_FUNC ) &Csimfxdwgtmds2local,         10},
  {"Csimfxdwgtmds3local",      ( DL_FUNC ) &Csimfxdwgtmds3local,         11},
  {"Csimwgtmds2interval",      ( DL_FUNC ) &Csimwgtmds2interval,         8},
  {"Csimfxdwgtmds2interval",      ( DL_FUNC ) &Csimfxdwgtmds2interval,         9},

  {"Csimwgtmds2localinterval",      ( DL_FUNC ) &Csimwgtmds2localinterval,         9},
  {"Csimfxdwgtmds2localinterval",      ( DL_FUNC ) &Csimfxdwgtmds2localinterval,         10},

  {"Csimlmkmds3",      ( DL_FUNC ) &Csimlmkmds3,         7},

  {NULL, NULL, 0}
};

void R_init_fmds( DllInfo *dll )
{
  R_registerRoutines( dll, CEntries, NULL, NULL, NULL );
  R_useDynamicSymbols( dll, FALSE );
}
