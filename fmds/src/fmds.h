//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#ifndef FMDS_H
#define FMDS_H

#define R

#include "flib.h"

// addconst.c
extern double addconst( const size_t n, double** delta );
extern double fastaddconst( const size_t n, double** delta );
extern void addconst_unittest( long seed );

// pcoa.c
extern int pcoa( const size_t n, double** d, const size_t p, const double ac, double** z );
extern int fastpcoa( const size_t n, double** d, const size_t p, const double ac, double** z );
extern int respcoa( const size_t n, double** d, const size_t h, double** q, const size_t p, const double ac, double** c, double** z );
extern void pcoa_unittest( long seed );

// mdist.c
extern int mdist( const size_t n, const size_t m, double** a, int* level, const bool scale, double** d );
extern void mdist_unittest( long seed );

// rdop.c
extern void Crdop( int* rn, double* rdelta, int* rk, double* rlambda, double* rscores );
extern void Cerdop( int* rn, double* rdelta, int* rk, double* rlambda, double* rw, double* alpha, double* beta );
extern void rdop_unittest( long seed );

// mds.c
extern double mds( const size_t n, double** delta, const size_t p, double** z, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double mdsneg( const size_t n, double **delta, const size_t p, double **z, double **d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t *lastiter, double *lastdif, const bool echo );
extern double wgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double wgtmdsneg( const size_t n, double **delta, double **w, const size_t p, double **z, double **d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t *lastiter, double *lastdif, const bool echo );
extern double fxdmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdmdsneg( const size_t n, double **delta, const size_t p, double **z, int **fz, double **d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t *lastiter, double *lastdif, const bool echo );
extern double fxdwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdwgtmdsneg( const size_t n, double **delta, double **w, const size_t p, double **z, int **fz, double **d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t *lastiter, double *lastdif, const bool echo );
extern double varmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varmdsneg( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varwgtmdsneg( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double penvarmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, const double lambda, const double alpha, const bool grouped, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern void mds_unittest( long seed );

// linmds.c
extern double linmds( const size_t n, double** delta, const size_t p, double** z, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double linwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdlinmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdlinwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varlinmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varlinwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern void linmds_unittest( long seed );

// powmds.c
extern double powmds( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double powwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdpowmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdpowwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varpowmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varpowwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern void powmds_unittest( long seed );

// bcxmds.c
extern double bcxmds( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double bcxwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdbcxmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdbcxwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varbcxmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varbcxwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern void bcxmds_unittest( long seed );

// splmds.c
extern double splmds( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double splwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdsplmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdsplwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varsplmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varsplwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern void splmds_unittest( long seed );

// ordmds.c
extern double ordmds( const size_t n, double** delta, const size_t p, double** z, double** d, const int approach, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double ordwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const int approach, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdordmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const int approach, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double fxdordwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const int approach, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varordmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const int approach, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double varordwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const int approach, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern void ordmds_unittest( long seed );

// fastermds.c
extern void Cfastermds( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfastermdsneg( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfasterfxdmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfasterwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void Cfasterordmds( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void fastermds_unittest( long seed );

// simmds.c
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
extern void simmds_unittest( long seed );

// explain.c
extern double explain( const size_t n, const size_t p, double** z, double* q, double* w, double* a, double* d, const int level, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern void explain_unittest( long seed );

// experimental stuff
extern double fmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, const size_t h, double** q, double** b, double** d, const bool anchor, const bool power, const size_t degree, const size_t ninner, double* iknots, const int knotstype, const int approach, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double stochasticstress( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnsteps, int* rseed );
extern void CRsuperfastmds1( int* rn, double* rdist, int* rp, double* rz, int* rminiter, double* rminrate, double* rfvalue, int* seed, int* recho );
extern void CRsuperfastmds2( int* rn, double* rdelta, int* rp, double* rz, int* rminiter, double* rminrate, double* rfvalue, int* seed, int* recho );
extern void CRsuperfastmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rminiter, double* rminrate, double* rfvalue, int* seed, int* recho );
extern void CRmegafastmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rsubsize, int* rnsteps, double* rminrate, int* rseed );
extern void CRultrafastmds1( int* rn, double* rdist, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void CRultrafastmds2( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void CRultrafastmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern void CRultrafastmds4( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed );
extern int run_SQuaD_MDS( double* Xhd, int N, int M, double* Xld_flat, int n_iter );
extern void spe( double *coord, int *nobs, int *ndim, int *edim, int *ncycle, double* minlambda, int *rseed, double *x );
extern void gma( int *rn, int *nm, double *rx, int *rp, double *rz, int *ncycle, int *rseed );
extern double predict( const size_t n, const size_t p, double** z, double* delta, double* w, double* x, double* d, const int level, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double mamds( char* infilename );
extern void submds( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, const size_t nrepls, size_t* index, double** stress, double** points );
extern void dimensionspath( const size_t ndims, size_t* dims, double* error, double* stderror, const size_t NREPLS, const size_t NFOLDS, const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, const bool echo );
extern void rcvpenvarmds( const size_t NREPEATS, const size_t NFOLDS, const size_t NLAMBDA, double* lambda, const double alpha, const bool grouped,
                          const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, const bool echo,
                          double* mserror, double* stddev, double* stderror );

#endif
