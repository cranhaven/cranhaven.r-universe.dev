//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#ifndef FLIB_H
#define FLIB_H

#define R

#include <stdbool.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <limits.h>


#ifdef R
  #include "R.h"
#endif

struct knotstype_struct { int NONE; int USERPROVIDED; int INTERVAL; int PERCENTILE; int MIDPERCENTILE; };
static const struct knotstype_struct KNOTSTYPE = { .NONE = 0, .USERPROVIDED = 1, .INTERVAL = 2, .PERCENTILE = 3, .MIDPERCENTILE = 4 };

extern size_t min_t( const size_t a, const size_t b );
extern size_t max_t( const size_t a, const size_t b );
extern bool isnull( double** ptr );
extern bool isnotnull( double** ptr );
extern bool isequal( const double d1, const double d2 );
extern bool iszero( const double x );
extern bool isnotzero( const double x );
extern bool isnotequal( const double d1, const double d2 );
extern double plogis( const double x );

extern void randomize( long *seed );
extern double nextdouble( void );
extern double stdnormal( void );
extern void permutate_t( const size_t n, size_t* a );
extern size_t binarysearch( const size_t n, double* x, const double p );

extern void set( const size_t n, const double b, double* const a, const size_t inca );
extern void copy( const size_t n, const double* const a, const size_t inca, double* const b, const size_t incb );
extern void copyall( const size_t n, const double* const a, double* const b );
extern double sum( const size_t n, const double* const a, const size_t inca );
extern double wsum( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw );
extern void scal( const size_t n, const double c, double* const a, const size_t inca );
extern double dot( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb );
extern double ssq( const size_t n, const double* const a, const size_t inca );
extern double wssq( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw );
extern void axpy( const size_t n, const double c, double* a, const size_t inca, double* b, const size_t incb );
extern double wnrm2( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw );
extern void gemm( const bool transa, const bool transb, const size_t nrc, const size_t ncc, const size_t nab, const double alpha, double** const a, double** const b, const double beta, double** const c );
extern void euclidean1( const size_t n, const size_t p, double** a, double** const r );
extern void euclidean2( const size_t n, const size_t p, double** a, const size_t m, double** b, double** const r );
extern void squaredeuclidean1( const size_t n, const size_t p, double** a, double** const r );
extern void squaredeuclidean2( const size_t n, const size_t p, double** a, const size_t m, double** b, double** const r );
extern bool symmetric( const size_t n, double** x );
extern bool anyequal( const size_t n, double* a, const size_t inca, const double c );
extern bool anynotequal( const size_t n, double* a, const size_t inca, const double c );
extern bool anygreater( const size_t n, double* a, const size_t inca, const double c );
extern bool anysmaller( const size_t n, double* a, const size_t inca, const double c );
extern bool anygreaterequal( const size_t n, double* a, const size_t inca, const double c );
extern bool anysmallerequal( const size_t n, double* a, const size_t inca, const double c );
extern bool allequal( const size_t n, double* a, const size_t inca, const double c );
extern bool allnotequal( const size_t n, double* a, const size_t inca, const double c );
extern bool allgreater( const size_t n, double* a, const size_t inca, const double c );
extern bool allsmaller( const size_t n, double* a, const size_t inca, const double c );
extern bool allgreaterequal( const size_t n, double* a, const size_t inca, const double c );
extern bool allsmallerequal( const size_t n, double* a, const size_t inca, const double c );

extern bool* getbvector( const size_t nr, const bool c );
extern void freebvector( bool* a );
extern int* getivector( const size_t nr, const int c );
extern void freeivector( int* a );
extern size_t* getvector_t( const size_t nr, const size_t c );
extern void freevector_t( size_t* a );
extern double* getvector( const size_t nr, const double c );
extern void freevector( double* a );

extern int** getimatrix( const size_t nr, const size_t nc, const int c );
extern void freeimatrix( int** a );
extern size_t** getmatrix_t( const size_t nr, const size_t nc, const size_t c );
extern void freematrix_t( size_t** a );extern double** 
getmatrix( const size_t nr, const size_t nc, const double c );																			  
extern void freematrix( double** a );

extern int inverse( const size_t n, double** a );
extern int evdcmp( const size_t n, double** vecs, double* vals );
extern int jacobi( const size_t n, double** vecs, double* vals, const size_t k );

extern int nnls( const size_t m, const size_t n, double** const ca, double* const x, double* const cb, size_t *MAXITER, double *FCRIT );
extern int nnals( const size_t n, const size_t m, double** x, double* b, double* y, size_t *MAXITER, double *FCRIT );
extern int fastnnls( const size_t n, const size_t m, double** x, double* b, double* y, size_t *MAXITER, double *FCRIT );
extern int nnccd( const size_t n, const size_t m, double** x, double *b, double *y, size_t *MAXITER, double *FCRIT );
extern int ccdu( const size_t n, const size_t m, double** x, double *b, double *y, size_t *MAXITER, double *FCRIT );

extern void nnlinear( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r );
extern size_t setindices( const bool symmetric, const size_t n, double** delta, double** w, size_t* index, size_t* ntb, size_t* tbl );
extern void ordinal1( const bool symmetric, const size_t n, double** d, const double mconst, double** w, const size_t count, size_t* index, const size_t ntb, size_t* tbl, double** gamma );
extern void ordinal2( const bool symmetric, const size_t n, double** d, const double mconst, double** w, const size_t count, size_t* index, const size_t ntb, size_t* tbl, double** gamma );
extern double** polynomialbasis( const bool symmetric, const size_t n, double** delta, double** w, const size_t ninner, double* iknots, const size_t degree, const bool anchor, const int knotstype, const bool monotone );
extern void polynomialcoefficients( const bool symmetric, const size_t n, double** d, double** w, const size_t m, double** base, double* b, const double mconst, double** gamma );

extern void rotation( const size_t n, const size_t p, double** z, double** r, double* ev );
extern void weightedrotation( const size_t n, const size_t p, double** z, double* w, double** r, double* ev );

extern void rotate( const size_t n, const size_t p, double** z );
extern void weightedrotate( const size_t n, const size_t p, double** z, double* w );
extern void rotateplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1 );
extern void weightedrotateplus( const size_t n, const size_t p, double** z, double* w, const size_t n1, double** z1 );
extern void rotateplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2 );
extern void rotateplusplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2, const size_t n3, double** z3 );
extern void printerror( char* title );
extern void printpass( const size_t nr );

extern void printscalar_t( char* title, const size_t a );
extern void printscalar( char* title, const double a );
extern void printivector( char* title, const size_t n, size_t* a );
extern void printvector( char* title, const size_t n, double* a );
extern void printmatrix( char* title, const size_t n, const size_t m, double** a );

extern void echoprogress( const size_t iter, const double fold, const double fhalf, const double fnew );

extern double** readmatrix( char* infilename, size_t *n, size_t *m );
extern char* getdatetime( void );
extern time_t setstarttime( void );
extern double getelapsedtime( const time_t starttime );
#endif
