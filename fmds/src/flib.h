//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
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
#include <ctype.h>
#include <stdint.h>
#include <inttypes.h>

#ifdef R
  #include "R.h"
#endif

#ifdef iszero
  #undef iszero  // use own iszero
#endif

struct measurement_struct { int NUMERICAL; int ORDINAL; int NOMINAL; };
static const struct measurement_struct MEASUREMENTLEVEL = { .NUMERICAL = 0, .ORDINAL = 1, .NOMINAL = 2 };
struct transformation_struct { int ABSOLUTE; int RATIO; int LINEAR; int POWER; int BOXCOX; int SPLINE; int ORDINAL; int NOMINAL; };
static const struct transformation_struct TRANSFORMATIONLEVEL = { .ABSOLUTE = 0, .RATIO = 1, .LINEAR = 2, .POWER = 3, .BOXCOX = 4, .SPLINE = 5, .ORDINAL = 6, .NOMINAL = 7 };
struct knotstype_struct { int NONE; int UNIFORM; int PERCENTILE; int MIDPERCENTILE; };
static const struct knotstype_struct KNOTSTYPE = { .NONE = 0, .UNIFORM = 1, .PERCENTILE = 2, .MIDPERCENTILE = 3 };
static const double SYSMIS = -1.0 * DBL_MAX;

extern size_t min_t( const size_t a, const size_t b );
extern size_t max_t( const size_t a, const size_t b );
extern bool isnull( double** ptr );
extern bool isnotnull( double** ptr );
extern bool isequal( const double d1, const double d2 );
extern bool iszero( const double x );
extern bool isnotzero( const double x );
extern bool isalmostzero( const double x );
extern bool isnotalmostzero( const double x );
extern bool isnotequal( const double d1, const double d2 );
extern bool issysmis( const double a );
extern bool isnotsysmis( const double a );
extern double plogis( const double x );

extern void randomize( long *seed );
extern size_t nextsize_t( void );
extern double nextdouble( void );

// extern void pcg32_srandom( uint64_t seed, uint64_t seq );
// extern uint32_t pcg32_random( );
// extern double pcg32_next( );

extern size_t duniform( const size_t n1, const size_t n2 );
extern double stdnormal( void );
extern double stdlognormal( void );
extern void permutate_t( const size_t n, size_t* a );
extern void draw_t( const size_t n, size_t* a, const size_t m, size_t* b, const bool replace );
extern double choose( double n, double k );
extern double combination( long n, long k );
extern size_t expecteddraws( const size_t n, const size_t m );
extern size_t binarysearch( const size_t n, double* x, const double p );
extern size_t wheel( const size_t n, double* cump, const double r );
extern void randomZ( const size_t n, const size_t p, double** z, long seed );
extern double randomDelta( const size_t n, const size_t m, int* vdist, double* vssq, const int edist, const double epr, const long seed, double** delta );

extern double dmin( const size_t n, const double* const a, const size_t inca );
extern double dmax( const size_t n, const double* const a, const size_t inca );
extern void dset( const size_t n, const double b, double* const a, const size_t inca );
extern void dcopy( const size_t n, const double* const a, const size_t inca, double* const b, const size_t incb );
extern double dsum( const size_t n, const double* const a, const size_t inca );
extern double dwsum( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw );
extern void dscal( const size_t n, const double c, double* const a, const size_t inca );
extern double ddot( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb );
extern double dwdot( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw );
extern double dssq( const size_t n, const double* const a, const size_t inca );
extern double dwssq( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw );
extern void daxpy( const size_t n, const double c, double* a, const size_t inca, double* b, const size_t incb );
extern double dsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb );
extern double drsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb );
extern double dwsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw );
extern double drwsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw );
extern void dgemv( const bool transa, const size_t nra, const size_t nca, const double alpha, double** const a, double* const b, const double beta, double* const c );
extern void dgemm( const bool transa, const bool transb, const size_t nrc, const size_t ncc, const size_t nab, const double alpha, double** const a, double** const b, const double beta, double** const c );
extern double variance( const size_t n, const double* const a, const size_t inca );
extern double stddev( const size_t n, const double* const a, const size_t inca );
extern void doublecenter( const size_t n, double** a );
extern double fdist1( const size_t p, double* x, double* y );
extern double fdist( size_t n, double* x, double* y, const size_t inc );
extern void euclidean1( const size_t n, const size_t p, double** a, double** const r );
extern void euclidean2( const size_t n, const size_t p, double** a, const size_t m, double** b, double** const r );
extern void squaredeuclidean1( const size_t n, const size_t p, double** a, double** const r );
extern void squaredeuclidean2( const size_t n, const size_t p, double** a, const size_t m, double** b, double** const r );
extern void pdist( const size_t n, double* d, double* w, double* r );
extern void dsort0( const size_t n, double* const a );
extern void dsort( const size_t n, double* const a, size_t* const r );
extern void dsort2( const size_t n, double* const a, double* const b, size_t* const r );
extern void sort_t( const size_t n, size_t* const a );
extern void sort_t2( const size_t n, size_t* const a, double* const b );
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
extern double scale( const size_t n, const size_t m, double** a, double** b );
extern void center( const size_t n, const size_t p, double** z );
extern double nstress( const size_t n, double** delta, double** d, double** w );
extern double pearson( const size_t n, double* a, double* b, double* w );
extern double covariance( const size_t n, const double* const x, const size_t incx, const double* const y, const size_t incy );

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
extern void freematrix_t( size_t** a );
extern double** getmatrix( const size_t nr, const size_t nc, const double c );
extern void freematrix( double** a );
extern double ***gettensor( const size_t ns, const size_t nr, const size_t nc, const double c );
extern void freetensor( double*** a );

extern int inverse( const size_t n, double** a );
extern int mpinverse( const size_t n, const size_t m, double **a, double **inva );
extern int evdcmp( const size_t n, double** vecs, double* vals );
extern int evdcmp_lanczos( const size_t n, double** a, double** vecs, double* vals, const size_t k );
extern int svdcmp( const size_t n, const size_t m, double** const a, double** const u, double* w, double** const v );
extern int solve( const size_t n, double** a, double* b );

extern int max_eigen_hessenberg( const size_t n, double** const a, double* mx );
extern int max_eigen_186( const size_t n, double** const a, double* mx );
extern int max_eigen_arnoldi( const size_t n, double** const a, const size_t nvecs, double* mx );

extern void nnintercept( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r );
extern void nnslope( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r );
extern void nnlinear( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r );
extern void nnpower( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, const bool use_a, const bool use_b, double** r );
extern void nnboxcox( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r );
extern void monotone( const size_t n, double* x, double* w );
extern size_t setindices( const bool symmetric, const size_t n, double** delta, double** w, size_t* index, size_t* ntb, size_t* tbl );
extern void ordinal1( const bool symmetric, const size_t n, double** d, const double mconst, double** w, const size_t count, size_t* index, const size_t ntb, size_t* tbl, double** gamma );
extern void ordinal2( const bool symmetric, const size_t n, double** d, const double mconst, double** w, const size_t count, size_t* index, const size_t ntb, size_t* tbl, double** gamma );
extern double** polynomialbasis( const bool symmetric, const size_t n, double** delta, double** w, const size_t ninner, double* iknots, const size_t degree, const bool anchor, const int knotstype, const bool monotone ); 
extern void polynomialcoefficients( const bool symmetric, const size_t n, double** d, double** w, const size_t m, double** base, double** bstbs, double* b, const double mconst, double** gamma );

extern double procrustes( const size_t n, const size_t p, double** a, double** b, double** res );
extern void varimaxrotation( const size_t n, const size_t m, double** x, double** loadings, double** rotation );

extern void rotate( const size_t n, const size_t p, double** z );
extern void weightedrotate( const size_t n, const size_t p, double** z, double* w );
extern void rotateplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1 );
extern void weightedrotateplus( const size_t n, const size_t p, double** z, double* w, const size_t n1, double** z1 );
extern void rotateplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2 );
extern void rotateplusplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2, const size_t n3, double** z3 );

extern size_t pairwiseinterchange( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER );
extern size_t objectinsertion( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER );
extern size_t objectoverlay( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER );
extern double rawstress( const size_t n, double** delta, const size_t p, double** z, double** d );
extern double WRCWG( const size_t n, double** delta, const size_t p, double** z );
extern void maxmin( const size_t n, double** const d, const size_t nlm, size_t* lm );
extern double lincolnpetersen( const size_t n, double* insample, const double fcrit );
extern double schnabel( const size_t n, double* catch, double* recaps, double* newmarks );
extern double chapman( const size_t n, double* catch, double* recaps, double* newmarks );
extern double schumachereschmeyer( const size_t n, double* catch, double* recaps, double* newmarks );

extern void pseudoconfidenceintervals( const size_t n, double** delta );
extern void jackknifeconfidenceintervals( const size_t n, double** delta );
extern void bootstrapconfidenceintervals( const size_t n, double** delta );
extern void permutationconfidenceintervals( const size_t n, double** delta );
extern void subsampleconfidenceintervals( const size_t n, double** delta );
extern double ciellipse( const size_t n, const size_t p, double** x, const double ci, double* c, double* r, double* a );

extern void printerror( char* title );
extern void printpass( const size_t nr );
extern void printstring( char* title, char* s );

extern void printscalar_t( char* title, const size_t a );
extern void printscalar( char* title, const double a );
extern void printivector( char* title, const size_t n, size_t* a );
extern void printvector( char* title, const size_t n, double* a );
extern void printmatrix( char* title, const size_t n, const size_t m, double** a );

extern void echoprogress( const size_t iter, const double fold, const double fhalf, const double fnew );

extern void writematrix( char* name, const size_t n, const size_t m, double** a );

extern double** readmatrix( char* infilename, size_t *n, size_t *m );
extern char* getdatetime(void);
extern size_t setstarttime( void );
extern double getelapsedtime( const size_t starttime );

typedef char char32[32];
typedef char char1024[1024];
typedef struct hash {
  char32 key;
  char1024 value;
  size_t vecsize;
  double* vec;
} hashmap;

extern hashmap* gethashmap( const size_t mapsize );
extern void freehashmap( const size_t mapsize, hashmap* map );
extern hashmap* readhashmap( char* infilename, size_t* mapsize );
extern void writehashmap( char* outfilename, const size_t mapsize, hashmap* map );
extern void displayhashmap( const size_t mapsize, hashmap* map );

extern char* gethashmapstring( const size_t mapsize, hashmap* map, char32 key, char1024* defchar );
extern int gethashmapint( const size_t mapsize, hashmap* map, char* key, const int defint );
extern size_t gethashmapsize_t( const size_t mapsize, hashmap* map, char* key, const size_t defsize_t );
extern double gethashmapdouble( const size_t mapsize, hashmap* map, char32 key, const double defdouble );
extern int* gethashmapvectorint( const size_t mapsize, hashmap* map, char32 key );
extern size_t* gethashmapvectorsize_t( const size_t mapsize, hashmap* map, char32 key );
extern double* gethashmapvectordouble( const size_t mapsize, hashmap* map, char32 key );

extern void sethashmapstring( const size_t mapsize, hashmap* map, char32 key, const char1024 value );
extern void sethashmapint( const size_t mapsize, hashmap* map, char32 key, const int value );
extern void sethashmapsize_t( const size_t mapsize, hashmap* map, char32 key, const size_t value );
extern void sethashmapdouble( const size_t mapsize, hashmap* map, char32 key, const double value );


extern void flib_unittest( long seed );

#endif
