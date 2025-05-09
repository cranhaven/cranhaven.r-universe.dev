//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

double addconst( const size_t n, double** const a )
// on input, a is a symmetrical matrix of which the diagonal is ignored
// return additive constant, such that d = d + c is Euclidean
// ref: F. Cailliez (1983). The Analytic Solution of the Additive Constant Problem. Psychometrika, 48(2).
{
  // initialize
  double ac = 0.0;

  // fast return
  if ( n <= 2 ) return 0;

  // allocate memory
  double** aa = getmatrix( n, n, 0.0 );
  double** b = getmatrix( n + n, n + n, 0.0 );

  // add -i
  for ( size_t i = 1; i <= n; i++ ) b[n + i][i] = -1.0;

  // add 2wd2 matrix
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) aa[i][j] = pow( a[i][j], 2 );
  doublecenter( n, aa );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) b[i][n + j] = -1.0 * aa[i][j];

  // add -4wd.5 matrix
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) aa[i][j] = a[i][j];
  doublecenter( n, aa );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) b[n + i][n + j] = 2.0 * aa[i][j];

  // eigenvalue decomposition: constant according to Cailliez
  int result = max_eigen_hessenberg( n + n, b, &ac );
  if ( 0 != result ) {
    result = max_eigen_186( n + n, b, &ac );
    if ( 0 != result ) ac = DBL_MAX;
  }

  // free memory
  freematrix( aa );
  freematrix( b );

  return ac;
} // addconst

void Caddconst( int* rn, double* rdelta, double* rac )
// Function Caddconst() returns additive constant based on Cailliez.
{
  // transfer to C
  size_t n = *rn;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];

  // run function
  const double ac = addconst( n, delta );

  // transfer to R
  ( *rac ) = ac;

  // de-allocate memory
  freematrix( delta );

} // Caddconst

double fastaddconst( const size_t n, double** const a )
// on input, a is a symmetrical matrix of which the diagonal is ignored
// return additive constant, such that d = d + c is Euclidean
// ref: F. Cailliez (1983). The Analytic Solution of the Additive Constant Problem. Psychometrika, 48(2).
{
  // constants
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08

  // initialize
  int result = 0;
  double ac = 0.0;

  // fast return
  if ( n <= 2 ) return 0;

  // allocate memory
  double** aa = getmatrix( n, n, 0.0 );
  double** b = getmatrix( n + n, n + n, 0.0 );

  // add -i
  for ( size_t i = 1; i <= n; i++ ) b[n + i][i] = -1.0;

  // add 2wd2 matrix
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) aa[i][j] = pow( a[i][j], 2 );
  doublecenter( n, aa );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) b[i][n + j] = -1.0 * aa[i][j];

  // add -4wd.5 matrix
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) aa[i][j] = a[i][j];
  doublecenter( n, aa );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) b[n + i][n + j] = 2.0 * aa[i][j];

  // eigenvalue decomposition: constant according to Cailliez
  // recalculate maximum eigenvalue if zero after Arnoldi
  const size_t bound = 50 + 2 * ( size_t )( sqrt( (double )( n ) ) );
  if ( n > bound ) result = max_eigen_arnoldi( n + n, b, 0, &ac );
  if ( 0 != result || ac < TOL ) {
    max_eigen_hessenberg( n + n, b, &ac );
    if ( 0 != result ) {
      result = max_eigen_186( n + n, b, &ac );
      if ( 0 != result ) ac = DBL_MAX;
    }
  }

  // free memory
  freematrix( aa );
  freematrix( b );

  return ac;
} // fastaddconst

void Cfastaddconst( int* rn, double* rdelta, double* rac )
// Function Caddconst() returns additive constant based on Cailliez.
{
  // transfer to C
  size_t n = *rn;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];

  // run function
  const double ac = fastaddconst( n, delta );

  // transfer to R
  ( *rac ) = ac;

  // de-allocate memory
  freematrix( delta );

} // Cfastaddconst

void addconst_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test addconst at ", dt );
  printstring( "\n", "==============================================================\n" );

  randomize( &seed );
 
  int n = 500;
  int p = 2;

  double** delta = getmatrix( n, n, 0.0 );
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) z[i][k] = nextdouble();
  euclidean1( n, p, z, delta );

  size_t tm = setstarttime();
  double ac = addconst( n, delta );
  printscalar( "elapsed for addconst", getelapsedtime( tm ) );
  printscalar( "addconst", ac );

  tm = setstarttime();
  Caddconst( &n, &delta[1][1], &ac );
  printscalar( "elapsed for Caddconst", getelapsedtime( tm ) );
  printscalar( "Caddconst", ac );

  tm = setstarttime();
  ac = fastaddconst( n, delta );
  printscalar( "elapsed for fastaddconst", getelapsedtime( tm ) );
  printscalar( "fastaddconst", ac );

  tm = setstarttime();
  Cfastaddconst( &n, &delta[1][1], &ac );
  printscalar( "elapsed for Cfastaddconst", getelapsedtime( tm ) );
  printscalar( "Cfastaddconst", ac );

  for ( size_t test = 10; test <= 500; test += 10 ) {

    double** d = getmatrix( test, test, 0.0 );
    double** x = getmatrix( test, p, 0.0 );

    for ( size_t i = 1; i <= test; i++ ) for ( size_t k = 1; k <= p; k++ ) x[i][k] = 100.0 * nextdouble();
    euclidean1( test, p, x, d );
    double mind = 100.0;
    for ( size_t i = 1; i <= test; i++ ) for ( size_t j = 1; j <= test; j++ ) if ( i != j ) mind = fmin( mind, d[i][j] );
    for ( size_t i = 1; i <= test; i++ ) for ( size_t j = 1; j <= test; j++ ) if ( i != j ) d[i][j] = d[i][j] - mind;

    tm = setstarttime();
    double ac1 = addconst( test, d );
    double d1 = getelapsedtime( tm );

    tm = setstarttime();
    double ac2 = fastaddconst( test, d );
    double d2 = getelapsedtime( tm );

    #ifdef R
      Rprintf( "%4zu; timing %8.6f, %8.6f; difference %8.6f, %8.6f;\n", test, d1, d2, ac1 - mind, ac2 - mind );
    #else
      printf( "%4llu; timing %8.6f, %8.6f; difference %8.6f, %8.6f;\n", test, d1, d2, ac1 - mind, ac2 - mind );
    #endif

    freematrix( d );
    freematrix( x );
  }

  freematrix( delta );
  freematrix( z );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test addconst at ", dt );
  printstring( "\n", "==============================================================\n" );
}
