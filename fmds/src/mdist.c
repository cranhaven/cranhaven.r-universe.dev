//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

static void normalize( const size_t n, double** d )
{
  const double ss = dssq( n * n, &d[1][1], 1 );
  const double norm = ( double )( 2 * n );
  const double c = ( isnotzero( ss ) ? sqrt( norm / ss ) : 0.0 );
  dscal( n * n, c, &d[1][1], 1 );
} // normalize

static void distnum( const size_t n, double* a, const bool scale, double** d )
{
  for ( size_t i = 1; i <= n; i++ ) {
    d[i][i] = 0.0;
    for ( size_t j = 1; j < i; j++ ) {
      const double work = fabs( a[i] - a[j] );
      d[i][j] = d[j][i] = work;
    }
  }
  if ( scale == true ) normalize( n, d );
} // distnum

static void distord( const size_t n, double* a, const bool scale, double** d )
{
  double* v = getvector( n, 0.0 );
  dcopy( n, &a[1], 1, &v[1], 1 );
  size_t* idx = getvector_t( n, 0 );
  for ( size_t i = 1; i <= n; i++ ) idx[i] = i;
  dsort( n, v, idx );
  double lastvalue = 0.0;
  double value = 0.0;
  double cumvalue = 0.0;;
  double* h = getvector( n, 0.0 );
  size_t size = 1;
  for ( size_t i = 2; i <= n; i++ ) {
    if ( isnotequal( v[i], v[i - 1] ) ) {
      value = 1.0 / ( double )( size );
      cumvalue += sqrt( lastvalue + value );
      for ( size_t j = 1; j <= size; j++ ) h[idx[i-j]] = cumvalue;
      lastvalue = value;
      size = 1;
    }
    else size++;
  }
  cumvalue += sqrt( lastvalue + 1.0 / ( double )( size ) );
  for ( size_t j = 1; j <= size; j++ ) h[idx[n-j+1]] = cumvalue;
  distnum( n, h, scale, d );
  freevector( v );
  freevector_t( idx );
  freevector( h );
} // distord

static void distnom( const size_t n, double* a, const bool scale, double** d )
{
  double* v = getvector( n, 0.0 );
  dcopy( n, &a[1], 1, &v[1], 1 );
  dsort0( n, v );
  size_t size = 1;
  v[size] = v[1];
  for ( size_t i = 2; i <= n; i++ ) {
    if ( isnotequal( v[i], v[i - 1] ) ) {
      size++;
      v[size] = v[i];
    }
  }
  double** h = getmatrix( n, size, 0.0 );
  for ( size_t j = 1; j <= size; j++ ) {
    for ( size_t i = 1; i <= n; i++ ) if ( isequal( a[i], v[j] ) ) h[i][j] = 1.0;
    const double mass = dsum( n, &h[1][j], size );
    const double alpha = 1.0 / sqrt( mass );
    dscal( n, alpha, &h[1][j], size );
  }
  euclidean1( n, size, h, d );
  if ( scale == true ) normalize( n, d );
  freevector( v );
  freematrix( h );
} // distnom

int mdist( const size_t n, const size_t m, double** a, int* level, const bool scale, double** d )
{
  int retval = 0;
  double* tmpa = getvector( n, 0.0 );
  double** tmpd = getmatrix( n, n, 0.0 );
  dset( n * n, 0.0, &d[1][1], 1 );
  for ( size_t j = 1; j <= m; j++ ) {
    dcopy( n, &a[1][j], m, &tmpa[1], 1 );
    if ( level[j] == MEASUREMENTLEVEL.NOMINAL ) distnom( n, tmpa, scale, tmpd );
    else if ( level[j] == MEASUREMENTLEVEL.ORDINAL ) distord( n, tmpa, scale, tmpd );
    else if ( level[j] == MEASUREMENTLEVEL.NUMERICAL ) distnum( n, tmpa, scale, tmpd );
    for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) {
      const double work = tmpd[i][j];
      d[i][j] += work * work;
    }
  }
  for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) d[j][i] = d[i][j] = sqrt( d[i][j] );
  if ( scale == true ) normalize( n, d );
  freevector( tmpa );
  freematrix( tmpd );
  return( retval );
} // mdist

void Cmdist( int* rn, int* rm, double* ra, int* rlevel, int* rscale, double* rd )
// Function Cmixed() returns Euclidean distances for mixed level variables
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  double** a = getmatrix( n, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) a[i][j] = ra[k];
  int* level = getivector( m, 0 );
  const bool scale = ( *rscale != 0 );
  for ( size_t j = 1, k = 0; j <= m; j++, k++ ) level[j] = rlevel[k];
  double** d = getmatrix( n, n, 0.0 );

  // run function
  int retval = mdist( n, m, a, level, scale, d );

  // transfer to R
  if ( retval == 0 ) for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];

  // de-allocate memory
  freematrix( a );
  freeivector( level );
  freematrix( d );

} // Cmdist

void mdist_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test mdist at ", dt );
  printstring( "\n", "==============================================================\n" );

  randomize( &seed );
 
  size_t n = 0;
  size_t m = 0; 
  double** data = readmatrix( "tortula.dat", &n, &m );
  double** w = getmatrix( n, n, 1.0 );
  dset( n, 0.0, &w[1][1], n + 1 );
  int* level = getivector( m, 0 );
  double** d1 = getmatrix( n, n, 0.0 );
  double** d2 = getmatrix( n, n, 0.0 );

  size_t tm = setstarttime();
  euclidean1( n, m, data, d1 );
  printscalar( "elapsed for euclidean1", getelapsedtime( tm ) );

  tm = setstarttime();
  mdist( n, m, data, level, false, d2 );
  printscalar( "elapsed for mdist", getelapsedtime( tm ) );
  printscalar( "n-stress", nstress( n, d1, d2, w ) );

//   double** xd = getmatrix( n, p, 0.0 );
//   for ( size_t j = 1; j <= n; j++ ) for ( size_t k = 1; k <= p; k++ ) xd[j][k] = nextdouble( );
//   double** rd = getmatrix( n, n, 0.0 );

//   size_t tmd = setstarttime();
//   euclidean1( n, p, xd, rd );
//   printf( "elapsed for 1 = %f\n", getelapsedtime( tmd ) );

  freematrix( data );
  freematrix( w );
  freeivector( level );
  freematrix( d1 );
  freematrix( d2 );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test mdist at ", dt );
  printstring( "\n", "==============================================================\n" );
}
