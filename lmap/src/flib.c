//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#include "flib.h"
#define R

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// scalar functions
//

size_t min_t( const size_t a, const size_t b ) {
  return( a < b ? a : b );
}

size_t max_t( const size_t a, const size_t b ) {
  return( a > b ? a : b );
}

bool isnull( double** ptr )
{
  return ( ptr == NULL );
} // isnull

bool isnotnull( double** ptr )
{
  return ( ptr != NULL );
} // isnotnull

bool isequal( const double d1, const double d2 )
// eps hold the minimum distance between the values
// that will be considered as the numbers are equal
// considering the magnitude of the numbers
{
  const double EPS = DBL_EPSILON;
  const double eps1 = fabs( d1 );
  const double eps2 = fabs( d2 );
  double eps = ( eps1 > eps2 ) ? eps1 : eps2;
  if ( eps == 0.0 ) return true; // check for both to be zero
  eps *= EPS;
  return ( fabs( d1 - d2 ) < eps );
} // isequal

bool isnotequal( const double d1, const double d2 )
// eps hold the minimum distance between the values
// that will be considered as the numbers are equal
// considering the magnitude of the numbers
{
  const double EPS = DBL_EPSILON;
  const double eps1 = fabsl( d1 );
  const double eps2 = fabsl( d2 );
  double eps = ( eps1 > eps2 ) ? eps1 : eps2;
  if ( eps == 0.0 ) return false; // check for both to be zero
  eps *= EPS;
  return ( fabsl( d1 - d2 ) >= eps );
} // isnotequal

bool iszero( const double x )
{
  return( ( x < DBL_EPSILON ) && ( x > -DBL_EPSILON ) );
} // iszero

bool isnotzero( const double x )
{
  return( ( x > DBL_EPSILON ) || ( x < -DBL_EPSILON ) );
}
double sign( const double r, const double s )
// returns r with sign of s
{
  if ( s == 0.0 ) return r;
  else return ( s < 0.0 ) ? -r : r;
} // sign

double pythag( double x, double y )
// return sqrt( x * x + y * y ) without problems
{
  const double xabs = fabs( x );
  const double yabs = fabs( y );
  const double w = fmax( xabs, yabs );
  const double z = fmin( xabs, yabs );
  if ( iszero( z ) ) return( w );
  else {
    const double work = z / w;
    return( w * sqrtl( 1.0 + work * work ) );
  }
} // pythag

double plogis( const double x )
{
  const double expx = expl( x );
  return( expx / ( 1.0 + expx ) );
} // plogis

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// random functions
//

static size_t timeseed(void)
{
  time_t t = time( NULL );
  clock_t c = clock( );
  static unsigned long differ = 0; // guarantee time-based seeds will change
  size_t h1 = 0;
  unsigned char* p = ( unsigned char* ) & t;
  for ( size_t i = 0; i < sizeof( t ); ++i ) {
    h1 *= 255 + 2;
    h1 += p[i];
  }
  unsigned long h2 = 0;
  p = ( unsigned char* ) & c;
  for ( size_t j = 0; j < sizeof( c ); ++j ) {
    h2 *= 255 + 2;
    h2 += p[j];
  }
  return ( h1 + differ++ ) ^ h2;
} // timeseed

static size_t xseed;

static size_t nextxseed(void)
{
  size_t z = ( xseed += 0x9e3779b97f4a7c15 );
  z = ( z ^ ( z >> 30 ) ) * 0xbf58476d1ce4e5b9;
  z = ( z ^ ( z >> 27 ) ) * 0x94d049bb133111eb;
  return z ^ ( z >> 31 );
} // nextxseed

static size_t xseeds[4];

void randomize( long *seed )
{
  if ( *seed == 0 ) *seed = ( timeseed( ) % LONG_MAX );
  xseed = ( size_t )( *seed );
  for ( size_t i = 0; i < 4; i++ ) xseeds[i] = nextxseed( );
} // randomize

static inline size_t rotl( const size_t x, int k )
{
  return ( x << k ) | ( x >> ( 64 - k ) );
} // rotl

static size_t nextsize_t( void )
{
  const size_t result_plus = xseeds[0] + xseeds[3];
  const size_t t = xseeds[1] << 17;
  xseeds[2] ^= xseeds[0];
  xseeds[3] ^= xseeds[1];
  xseeds[1] ^= xseeds[2];
  xseeds[0] ^= xseeds[3];
  xseeds[2] ^= t;
  xseeds[3] = rotl( xseeds[3], 45 );
  return result_plus;
} // nextint

double nextdouble( void )
{
  return ( double )( nextsize_t( ) >> 11 ) * ( 1.0 / ( double )( ( size_t )( 1 ) << 53 ) );
} // nextdouble

double stdnormal(void)
// return a real number from a standard normal (Gaussian) distribution
// by polar form of Box-Muller transformation (Marsaglia)
{
  double variate = 0.0;
  static double nextvariate = 0.0;
  static bool usenextvariate = false;

  if ( usenextvariate ) variate = nextvariate;
  else {
    double x = 0.0;
    double y = 0.0;
    double r = 0.0;
    do {
      x = 2.0 * nextdouble( ) - 1.0;
      y = 2.0 * nextdouble( ) - 1.0;
      r = x * x + y * y;
    }
    while ( r >= 1.0 );
    if ( iszero( r ) ) r = DBL_MIN;
    double s = sqrt( -2.0 * log( r ) / r );
    variate = x * s;
    nextvariate = y * s;
  }
  usenextvariate = !usenextvariate;
  return variate;
} // stdnormal

void permutate_t( const size_t n, size_t* a )
// Uses the Fisher-Yates or Knuth shuffle
{
  for ( size_t i = 1; i < n; i++ ) {
    size_t k = i + ( size_t )( nextdouble( ) * ( double )( n - i + 1 ) );
    size_t j = a[i];
    a[i] = a[k];
    a[k] = j;
  }
} // permutate

size_t binarysearch( const size_t n, double* x, const double p )
{
  size_t a = 1;
  size_t b = n;
  size_t c = 0;
  while ( ( b - a ) > 1 ) {
    c = ( a + b ) / 2;
    if ( p <= x[c] ) b = c;
    else a = c;
  }
  return ( p <= x[a] ? a : b );
}

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// basis linear algebra functions (subroutines)
//

static size_t iamax( const size_t n, const double* const a, const size_t inca )
// return index of maximum absolute value of vector
{
  if ( n < 1 ) return 0;
  if ( n == 1 ) return 1;
  size_t ia = 0;
  double work = fabs( a[ia] );
  size_t index = 1;
  ia += inca;
  for ( size_t i = 2; i <= n; i++ ) {
    if ( fabs( a[ia] ) > work ) {
      work = fabs( a[ia] );
      index = i;
    }
    ia += inca;
  }
  return index;
} // iamax

static void swap( const size_t n, double* const a, const size_t inca, double* const b, const size_t incb )
// interchanges two vectors
{
  if ( inca != 1 || incb != 1 ) {
    size_t ia = 0;
    size_t ib = 0;
    for ( size_t i = n; i--; ) {
      const double s = a[ia];
      a[ia] = b[ib];
      b[ib] = s;
      ia += inca;
      ib += incb;
    }
    return;
  }
  size_t i = n >> 2;
  size_t j = 0;
  const size_t k = n & 3;
  double s = 0.0;
  while ( i-- ) {
    s = a[j]; a[j] = b[j]; b[j] = s;
    s = a[j + 1]; a[j + 1] = b[j + 1]; b[j + 1] = s;
    s = a[j + 2]; a[j + 2] = b[j + 2]; b[j + 2] = s;
    s = a[j + 3]; a[j + 3] = b[j + 3]; b[j + 3] = s;
    j += 4;
  }
  switch ( k ) {
  case 3: s = a[j]; a[j] = b[j]; b[j] = s; j++;
  case 2: s = a[j]; a[j] = b[j]; b[j] = s; j++;
  case 1: s = a[j]; a[j] = b[j]; b[j] = s; j++;
  }
} // swap

static void zeroall( const size_t n, double* const a )
// set all elements of vector a equal to zero
{
  memset( a, 0, n * sizeof( a[1] ) );
} // zeroall

static double asum( const size_t n, const double* const a, const size_t inca )
// return the sum of the absolute values of vector a
{
  if ( inca != 1 ) {
    size_t ia = 0;
    double s = 0.0;
    for ( size_t i = n; i--;) {
      s += fabs( a[ia] );
      ia += inca;
    }
    return s;
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  double s = 0.0;
  while ( i-- ) {
    s += fabs( a[j] ) + fabs( a[j + 1] ) + fabs( a[j + 2] ) + fabs( a[j + 3] ) + fabs( a[j + 4] ) + fabs( a[j + 5] ) + fabs( a[j + 6] ) + fabs( a[j + 7] );
    j += 8;
  }
  switch ( k ) {
    case 7: s += fabs( a[j] ); j++;
    case 6: s += fabs( a[j] ); j++;
    case 5: s += fabs( a[j] ); j++;
    case 4: s += fabs( a[j] ); j++;
    case 3: s += fabs( a[j] ); j++;
    case 2: s += fabs( a[j] ); j++;
    case 1: s += fabs( a[j] ); j++;
  }
  return s;
} // asum

double sum( const size_t n, const double* const a, const size_t inca )
// returns sum of vector
{
  double s = 0.0;
  if ( inca != 1 ) {
    size_t ia = 0;
    for ( size_t i = n; i--; ) {
      s += a[ia];
      ia += inca;
    }
    return s;
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  while ( i-- ) {
    s += a[j] + a[j + 1] + a[j + 2] + a[j + 3] + a[j + 4] + a[j + 5] + a[j + 6] + a[j + 7];
    j += 8;
  }
  switch ( k ) {
  case 7: s += a[j]; j++;
  case 6: s += a[j]; j++;
  case 5: s += a[j]; j++;
  case 4: s += a[j]; j++;
  case 3: s += a[j]; j++;
  case 2: s += a[j]; j++;
  case 1: s += a[j]; j++;
  }
  return s;
} // sum

double wsum( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw )
// returns weighted sum of vector
{
  double s = 0.0;
  if ( inca != 1 || incw != 1 ) {
    size_t ia = 0;
    size_t iw = 0;
    for ( size_t i = n; i--; ) {
      s += w[iw] * a[ia];
      ia += inca;
      iw += incw;
    }
    return s;
  }
  size_t i = n >> 2;
  size_t j = 0;
  const size_t k = n & 3;
  while ( i-- ) {
    s += w[j] * a[j];
    s += w[j + 1] * a[j + 1];
    s += w[j + 2] * a[j + 2];
    s += w[j + 3] * a[j + 3];
    j += 4;
  }
  switch ( k ) {
  case 3: s += w[j] * a[j]; j++;
  case 2: s += w[j] * a[j]; j++;
  case 1: s += w[j] * a[j]; j++;
  }
  return s;
} // wsum

void scal( const size_t n, const double c, double* const a, const size_t inca )
// scales vector a by constant c
// if c equals 1.0 there is a fast return
// if c equals 0.0 vector a is set to zero
{
  if ( n == 0 ) return;
  if ( isequal( c, 1.0 ) ) return;
  if ( inca != 1 ) {
    size_t ia = 0;
    for ( size_t i = n; i--; ) {
      a[ia] = c * a[ia];
      ia += inca;
    }
    return;
  }
  size_t i = n >> 2;
  size_t j = 0;
  const size_t k = n & 3;
  if ( iszero( c ) ) {
    while ( i-- ) {
      a[j] = 0.0;
      a[j + 1] = 0.0;
      a[j + 2] = 0.0;
      a[j + 3] = 0.0;
      j += 4;
    }
    switch ( k ) {
    case 3: a[j] = 0.0; j++;
    case 2: a[j] = 0.0; j++;
    case 1: a[j] = 0.0; j++;
    }
  }
  else {
    register const double C = c;
    register double a0, a1, a2, a3;
    while ( i-- ) {
      a0 = a[j];
      a1 = a[j + 1];
      a2 = a[j + 2];
      a3 = a[j + 3];
      a0 *= C;
      a1 *= C;
      a2 *= C;
      a3 *= C;
      a[j] = a0;
      a[j + 1] = a1;
      a[j + 2] = a2;
      a[j + 3] = a3;
      j += 4;
    }
    switch ( k ) {
    case 3: a[j] *= C; j++;
    case 2: a[j] *= C; j++;
    case 1: a[j] *= C; j++;
    }
  }
} // scal

double dot( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb )
// returns the dot product of two vectors
// with the same vector for a and b, the sum-of-squares is returned
{
  if ( n == 0 ) return 0.0;
  double s = 0.0;
  if ( inca != 1 || incb != 1 ) {
    size_t ia = 0;
    size_t ib = 0;
    for ( size_t i = n; i--; ) {
      s += a[ia] * b[ib];
      ia += inca;
      ib += incb;
    }
    return s;
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  while ( i-- ) {
    s += a[j] * b[j];
    s += a[j + 1] * b[j + 1];
    s += a[j + 2] * b[j + 2];
    s += a[j + 3] * b[j + 3];
    s += a[j + 4] * b[j + 4];
    s += a[j + 5] * b[j + 5];
    s += a[j + 6] * b[j + 6];
    s += a[j + 7] * b[j + 7];
    j += 8;
  }
  switch ( k ) {
  case 7: s += a[j] * b[j]; j++;
  case 6: s += a[j] * b[j]; j++;
  case 5: s += a[j] * b[j]; j++;
  case 4: s += a[j] * b[j]; j++;
  case 3: s += a[j] * b[j]; j++;
  case 2: s += a[j] * b[j]; j++;
  case 1: s += a[j] * b[j]; j++;
  }
  return s;
} // dot

double wdot( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw )
// returns the weighted dot product of two vectors
// with the same vector for a and b, the weighted sum-of-squares is returned
{
  if ( n == 0 ) return 0.0;
  double s = 0.0;
  if ( inca != 1 || incb != 1 || incw != 1 ) {
    size_t ia = 0;
    size_t ib = 0;
    size_t iw = 0;
    for ( size_t i = n; i--; ) {
      s += w[iw] * a[ia] * b[ib];
      ia += inca;
      ib += incb;
      iw += incw;
    }
    return s;
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  while ( i-- ) {
    s += w[j] * a[j] * b[j];
    s += w[j + 1] * a[j + 1] * b[j + 1];
    s += w[j + 2] * a[j + 2] * b[j + 2];
    s += w[j + 3] * a[j + 3] * b[j + 3];
    s += w[j + 4] * a[j + 4] * b[j + 4];
    s += w[j + 5] * a[j + 5] * b[j + 5];
    s += w[j + 6] * a[j + 6] * b[j + 6];
    s += w[j + 7] * a[j + 7] * b[j + 7];
    j += 8;
  }
  switch ( k ) {
    case 7: s += w[j] * a[j] * b[j]; j++;
    case 6: s += w[j] * a[j] * b[j]; j++;
    case 5: s += w[j] * a[j] * b[j]; j++;
    case 4: s += w[j] * a[j] * b[j]; j++;
    case 3: s += w[j] * a[j] * b[j]; j++;
    case 2: s += w[j] * a[j] * b[j]; j++;
    case 1: s += w[j] * a[j] * b[j]; j++;
  }
  return s;
} // wdot

double ssq( const size_t n, const double* const a, const size_t inca )
// returns the weighted sum-of-squares of vector a
{
  double s = 0.0;
  if ( inca != 1 ) {
    size_t ia = 0;
    for ( size_t i = n; i--; ) {
      s += a[ia] * a[ia];
      ia += inca;
    }
    return s;
  }
  size_t i = n >> 2;
  size_t j = 0;
  const size_t k = n & 3;
  while ( i-- ) {
    s += a[j] * a[j] + a[j + 1] * a[j + 1] + a[j + 2] * a[j + 2] + a[j + 3] * a[j + 3];  // profile
    j += 4;
  }
  switch ( k ) {
  case 3: s += a[j] * a[j]; j++;
  case 2: s += a[j] * a[j]; j++;
  case 1: s += a[j] * a[j]; j++;
  }
  return s;
} // ssq

double wssq( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw )
// returns the weighted sum-of-squares of vector a
{
  double s = 0.0;
  if ( inca != 1 || incw != 1 ) {
    size_t ia = 0;
    size_t iw = 0;
    for ( size_t i = n; i--; ) {
      s += w[iw] * a[ia] * a[ia];
      ia += inca;
      iw += incw;
    }
    return s;
  }
  size_t i = n >> 2;
  size_t j = 0;
  const size_t k = n & 3;
  while ( i-- ) {
    s += w[j] * a[j] * a[j] + w[j + 1] * a[j + 1] * a[j + 1] + w[j + 2] * a[j + 2] * a[j + 2] + w[j + 3] * a[j + 3] * a[j + 3];  // profile
    j += 4;
  }
  switch ( k ) {
  case 3: s += w[j] * a[j] * a[j]; j++;
  case 2: s += w[j] * a[j] * a[j]; j++;
  case 1: s += w[j] * a[j] * a[j]; j++;
  }
  return s;
} // wssq

void axpy( const size_t n, const double c, double* a, const size_t inca, double* b, const size_t incb )
// constant c times vector a plus vector b is returned in vector b: b = b+ca
{
  if ( iszero( c ) ) return;
  if ( inca != 1 || incb != 1 ) {
    size_t ia = 0;
    size_t ib = 0;
    for ( size_t i = n; i--; ) {
      b[ib] += c * a[ia];
      ia += inca;
      ib += incb;
    }
    return;
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  double* __restrict ra = a;
  double* __restrict rb = b;
  while ( i-- ) {

    const double ca1 = ra[j];
    const double ca2 = ra[j + 1];
    const double ca3 = ra[j + 2];
    const double ca4 = ra[j + 3];
    const double ca5 = ra[j + 4];
    const double ca6 = ra[j + 5];
    const double ca7 = ra[j + 6];
    const double ca8 = ra[j + 7];

    const double cb1 = rb[j];
    const double cb2 = rb[j + 1];
    const double cb3 = rb[j + 2];
    const double cb4 = rb[j + 3];
    const double cb5 = rb[j + 4];
    const double cb6 = rb[j + 5];
    const double cb7 = rb[j + 6];
    const double cb8 = rb[j + 7];

    const double nb1 = cb1 + ( ca1 * c );
    const double nb2 = cb2 + ( ca2 * c );
    const double nb3 = cb3 + ( ca3 * c );
    const double nb4 = cb4 + ( ca4 * c );
    const double nb5 = cb5 + ( ca5 * c );
    const double nb6 = cb6 + ( ca6 * c );
    const double nb7 = cb7 + ( ca7 * c );
    const double nb8 = cb8 + ( ca8 * c );

    rb[j] = nb1;
    rb[j + 1] = nb2;
    rb[j + 2] = nb3;
    rb[j + 3] = nb4;
    rb[j + 4] = nb5;
    rb[j + 5] = nb6;
    rb[j + 6] = nb7;
    rb[j + 7] = nb8;

    j += 8;
  }
  switch ( k ) {
  case 7: b[j] += c * a[j]; j++;
  case 6: b[j] += c * a[j]; j++;
  case 5: b[j] += c * a[j]; j++;
  case 4: b[j] += c * a[j]; j++;
  case 3: b[j] += c * a[j]; j++;
  case 2: b[j] += c * a[j]; j++;
  case 1: b[j] += c * a[j]; j++;
  }
} // axpy

double wnrm2( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw )
// dnrm2 returns the weighted euclidean norm of a difference vector, i.e., dnrm2 = sqrt ( a-b'*diag(w)*a-b )
{  // profile
  if ( n == 0 ) return 0.0;
  double d;
  double s = 0.0;
  if ( inca != 1 || incb != 1 || incw != 1 ) {
    size_t ia = 0;
    size_t ib = 0;
    size_t iw = 0;
    for ( size_t i = n; i--; ) {
      if ( w[iw] != 0.0 ) {
        d = a[ia] - b[ib];
        s += d * w[iw] * d;
      }
      ia += inca;
      ib += incb;
      iw += incw;
    }
    return sqrt( s );
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  while ( i-- ) {
    if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; }
    if ( w[j + 1] != 0.0 ) { d = a[j + 1] - b[j + 1]; s += d * w[j + 1] * d; }
    if ( w[j + 2] != 0.0 ) { d = a[j + 2] - b[j + 2]; s += d * w[j + 2] * d; }
    if ( w[j + 3] != 0.0 ) { d = a[j + 3] - b[j + 3]; s += d * w[j + 3] * d; }
    if ( w[j + 4] != 0.0 ) { d = a[j + 4] - b[j + 4]; s += d * w[j + 4] * d; }
    if ( w[j + 5] != 0.0 ) { d = a[j + 5] - b[j + 5]; s += d * w[j + 5] * d; }
    if ( w[j + 6] != 0.0 ) { d = a[j + 6] - b[j + 6]; s += d * w[j + 6] * d; }
    if ( w[j + 7] != 0.0 ) { d = a[j + 7] - b[j + 7]; s += d * w[j + 7] * d; }
    j += 8;
  }
  switch ( k ) {
  case 7: if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; } j++;
  case 6: if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; } j++;
  case 5: if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; } j++;
  case 4: if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; } j++;
  case 3: if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; } j++;
  case 2: if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; } j++;
  case 1: if ( w[j] != 0.0 ) { d = a[j] - b[j]; s += d * w[j] * d; } j++;
  }
  return sqrt( s );
} // wnrm2

void set( const size_t n, const double b, double* const a, const size_t inca )
// set elements of vector a equal to scalar b
{
  if ( inca != 1 ) {
    size_t ia = 0;
    for ( size_t i = n; i--; ) {
      a[ia] = b;
      ia += inca;
    }
    return;
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  while ( i-- ) {
    a[j] = b;
    a[j + 1] = b;
    a[j + 2] = b;
    a[j + 3] = b;
    a[j + 4] = b;
    a[j + 5] = b;
    a[j + 6] = b;
    a[j + 7] = b;
    j += 8;
  }
  switch ( k ) {
    case 7: a[j] = b; j++;
    case 6: a[j] = b; j++;
    case 5: a[j] = b; j++;
    case 4: a[j] = b; j++;
    case 3: a[j] = b; j++;
    case 2: a[j] = b; j++;
    case 1: a[j] = b;
  }
} // set

void copy( const size_t n, const double* const a, const size_t inca, double* const b, const size_t incb )
// copies vector a to vector b
{
  if ( n == 0 ) return;
  if ( inca != 1 || incb != 1 ) {
    size_t ia = 0;
    size_t ib = 0;
    for ( size_t i = n; i--; ) {
      b[ib] = a[ia];
      ia += inca;
      ib += incb;
    }
    return;
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  while ( i-- ) {
    b[j] = a[j];
    b[j + 1] = a[j + 1];
    b[j + 2] = a[j + 2];
    b[j + 3] = a[j + 3];
    b[j + 4] = a[j + 4];
    b[j + 5] = a[j + 5];
    b[j + 6] = a[j + 6];
    b[j + 7] = a[j + 7];
    j += 8;
  }
  switch ( k ) {
    case 7: b[j] = a[j]; j++;
    case 6: b[j] = a[j]; j++;
    case 5: b[j] = a[j]; j++;
    case 4: b[j] = a[j]; j++;
    case 3: b[j] = a[j]; j++;
    case 2: b[j] = a[j]; j++;
    case 1: b[j] = a[j]; j++;
  }
} // copy

void copyall( const size_t n, const double* const a, double* const b )
{
  memcpy( b, a, n * sizeof( a[1] ) );
} // copyall

void gemv( const bool transa, const size_t nra, const size_t nca, const double alpha, double** const a, double* const b, const double beta, double* const c )
// c = alpha * trans( a ) * b + beta * c
{
  if ( nra == 0 || nca == 0 || ( iszero( alpha ) && isequal( beta, 1.0 ) ) ) return;
  if ( transa == false ) {
    double* r = getvector( nra, 0.0 );
    double* ab = getvector( nca, 0.0 );
    axpy( nra, beta, &c[1], 1, &r[1], 1 );
    if ( isnotzero( alpha ) ) {
      axpy( nca, alpha, &b[1], 1, &ab[1], 1 );
      for ( size_t i = 1; i <= nra; i++ ) {
        double work = 0.0;
        for ( size_t j = 1; j <= nca; j++ ) work += a[i][j] * ab[j];
        r[i] += work;
      }
    }
    copy( nra, &r[1], 1, &c[1], 1 );
    freevector( r );
    freevector( ab );
  }
  else if ( transa == true ) {
    double* r = getvector( nca, 0.0 );
    axpy( nca, beta, &c[1], 1, &r[1], 1 );
    if ( isnotzero( alpha ) ) {
      for ( size_t j = 1; j <= nca; j++ ) {
        double work = 0.0;
        for ( size_t i = 1; i <= nra; i++ ) work += a[i][j] * b[i];
        r[j] += alpha * work;
      }
    }
    copy( nca, &r[1], 1, &c[1], 1 );
    freevector( r );
  }
} // gemv

void gemm( const bool transa, const bool transb, const size_t nrc, const size_t ncc, const size_t nab, const double alpha, double** const a, double** const b, const double beta, double** const c )
// C = alpha * A(ta) * B(tb) + beta * C
{
  // input cannot be same as output
  assert( a != c );
  assert( b != c );

  // if alpha equals zero
  if ( iszero( alpha ) ) {
    if ( iszero( beta ) ) zeroall( nrc * ncc, &c[1][1] );
    else if ( isnotequal( beta, 1.0 ) ) scal( nrc * ncc, beta, &c[1][1], 1 );
    return;
  }

  if ( isnotzero( beta ) ) scal( nrc * ncc, beta, &c[1][1], 1 );
  else zeroall( nrc * ncc, &c[1][1] );

  // handle transpose options
  if ( transb == false ) {
    if ( transa == false ) {

      // form: C = alpha*A*B + beta*C
      for ( size_t j = 1; j <= ncc; j++ ) {
        for ( size_t k = 1; k <= nab; k++ ) {
          if ( isnotzero( b[k][j] ) ) {
            register double temp = alpha * b[k][j];
            for ( size_t i = 1; i <= nrc; i++ ) c[i][j] += temp * a[i][k];
          }
        }
      }
    }
    else {

      // form: C = alpha*A'*B + beta*C for beta == 0.0
      if ( iszero( beta ) ) {
        for ( size_t j = 1; j <= ncc; j++ ) {
          for ( size_t i = 1; i <= nrc; i++ ) {
            register double work = 0.0;
            for ( size_t k = 1; k <= nab; k++ ) work += a[k][i] * b[k][j];
            c[i][j] = alpha * work;
          }
        }
      }

      // form: C = alpha*A'*B + beta*C for beta != 0.0
      else {
        for ( size_t j = 1; j <= ncc; j++ ) {
          for ( size_t i = 1; i <= nrc; i++ ) {
            register double work = 0.0;
            for ( size_t k = 1; k <= nab; k++ ) work += a[k][i] * b[k][j];
            c[i][j] += alpha * work;
          }
        }
      }
    }
  }
  else {
    if ( transa == false ) {

      // form: C = alpha*A*B' + beta*C
      for ( size_t j = 1; j <= ncc; j++ ) {
        for ( size_t k = 1; k <= nab; k++ ) {
          if ( isnotzero( b[j][k] ) ) {
            register double work = alpha * b[j][k];
            for ( size_t i = 1; i <= nrc; i++ ) c[i][j] += work * a[i][k];
          }
        }
      }
    }
    else {

      // form: C = alpha*A'*B' + beta*C for beta == 0.0
      if ( iszero( beta ) ) {
        for ( size_t j = 1; j <= ncc; j++ ) {
          for ( size_t i = 1; i <= nrc; i++ ) {
            register double work = 0.0;
            for ( size_t k = 1; k <= nab; k++ ) work += a[k][i] * b[j][k];
            c[i][j] = alpha * work;
          }
        }
      }

      // form: C = alpha*A'*B' + beta*C for beta != 0.0
      else {
        for ( size_t j = 1; j <= ncc; j++ ) {
          for ( size_t i = 1; i <= nrc; i++ ) {
            register double work = 0.0;
            for ( size_t k = 1; k <= nab; k++ ) work += a[k][i] * b[j][k];
            c[i][j] += alpha * work;
          }
        }
      }
    }
  }
} // gemm

void euclidean1( const size_t n, const size_t p, double** a, double** r )
// compute euclidean distances r between rows of a and b
{
  for ( size_t i = 1; i <= n; i++ ) {
    r[i][i] = 0.0;
    for ( size_t j = 1; j < i; j++ ) {
      double sum = 0.0;
      for ( size_t k = 1; k <= p; k++ ) {
        const double diff = a[i][k] - a[j][k];
        if ( isnotzero( diff ) ) sum += diff * diff;
      }
      r[i][j] = r[j][i] = sqrt( sum );
    }
  }
} // euclidean1

void euclidean2( const size_t n, const size_t p, double** a, const size_t m, double** b, double** const r )
// compute euclidean distances r between rows of a and b
{
  for ( size_t j = 1; j <= m; j++ ) {
    for ( size_t i = 1; i <= n; i++ ) {
      double sum = 0.0;
      for ( size_t k = 1; k <= p; k++ ) {
        const double diff = a[i][k] - b[j][k];
        if ( isnotzero( diff ) ) sum += diff * diff;
      }
      r[i][j] = sqrt( sum );
    }
  }
} // euclidean2

void squaredeuclidean1( const size_t n, const size_t p, double** a, double** r )
// compute euclidean distances r between rows of a and b
{
  for ( size_t i = 1; i <= n; i++ ) {
    r[i][i] = 0.0;
    for ( size_t j = 1; j < i; j++ ) {
      double sum = 0.0;
      for ( size_t k = 1; k <= p; k++ ) {
        const double diff = a[i][k] - a[j][k];
        if ( isnotzero( diff ) ) sum += diff * diff;
      }
      r[i][j] = r[j][i] = sum;
    }
  }
} // squaredeuclidean1

void squaredeuclidean2( const size_t n, const size_t p, double** a, const size_t m, double** b, double** const r )
// compute squared euclidean distances r between rows of a and b with minimum EPSILON
{
  for ( size_t j = 1; j <= m; j++ ) {
    for ( size_t i = 1; i <= n; i++ ) {
      double sum = 0.0;
      for ( size_t k = 1; k <= p; k++ ) {
        const double diff = a[i][k] - b[j][k];
        if ( isnotzero( diff ) ) sum += diff * diff;
      }
      r[i][j] = fmax( DBL_EPSILON, sum );
    }
  }
} // squaredeuclidean2

static void insertionsort( const size_t n, double* const a, size_t* const r )
// fast insertion sort for small arrays
{
  for ( size_t i = n - 1; i > 0; --i ) {
    size_t j = i;
    const double val = a[i];
    const size_t ind = r[i];
    for ( ; j < n && a[j + 1] < val; ++j ) {
      a[j] = a[j + 1];
      r[j] = r[j + 1];
    }
    a[j] = val;
    r[j] = ind;
  }
} // insertionsort

static void dsort( const size_t n, double* const a, size_t* const r )
// replaces double vector with elements in increasing order
// also return the ordered indices in r
{
  // fast return
  if ( n <= 1 ) return;

  // fast sort for small vectors
  if ( n <= 16 ) {
    insertionsort( n, a, r );
    return;
  }

  const size_t select = 20;
  size_t i, j;
  double dmnmx;

  long stkpnt = 0;
  size_t stack[2][32] = { 0 };
  stack[0][0] = 1;
  stack[1][0] = n;
  while ( stkpnt >= 0 ) {
    size_t start = stack[0][stkpnt];
    size_t endd = stack[1][stkpnt];
    stkpnt--;
    if ( endd - start <= select && endd - start > 0 ) {
      for ( i = start + 1; i <= endd; i++ ) {
        for ( j = i; j >= start + 1; j-- ) {
          if ( a[j] < a[j - 1] ) {
            dmnmx = a[j];
            a[j] = a[j - 1];
            a[j - 1] = dmnmx;
            size_t itmp = r[j];
            r[j] = r[j - 1];
            r[j - 1] = itmp;
          }
          else break;
        }
      }
    }
    else if ( endd - start > select ) {
      const double d1 = a[start];
      const double d2 = a[endd];
      i = ( start + endd ) / 2;
      const double d3 = a[i];
      if ( d1 < d2 ) {
        if ( d3 < d1 ) dmnmx = d1;
        else if ( d3 < d2 ) dmnmx = d3;
        else dmnmx = d2;
      }
      else {
        if ( d3 < d2 ) dmnmx = d2;
        else if ( d3 < d1 ) dmnmx = d3;
        else dmnmx = d1;
      }
      i = start - 1;
      j = endd + 1;
      do {
        do {
          j--;
        }
        while ( a[j] > dmnmx );
        do {
          i++;
        }
        while ( a[i] < dmnmx );
        if ( i < j ) {
          double dtmp = a[i];
          a[i] = a[j];
          a[j] = dtmp;
          size_t itmp = r[i];
          r[i] = r[j];
          r[j] = itmp;
        }
      }
      while ( i < j );
      if ( j - start > endd - j - 1 ) {
        stkpnt++;
        stack[0][stkpnt] = start;
        stack[1][stkpnt] = j;
        stkpnt++;
        stack[0][stkpnt] = j + 1;
        stack[1][stkpnt] = endd;
      }
      else {
        stkpnt++;
        stack[0][stkpnt] = j + 1;
        stack[1][stkpnt] = endd;
        stkpnt++;
        stack[0][stkpnt] = start;
        stack[1][stkpnt] = j;
      }
    }
  }
} // dsort

bool symmetric( const size_t n, double** x )
{
  if ( n == 1 ) return true;
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) {
      if ( isnotequal( x[i][j], x[j][i] ) ) return false;
    }
  }
  return true;
} // symmetric

bool anyequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = false;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( isequal( a[ia], c ) ) {
      b = true;
      break;
    }
    ia += inca;
  }
  return b;
} // anyequal

bool anynotequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = false;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( isnotequal( a[ia], c ) ) {
      b = true;
      break;
    }
    ia += inca;
  }
  return b;
} // anynotequal

bool anygreater( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = false;
  const double cplus = c + DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] > cplus ) {
      b = true;
      break;
    }
    ia += inca;
  }
  return b;
} // anygreater

bool anysmaller( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = false;
  const double cminus = c - DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] < cminus ) {
      b = true;
      break;
    }
    ia += inca;
  }
  return b;
} // anysmaller

bool anygreaterequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = false;
  const double cminus = c - DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] >= cminus ) {
      b = true;
      break;
    }
    ia += inca;
  }
  return b;
} // anygreaterequal

bool anysmallerequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = false;
  const double cplus = c + DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] <= cplus ) {
      b = true;
      break;
    }
    ia += inca;
  }
  return b;
} // anysmallerequal

bool allequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = true;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( isnotequal( a[ia], c ) ) {
      b = false;
      break;
    }
    ia += inca;
  }
  return b;
} // allequal

bool allnotequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = true;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( isequal( a[ia], c ) ) {
      b = false;
      break;
    }
    ia += inca;
  }
  return b;
} // allnotequal

bool allgreater( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = true;
  const double cplus = c + DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] <= cplus ) {
      b = false;
      break;
    }
    ia += inca;
  }
  return b;
} // allgreater

bool allsmaller( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = true;
  const double cminus = c - DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] >= cminus ) {
      b = false;
      break;
    }
    ia += inca;
  }
  return b;
} // allsmaller

bool allgreaterequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = true;
  const double cminus = c - DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] < cminus ) {
      b = false;
      break;
    }
    ia += inca;
  }
  return b;
} // allgreaterequal

bool allsmallerequal( const size_t n, double* a, const size_t inca, const double c )
{
  bool b = true;
  const double cplus = c + DBL_EPSILON;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] > cplus ) {
      b = false;
      break;
    }
    ia += inca;
  }
  return b;
} // allgreaterequal

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// memory functions
//

bool* getbvector( const size_t nr, const bool c )
// allocates vector space on the heap
{
  bool* ptr = 0;
  if ( nr == 0 ) return ptr;
  ptr = ( bool* ) calloc( nr, sizeof( bool ) );
  ptr--;
  for ( size_t i = 1; i <= nr; i++ ) ptr[i] = c;
  return ptr;
} // getbvector

void freebvector( bool* a )
// de-allocates vector space from the heap
{
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freebvector

int* getivector( const size_t nr, const int c )
// allocates vector space on the heap
{
  int* ptr = 0;
  if ( nr == 0 ) return ptr;
  ptr = ( int* ) calloc( nr, sizeof( int ) );
  ptr--;
  for ( size_t i = 1; i <= nr; i++ ) ptr[i] = c;
  return ptr;
} // getivector

void freeivector( int* a )
// de-allocates vector space from the heap
{
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freeivector

size_t* getvector_t( const size_t nr, const size_t c )
// allocates vector space on the heap
{
  size_t* ptr = 0;
  if ( nr == 0 ) return ptr;
  ptr = ( size_t* ) calloc( nr, sizeof( size_t ) );
  ptr--;
  for ( size_t i = 1; i <= nr; i++ ) ptr[i] = c;
  return ptr;
} // getvector_t

void freevector_t( size_t* a )
// de-allocates vector space from the heap
{
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freevector_t

double* getvector( const size_t nr, const double c )
// allocates vector space on the heap
{
  double* ptr = 0;
  if ( nr == 0 ) return ptr;
  ptr = ( double* ) calloc( nr, sizeof( double ) );
  ptr--;
  for ( size_t i = 1; i <= nr; i++ ) ptr[i] = c;
  return ptr;
} // getvector

void freevector( double* a )
// de-allocates vector space from the heap
{
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freevector


int** getimatrix( const size_t nr, const size_t nc, const int c )
// allocates matrix space on the heap
{
  int** ptr = 0;
  if ( nr == 0 || nc == 0 ) return ptr;
  int* block = 0;
  ptr = ( int** ) calloc( nr, sizeof( int* ) );
  block = ( int* ) calloc( nr*nc, sizeof( int ) );
  ptr--;
  block--;
  for ( size_t i = 1, im1 = 0; i <= nr; i++, im1++ ) {
    ptr[i] = &block[im1*nc];
    for ( size_t j = 1; j <= nc; j++ ) ptr[i][j] = c;
  }
  return ptr;
} // getimatrix

void freeimatrix( int** a )
// de-allocates matrix space from the heap
{
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a[1] ); 
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freeimatrix

size_t** getmatrix_t( const size_t nr, const size_t nc, const size_t c )
// allocates matrix space on the heap
{
  size_t** ptr = 0;
  if ( nr == 0 || nc == 0 ) return ptr;
  size_t* block = 0;
  ptr = ( size_t** ) calloc( nr, sizeof( size_t* ) );
  block = ( size_t* ) calloc( nr*nc, sizeof( size_t ) );
  ptr--;
  block--;
  for ( size_t i = 1, im1 = 0; i <= nr; i++, im1++ ) {
    ptr[i] = &block[im1*nc];
    for ( size_t j = 1; j <= nc; j++ ) ptr[i][j] = c;
  }
  return ptr;
} // getmatrix_t

void freematrix_t( size_t** a )
// de-allocates matrix space from the heap
{
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a[1] ); 
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freematrix_t

double** getmatrix( const size_t nr, const size_t nc, const double c )
// allocates matrix space on the heap
{
  double** ptr = 0;
  if ( nr == 0 || nc == 0 ) return ptr;
  double* block = 0;
  ptr = ( double** ) calloc( nr, sizeof( double* ) );
  block = ( double* ) calloc( nr*nc, sizeof( double ) );
  ptr--;
  block--;
  for ( size_t i = 1, im1 = 0; i <= nr; i++, im1++ ) {
    ptr[i] = &block[im1*nc];
    for ( size_t j = 1; j <= nc; j++ ) ptr[i][j] = c;
  }
  return ptr;
} // getmatrix

void freematrix( double** a )
// de-allocates matrix space from the heap
{
  if ( a == 0 ) return;
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a[1] ); 
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freematrix

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// linear algebra functions
//

static int inverse1x1( const size_t n, double** const a )
{
  if ( n != 1 ) return 1;
  const double det = a[1][1];
  if ( iszero( det ) ) return 1;
  a[1][1] = 1.0 / det;
  return 0;
} // inverse1x1

static int inverse2x2( const size_t n, double** const a )
{
  if ( n != 2 ) return 1;
  const double det = a[1][1] * a[2][2] - a[1][2] * a[2][1];
  if ( iszero( det ) ) return 1;
  const double detinv = 1.0 / det;
  const double temp = a[1][1];
  a[1][1] = detinv * a[2][2];
  a[1][2] = -detinv * a[1][2];
  a[2][1] = -detinv * a[2][1];
  a[2][2] = detinv * temp;
  return 0;
} // inverse2x2

static int inverse3x3( const size_t n, double** const a )
{
  if ( n != 3 ) return 1;
  double r[9] = { 0 };
  r[0] = a[3][3] * a[2][2] - a[3][2] * a[2][3];
  r[1] = a[3][2] * a[1][3] - a[3][3] * a[1][2];
  r[2] = a[2][3] * a[1][2] - a[2][2] * a[1][3];
  r[3] = a[3][1] * a[2][3] - a[3][3] * a[2][1];
  r[4] = a[3][3] * a[1][1] - a[3][1] * a[1][3];
  r[5] = a[2][1] * a[1][3] - a[2][3] * a[1][1];
  r[6] = a[3][2] * a[2][1] - a[3][1] * a[2][2];
  r[7] = a[3][1] * a[1][2] - a[3][2] * a[1][1];
  r[8] = a[2][2] * a[1][1] - a[2][1] * a[1][2];
  const double det = a[1][1] * r[0] + a[2][1] * r[1] + a[3][1] * r[2];
  if ( iszero( det ) ) return 1;
  const double detinv = 1.0 / det;
  for ( size_t i = 1, k = 0; i <= 3; i++ ) for ( size_t j = 1; j <= 3; j++, k++ ) a[i][j] = detinv * r[k];
  return 0;
} // inverse3x3

static int inverse4x4( const size_t n, double** const a )
{
  if ( n != 4 ) return 1;

  double t[12] = { 0 };
  double r[16] = { 0 };

  t[0] = a[3][3] * a[4][4];
  t[1] = a[4][3] * a[3][4];
  t[2] = a[2][3] * a[4][4];
  t[3] = a[4][3] * a[2][4];
  t[4] = a[2][3] * a[3][4];
  t[5] = a[3][3] * a[2][4];
  t[6] = a[1][3] * a[4][4];
  t[7] = a[4][3] * a[1][4];
  t[8] = a[1][3] * a[3][4];
  t[9] = a[3][3] * a[1][4];
  t[10] = a[1][3] * a[2][4];
  t[11] = a[2][3] * a[1][4];

  r[0] = t[0] * a[2][2] + t[3] * a[3][2] + t[4] * a[4][2];
  r[0] -= t[1] * a[2][2] + t[2] * a[3][2] + t[5] * a[4][2];
  r[1] = t[1] * a[1][2] + t[6] * a[3][2] + t[9] * a[4][2];
  r[1] -= t[0] * a[1][2] + t[7] * a[3][2] + t[8] * a[4][2];
  r[2] = t[2] * a[1][2] + t[7] * a[2][2] + t[10] * a[4][2];
  r[2] -= t[3] * a[1][2] + t[6] * a[2][2] + t[11] * a[4][2];
  r[3] = t[5] * a[1][2] + t[8] * a[2][2] + t[11] * a[3][2];
  r[3] -= t[4] * a[1][2] + t[9] * a[2][2] + t[10] * a[3][2];
  r[4] = t[1] * a[2][1] + t[2] * a[3][1] + t[5] * a[4][1];
  r[4] -= t[0] * a[2][1] + t[3] * a[3][1] + t[4] * a[4][1];
  r[5] = t[0] * a[1][1] + t[7] * a[3][1] + t[8] * a[4][1];
  r[5] -= t[1] * a[1][1] + t[6] * a[3][1] + t[9] * a[4][1];
  r[6] = t[3] * a[1][1] + t[6] * a[2][1] + t[11] * a[4][1];
  r[6] -= t[2] * a[1][1] + t[7] * a[2][1] + t[10] * a[4][1];
  r[7] = t[4] * a[1][1] + t[9] * a[2][1] + t[10] * a[3][1];
  r[7] -= t[5] * a[1][1] + t[8] * a[2][1] + t[11] * a[3][1];

  t[0] = a[3][1] * a[4][2];
  t[1] = a[4][1] * a[3][2];
  t[2] = a[2][1] * a[4][2];
  t[3] = a[4][1] * a[2][2];
  t[4] = a[2][1] * a[3][2];
  t[5] = a[3][1] * a[2][2];
  t[6] = a[1][1] * a[4][2];
  t[7] = a[4][1] * a[1][2];
  t[8] = a[1][1] * a[3][2];
  t[9] = a[3][1] * a[1][2];
  t[10] = a[1][1] * a[2][2];
  t[11] = a[2][1] * a[1][2];

  r[8] = t[0] * a[2][4] + t[3] * a[3][4] + t[4] * a[4][4];
  r[8] -= t[1] * a[2][4] + t[2] * a[3][4] + t[5] * a[4][4];
  r[9] = t[1] * a[1][4] + t[6] * a[3][4] + t[9] * a[4][4];
  r[9] -= t[0] * a[1][4] + t[7] * a[3][4] + t[8] * a[4][4];
  r[10] = t[2] * a[1][4] + t[7] * a[2][4] + t[10] * a[4][4];
  r[10] -= t[3] * a[1][4] + t[6] * a[2][4] + t[11] * a[4][4];
  r[11] = t[5] * a[1][4] + t[8] * a[2][4] + t[11] * a[3][4];
  r[11] -= t[4] * a[1][4] + t[9] * a[2][4] + t[10] * a[3][4];
  r[12] = t[2] * a[3][3] + t[5] * a[4][3] + t[1] * a[2][3];
  r[12] -= t[4] * a[4][3] + t[0] * a[2][3] + t[3] * a[3][3];
  r[13] = t[8] * a[4][3] + t[0] * a[1][3] + t[7] * a[3][3];
  r[13] -= t[6] * a[3][3] + t[9] * a[4][3] + t[1] * a[1][3];
  r[14] = t[6] * a[2][3] + t[11] * a[4][3] + t[3] * a[1][3];
  r[14] -= t[10] * a[4][3] + t[2] * a[1][3] + t[7] * a[2][3];
  r[15] = t[10] * a[3][3] + t[4] * a[1][3] + t[9] * a[2][3];
  r[15] -= t[8] * a[2][3] + t[11] * a[3][3] + t[5] * a[1][3];

  const double det = a[1][1] * r[0] + a[2][1] * r[1] + a[3][1] * r[2] + a[4][1] * r[3];

  if ( iszero( det ) ) return 1;

  const double detinv = 1.0 / det;
  for ( size_t i = 1, k = 0; i <= 4; i++ ) for ( size_t j = 1; j <= 4; j++, k++ ) a[i][j] = detinv * r[k];

  return 0;
} // inverse4x4


int inverse( const size_t n, double** a )
// compute inverse from real symmetric matrix a
// inverse is returned in matrix a
{
  // speed up computations for small matrices
  if ( n == 1 ) return( inverse1x1( n, a ) );
  if ( n == 2 ) return( inverse2x2( n, a ) );
  if ( n == 3 ) return( inverse3x3( n, a ) );
  if ( n == 4 ) return( inverse4x4( n, a ) );


  size_t* idx = getvector_t( n, ( size_t )( 0 ) );
  for ( size_t j = 1; j <= n; j++ ) {
    size_t jp = j - 1 + iamax( n - j + 1, &a[j][j], n );
    idx[j] = jp;
    if ( isnotzero( a[jp][j] ) ) {
      if ( jp != j ) swap( n, &a[j][1], 1, &a[jp][1], 1 );
      if ( j < n ) for ( size_t i = j + 1; i <= n; i++ ) a[i][j] /= a[j][j];
    }
    else return ( int ) ( j );
    if ( j < n ) {
      for ( size_t i = j + 1; i <= n; i++ ) {
        for ( size_t ii = j + 1; ii <= n; ii++ ) a[i][ii] -= a[i][j] * a[j][ii];
      }
    }
  }
  for ( size_t j = 1; j <= n; j++ ) {
    if ( iszero( a[j][j] ) ) return ( int ) ( j );
  }
  for ( size_t j = 1; j <= n; j++ ) {
    a[j][j] = 1.0 / a[j][j];
    const double ajj = -a[j][j];
    for ( size_t i = 1; i <= j - 1; i++ ) {
      if ( isnotequal( a[i][j], 0.0 ) ) {
        const double temp = a[i][j];
        for ( size_t ii = 1; ii <= i - 1; ii++ ) a[ii][j] += temp * a[ii][i];
        a[i][j] *= a[i][i];
      }
    }
    scal( j - 1, ajj, &a[1][j], n );
  }
  double* v = getvector( n, 0.0 );
  for ( size_t j = n - 1; j >= 1; j-- ) {
    for ( size_t i = j + 1; i <= n; i++ ) {
      v[i] = a[i][j];
      a[i][j] = 0.0;
    }
    for ( size_t jj = j + 1, k = 1; jj <= n; jj++, k++ ) {
      const double work = -1.0 * v[jj];
      for ( size_t ii = 1; ii <= n; ii++ ) a[ii][j] += work * a[ii][jj];
    }
  }
  for ( size_t j = n - 1; j >= 1; j-- ) {
    const size_t jp = idx[j];
    if ( jp != j ) swap( n, &a[1][j], n, &a[1][jp], n );
  }
  freevector_t( idx );
  freevector( v );
  return 0;
} // inverse



static int Tridiagonalize( double* V, double* d, double* e, size_t n )
// Symmetric Householder reduction to tridiagonal form, derived from the Algol procedure tred2
// by Bowdler, Martin, Reinsch, and Wilkinson, Handbook for Auto. Comp., Vol.ii-Linear Algebra,
// and the corresponding Fortran subroutine in EISPACK.
{
  int retval = 0;
  double* V_nm1 = ( V + ( n - 1 ) * n );
  for ( size_t j = 0; j < n; j++ ) *( d + j ) = *( V_nm1 + j );

  // Householder reduction to tridiagonal form
  double *V_i = V + ( n - 1 ) * n;
  double *V_im1 = V_i - n;
  for ( long long lli = n - 1; lli > 0; V_i -= n, V_im1 -= n, lli-- ) {
    size_t i = ( size_t )( lli );

    // scale to avoid under/overflow
    double scale = 0.0;
    double h = 0.0;
    for ( size_t k = 0; k < i; k++ ) scale += fabs( *( d + k ) );
    if ( iszero( scale ) ) {
      *( e + i ) = *( d + i - 1 );
      double *V_j = V;
      for ( size_t j = 0; j < i; V_j += n, j++ ) {
        *( d + j ) = *( V_im1 + j );
        *( V_i + j ) = 0.0;
        *( V_j + i ) = 0.0;
      }
    }
    else {

      // generate Householder vector
      for ( size_t k = 0; k < i; k++ ) {
        double* d_k = d + k;
        *d_k /= scale;
        h += *d_k * *d_k;
      }
      double f = *( d + i - 1 );
      double g = sqrt( h );
      if ( f > 0.0 ) g = -g;
      *( e + i ) = scale * g;
      h -= f * g;
      *( d + i - 1 ) = f - g;
      for ( size_t j = 0; j < i; j++ ) *( e + j ) = 0.0;

      // apply similarity transformation to remaining columns
      double *V_j = V;
      for ( size_t j = 0; j < i; V_j += n, j++ ) {
        f = *( d + j );
        *( V_j + i ) = f;
        g = *( e + j ) + *( V_j + j ) * f;
        double *V_k = V + ( j + 1 ) * n;
        for ( size_t k = j + 1; k <= i - 1; V_k += n, k++ ) {
          g += *( V_k + j ) * *( d + k );
          *( e + k ) += *( V_k + j ) * f;
        }
        *( e + j ) = g;
      }
      f = 0.0;
      for ( size_t j = 0; j < i; j++ ) {
        *( e + j ) /= h;
        f += *( e + j ) * *( d + j );
      }
      const double hh = f / ( h + h );
      for ( size_t j = 0; j < i; j++ ) *( e + j ) -= hh * *( d + j );
      for ( size_t j = 0; j < i; j++ ) {
        f = d[j];
        g = e[j];
        double *V_k = V + j * n;
        for ( size_t k = j; k <= i - 1; V_k += n, k++ ) *( V_k + j ) -= ( f * *( e + k ) + g * *( d + k ) );
        *( d + j ) = *( V_im1 + j );
        *( V_i + j ) = 0.0;
      }
    }
    *( d + i ) = h;
  }

  // accumulate transformations
  V_i = V;
  for ( size_t i = 0; i < n - 1; V_i += n, i++ ) {
    *( V_nm1 + i ) = *( V_i + i );
    *( V_i + i ) = 1.0;
    const double h = *( d + i + 1 );
    if ( isnotzero( h ) ) {
      double *V_k = V;
      for ( size_t k = 0; k <= i; V_k += n, k++ ) *( d + k ) = *( V_k + i + 1 ) / h;
      for ( size_t j = 0; j <= i; j++ ) {
        double g = 0.0;
        V_k = V;
        for ( size_t k = 0; k <= i; V_k += n, k++ ) g += *( V_k + i + 1 ) * *( V_k + j );
        V_k = V;
        for ( size_t k = 0; k <= i; V_k += n, k++ ) *( V_k + j ) -= g * *( d + k );
      }
    }
    double *V_k = V;
    for ( size_t k = 0; k <= i; V_k += n, k++ ) *( V_k + i + 1 ) = 0.0;
  }
  for ( size_t j = 0; j < n; j++ ) {
    *( d + j ) = *( V_nm1 + j );
    *( V_nm1 + j ) = 0.0;
  }
  *( V_nm1 + n - 1 ) = 1.0;
  *( e ) = 0.0;
  return retval;
} // Tridiagonalize

// option to remove
static int Diagonalize( double *V, double *d, double *e, size_t n )
// Symmetric tridiagonal QL algorithm, derived from the Algol procedure tql2,
// by Bowdler, Martin, Reinsch, and Wilkinson, Handbook for Auto. Comp., Vol.ii-Linear Algebra,
// and the corresponding Fortran subroutine in EISPACK.
{
  int retval = 0;
  const size_t MAXITER = 40;

  double *e_i = e + 1;
  for ( size_t i = 1; i < n; e_i++, i++ ) *( e_i - 1 ) = *( e_i );
  *( e + n - 1 ) = 0.0;

  double f = 0.0;
  double tst1 = 0.0;
  for ( size_t l = 0; l < n; l++ ) {

    // find small subdiagonal element
    tst1 = fmax( tst1, fabs( d[l] ) + fabs( e[l] ) );
    size_t m = l;
    while ( m < n ) {
      if ( fabs( e[m] ) <= DBL_EPSILON * tst1 ) break;
      m++;
  }

    // if m == l, d[l] is an eigenvalue, otherwise, iterate.
    size_t iter = 0;
    if ( m > l ) {
      for ( iter = 1; iter <= MAXITER; iter++ ) {

        // compute implicit shift
        double g = d[l];
        double p = ( d[l + 1] - g ) / ( 2.0 * e[l] );
        double r = hypot( p, 1.0 );
        if ( p < 0 ) r = -r;
        d[l] = e[l] / ( p + r );
        d[l + 1] = e[l] * ( p + r );
        double dl1 = d[l + 1];
        double h = g - d[l];
        for ( size_t i = l + 2; i < n; i++ ) d[i] -= h;
        f = f + h;

        // implicit QL transformation
        p = d[m];
        double c = 1.0;
        double c2 = c;
        double c3 = c;
        double el1 = e[l + 1];
        double s = 0.0;
        double s2 = 0.0;
        for ( long long lli = m - 1; lli >= ( long long ) l; lli-- ) {
          size_t i = ( size_t )( lli );
          c3 = c2;
          c2 = c;
          s2 = s;
          g = c * e[i];
          h = c * p;
          r = hypot( p, e[i] );
          e[i + 1] = s * r;
          s = e[i] / r;
          c = p / r;
          p = c * d[i] - s * g;
          d[i + 1] = h + s * ( c * g + s * d[i] );

          // accumulate transformation
          double *V_k = V;
          for ( size_t k = 0; k < n; V_k += n, k++ ) {
            h = *( V_k + i + 1 );
            *( V_k + i + 1 ) = s * *( V_k + i ) + c * h;
            *( V_k + i ) = c * *( V_k + i ) - s * h;
          }
        }
        p = -s * s2 * c3 * el1 * e[l] / dl1;
        e[l] = s * p;
        d[l] = c * p;

        // check for convergence
        if ( fabs( e[l] ) <= DBL_EPSILON * tst1 ) break;
      }
    }
    if ( iter > MAXITER ) return retval = -1;
    d[l] = d[l] + f;
    e[l] = 0.0;
  }

  // sort eigenvalues and corresponding vectors
  for ( size_t i = 0; i < n - 1; i++ ) {
    size_t k = i;
    double p = d[i];
    for ( size_t j = i + 1; j < n; j++ ) {
      if ( d[j] > p ) {
        k = j;
        p = d[j];
      }
    }
    if ( k != i ) {
      d[k] = d[i];
      d[i] = p;
      double *V_j = V;
      for ( size_t j = 0; j < n; V_j += n, j++ ) {
        p = *( V_j + i );
        *( V_j + i ) = *( V_j + k );
        *( V_j + k ) = p;
      }
    }
  }
  return retval;
} // Diagonalize


static int Eigen_Value_Decomposition( double** V, double* D, size_t n )
{
  int retval = 0;
  double* e = getvector( n, 0.0 );
  retval = Tridiagonalize( &V[1][1], &D[1], &e[1], n );
  if ( retval == 0 ) retval = Diagonalize( &V[1][1], &D[1], &e[1], n );
  freevector( e );
  return retval;
} // Eigen_Value_Decomposition


int evdcmp( const size_t n, double** vecs, double* vals )
// eigenvalue decomposition a = vwv'
// matrix vecs is replaced by the eigenvector matrix vecs
// the eigenvalues are returned in vector vals
{
  int retval = 0;

  retval = Eigen_Value_Decomposition( vecs, vals, n );

  return retval;
} // evdcmp


int jacobi( const size_t n, double** vecs, double* vals, const size_t m )
// truncated eigenvalue decomposition of symmetric n by n matrix a
// eigenvectors returned in vecs and eigenvalues returned in vals
// number of components of vecs and proper values in vals equals m
{
  // convergence constants
  const size_t MINSWEEPS = 4;      // should be enough
  const size_t MAXSWEEPS = 64;     // should be enough
  const double EPS = DBL_EPSILON;  // 2.2204460492503131e-16

  // initialize vecs to identity matrix
  double** a = getmatrix( n, n, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) {
      a[i][j] = vecs[i][j];
      vecs[i][j] = 0.0;
    }
    vecs[i][i] = 1.0;
  }

  // administration
  // sum-of-squares eigenvalues decreasingly increases over sweeps
  double fold = 0.0;
  for ( size_t i = 1; i <= m; i++ ) fold += a[i][i] * a[i][i];

  // main loop for sweeps
  size_t sweep = 0;
  for ( sweep = 1; sweep <= MAXSWEEPS; sweep++ ) {

    // loop over columns / components
    for ( size_t j = 1; j <= m; j++ ) {

      // loop over rows
      for ( size_t i = j + 1; i <= n; i++ ) {

        const double p = a[i][j];
        if ( p < EPS && p > -EPS ) continue;  // continue at true zero. only after 4 sweeps?

        // set constants
        const double q = a[i][i];
        const double r = a[j][j];
        const double d = 0.5 * ( q - r );
        const double t = -1.0 * d / sqrt( d * d + p * p );
        const double uu = 0.5 * ( 1.0 + t );
        const double u = sqrt( uu );
        const double vv = 0.5 * ( 1.0 - t );
        const double v = ( p < 0.0 ? -1.0 : 1.0 ) * sqrt( vv );
        const double twouvp = 2.0 * u * v * p;

        // rotate, using information from the lower triangle of a only, in three steps

        // step 1: case of rotations 1 <= k <= j
        for ( size_t k = 1; k <= j; k++ ) {
          const double ik = a[i][k];
          const double jk = a[j][k];
          a[i][k] = u * ik - v * jk;
          a[j][k] = v * ik + u * jk;
        }

        // step 2: case of rotations j < k < i
        for ( size_t k = j + 1; k < i; k++ ) {
          const double ik = a[i][k];
          const double kj = a[k][j];
          a[i][k] = u * ik - v * kj;
          a[k][j] = v * ik + u * kj;
        }

        // step 3: case of rotations i <= k <= n
        for ( size_t k = i; k <= n; k++ ) {
          const double ki = a[k][i];
          const double kj = a[k][j];
          a[k][i] = u * ki - v * kj;
          a[k][j] = v * ki + u * kj;
        }

        // set eigenvectors: can be skipped if not needed - not much time gain though
        for ( size_t k = 1; k <= n; k++ ) {
          const double ki = vecs[k][i];
          const double kj = vecs[k][j];
          vecs[k][i] = u * ki - v * kj;
          vecs[k][j] = v * ki + u * kj;
        }

        // update a: diagonal and elements
        a[i][i] = uu * q + vv * r - twouvp;
        a[j][j] = vv * q + uu * r + twouvp;
        a[i][j] = u * v * ( q - r ) + ( uu - vv ) * p;
      }
    }

    // administration: always update fold
    // sum-of-squares eigenvalues decreasingly increases over sweeps
    double fnew = 0.0;
    for ( size_t i = 1; i <= m; i++ ) fnew += a[i][i] * a[i][i];
    if ( sweep > MINSWEEPS ) if ( ( fnew - fold ) < EPS ) break;
    fold = fnew;
  }

  // set eigenvalues: garbage after m values
  for ( size_t i = 1; i <= n; i++ ) vals[i] = a[i][i];

  // de-allocate memory
  freematrix( a );

  // proper return value
  return( sweep <= MAXSWEEPS ? 0 : 1 );
} // jacobi


static int solve1x1( const size_t n, double** a, double* b )
{
  int retval = 0;
  if ( n != 1 ) return retval = 1;
  double det = a[1][1];
  const double b1 = b[1];
  if ( iszero( det ) ) {
    if ( iszero( b1 ) ) return retval = 3;
    else return retval = 2;
  }
  b[1] = b1 / det;
  return retval;
} // solve1x1


static int solve2x2( const size_t n, double** a, double* b )
{
  int retval = 0;
  if ( n != 2 ) return retval = 1;
  const double det = a[2][2] * a[1][1] - a[1][2] * a[2][1];
  if ( iszero( det ) ) {
    if ( iszero( b[1] ) && iszero( b[2] ) ) return retval = 3;
    else return retval = 2;
  }
  const double detx = a[2][2] * b[1] - a[1][2] * b[2];
  const double dety = a[1][1] * b[2] - a[2][1] * b[1];
  b[1] = detx / det;
  b[2] = dety / det;
  return retval;
} // solve2x2


static int solve3x3( const size_t n, double** a, double* b )
{
  int retval = 0;
  if ( n != 3 ) return retval = 1;
  const double a11 = a[1][1];
  const double a12 = a[1][2];
  const double a13 = a[1][3];
  const double a21 = a[2][1];
  const double a22 = a[2][2];
  const double a23 = a[2][3];
  const double a31 = a[3][1];
  const double a32 = a[3][2];
  const double a33 = a[3][3];
  const double det = a11 * ( a22 * a33 - a32 * a23 ) - a12 * ( a21 * a33 - a23 * a31 ) + a13 * ( a21 * a32 - a22 * a31 );
  const double b1 = b[1];
  const double b2 = b[2];
  const double b3 = b[3];
  const double detx = b1 * ( a22 * a33 - a32 * a23 ) - a12 * ( b2 * a33 - a23 * b3 ) + a13 * ( b2 * a32 - a22 * b3 );
  const double dety = a11 * ( b2 * a33 - b3 * a23 ) - b1 * ( a21 * a33 - a23 * a31 ) + a13 * ( a21 * b3 - b2 * a31 );
  const double detz = a11 * ( a22 * b3 - a32 * b2 ) - a12 * ( a21 * b3 - b2 * a31 ) + b1 * ( a21 * a32 - a22 * a31 );
  if ( iszero( det ) ) {
    if ( iszero( detx ) && iszero( dety ) && iszero( detz ) ) return retval = 3;
    else return retval = 2;
  }
  b[1] = detx / det;
  b[2] = dety / det;
  b[3] = detz / det;
  return 0;
} // solve3x3


static int solve4x4( const size_t n, double** a, double* b )
{
  int retval = inverse4x4( n, a );
  if ( 0 != retval ) return retval;
  double* c = getvector( n, 0.0 );
  copyall( n, &b[1], &c[1] );
  gemv( false, n, n, 1.0, a, c, 0.0, b );
  freevector( c );
  return retval;
} // solve4x4


static int chdcmp( double* A, size_t n )
{
  double *p_Lk0 = A;
  for ( size_t k = 0; k < n; p_Lk0 += n, k++ ) {
    double *p_Lkk = p_Lk0 + k;
    double *p_Lkp = p_Lk0;
    for ( size_t p = 0; p < k; p_Lkp += 1, p++ ) *p_Lkk -= *p_Lkp * *p_Lkp;
    if ( *p_Lkk <= 0.0 ) return -1;
    *p_Lkk = sqrt( *p_Lkk );
    double reciprocal = 1.0 / *p_Lkk;
    double *p_Li0 = p_Lk0 + n;
    for ( size_t i = k + 1; i < n; p_Li0 += n, i++ ) {
      for ( size_t p = 0; p < k; p++ ) *( p_Li0 + k ) -= *( p_Li0 + p ) * *( p_Lk0 + p );
      *( p_Li0 + k ) *= reciprocal;
      *( p_Lk0 + i ) = *( p_Li0 + k );
    }
  }
  return 0;
} // chdcmp


static int chsolve( double* LU, double* x, size_t n )
{
  double* L = LU;
  for ( size_t k = 0; k < n; L += n, k++ ) {
    if ( iszero( *( L + k ) ) ) return -1;  // The matrix L is singular
    for ( size_t i = 0; i < k; i++ ) x[k] -= x[i] * *( L + i );
    x[k] /= *( L + k );
  }
  LU += n * ( n - 1 );
  for ( long long k = n - 1; k >= 0; LU -= n, k-- ) {
    if ( iszero( *( LU + k ) ) ) return -1;            // The matrix U is singular
    for ( size_t i = k + 1; i < n; i++ ) x[k] -= x[i] * *( LU + i );
    x[k] /= *( LU + k );
  }
  return 0;
}// chsolve


static int llsolve( const size_t n, double** a, double* b )
{
  int retval = 0;
  retval = chdcmp( &a[1][1], n );
  if ( retval == 0 ) {
    retval = chsolve( &a[1][1], &b[1], n );
  }
  return retval;
} // llsolve


static int Choleski_LDU_Decomposition( double* A, size_t n )
{
  double *p_i = A + n;
  for ( size_t i = 1; i < n; p_i += n, i++ ) {
    double *p_j = A;
    for ( size_t j = 0; j < i; j++, p_j += n ) {
      for ( size_t k = 0; k < j; k++ ) *( p_i + j ) -= *( p_i + k ) * *( p_j + k );
    }
    double *p_k = A;
    for ( size_t k = 0; k < i; p_k += n, k++ ) {
      double ld = *( p_i + k ) / *( p_k + k );
      *( p_i + i ) -= *( p_i + k ) * ld;
      *( p_i + k ) = ld;
      *( p_k + i ) = ld;
    }
    if ( *( p_i + i ) <= 0.0 ) return -1;
  }
  return 0;
} // Choleski_LDU_Decomposition


static void Unit_Lower_Triangular_Solve( double* L, double* B, double* x, size_t n )
// Solve the linear equation Lx = B for x, where L is a unit lower triangular matrix.
{
  x[0] = B[0];
  L += n;
  for ( size_t k = 1; k < n; L += n, k++ ) {
    x[k] = B[k];
    for ( size_t i = 0; i < k; i++ ) x[k] -= x[i] * *( L + i );
  }
} // Unit_Lower_Triangular_Solve


static void Unit_Upper_Triangular_Solve( double* U, double* B, double* x, size_t n )
// Solve the linear equation Ux = B for x, where U is an upper triangular matrix.
{
  x[n - 1] = B[n - 1];
  U += n * ( n - 2 );
  for ( long long k = n - 2; k >= 0; U -= n, k-- ) {
    x[k] = B[k];
    for ( size_t i = k + 1; i < n; i++ ) x[k] -= x[i] * *( U + i );
  }
} // Unit_Upper_Triangular_Solve


static int Choleski_LDU_Solve( double* LDU, double* B, double* x, size_t n )
{
  Unit_Lower_Triangular_Solve( LDU, B, x, n );
  double *p_k = LDU;
  for ( size_t k = 0; k < n; k++, p_k += n ) {
    if ( iszero( *( p_k + k ) ) ) return -1;
    x[k] /= *( p_k + k );
  }
  Unit_Upper_Triangular_Solve( LDU, x, x, n );
  return 0;
} // Choleski_LDU_Solve


static int ldlsolve( const size_t n, double** a, double* b )
{
  int retval = 0;
  retval = Choleski_LDU_Decomposition( &a[1][1], n );
  if ( retval == 0 ) {
    retval = Choleski_LDU_Solve( &a[1][1], &b[1], &b[1], n );
  }
  return retval;
} // ldlsolve


static int solve( const size_t n, double** a, double* b )
{
  int retval = 1;
  double* save = getvector( n, 0.0 );
  copyall( n, &b[1], &save[1] );
  if ( n == 1 ) retval = solve1x1( n, a, b );
  if ( n == 2 ) retval = solve2x2( n, a, b );
  if ( n == 3 ) retval = solve3x3( n, a, b );
  if ( n == 4 ) retval = solve4x4( n, a, b );

  if ( 0 != retval ) {
    if ( n <= 5 ) copyall( n, &save[1], &b[1] );
    retval = llsolve( n, a, b );
    if ( 0 != retval ) {
      copyall( n, &save[1], &b[1] );
      retval = ldlsolve( n, a, b );
      // if ( 0 != retval ) {
      //   dcopy( n, &save[1], 1, &b[1], 1 );
      //   retval = lusolve( n, a, b );
      //   if ( 0 != retval ) {
      //     dcopy( n, &save[1], 1, &b[1], 1 );
      //     retval = evsolve( n, a, b );
      //   }
      // }
    }
  }
  freevector( save );
  return retval;
} // solve

// option to remove
int nnls( const size_t m, const size_t n, double** const ca, double* const x, double* const cb, size_t *MAXITER, double *FCRIT )
// algorithm NNLS: nonnegative least squares
// given an m by n matrix, a, and an m-vector, b,
// compute an n-vector, x, that solves the least squares problem ax = b subject to x >= 0
// returns norm ax-b in rnorm
// the original version of this code was developed by Charles L. Lawson and Richard J. Hanson at Jet Propulsion Laboratory 1972 dec 15
// published in the book "SOLVING LEAST SQUARES PROBLEMS", Prentice-HalL, 1974
// revised feb 1995 to accompany reprinting of the book by SIAM
{
  // fast return
  int info = 0;
  if ( m == 0 || n == 0 ) return info = 1;

  // variables
  size_t iz = 0, j = 0, jj = 0, k = 0;
  double** a = getmatrix( m, n, 0.0 );
  copyall( m * n, &ca[1][1], &a[1][1] );
  double* b = getvector( m, 0.0 );
  copyall( m, &cb[1], &b[1] );
  double* w = getvector( n, 0.0 );
  double* zz = getvector( m, 0.0 );
  size_t * index = getvector_t( n, ( size_t )( 0 ) );

  // initialization
  size_t iter = 0;
  if ( *MAXITER == 0 ) *MAXITER = ( n < 3 ? 3 * n : n * n );
  double up = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    x[i] = 0.0;
    index[i] = i;
  }
  size_t iz2 = n;
  size_t iz1 = 1;
  size_t nsetp = 0;
  size_t npp1 = 1;

  // main loop begins here
  // quit if all coefficients are already in the solution or if m cols of a have been triangularized
  while ( iz1 <= iz2 && nsetp < m ) {

    // compute components of the dual (negative gradient) vector W
    for ( iz = iz1; iz <= iz2; iz++ ) {
      j = index[iz];
      double sm = 0.0;
      for ( size_t l = npp1; l <= m; l++ ) sm += a[l][j] * b[l];
      w[j] = sm;
    }

    double wmax = 0.0;
    for ( ;; ) {

      // find largest positive wj
      size_t izmax = 0;
      wmax = 0.0;
      for ( iz = iz1; iz <= iz2; iz++ ) {
        j = index[iz];
        if ( w[j] > wmax ) {
          wmax = w[j];
          izmax = iz;
        }
      }

      // if wmax <= 0.0 goto termination
      // this indicates satisfaction of the kuhn-tucker conditions
      if ( wmax < 0.0 || iszero( wmax ) ) break;
      iz = izmax;
      j = index[iz];

      // the sign of W(j) is ok for j to be moved to set p
      // begin the transformation and check new diagonal element to avoid near linear dependence
      double asave = a[npp1][j];

      // construct the transformation (h12)
      double cl = fabs( a[npp1][j] );
      for ( size_t hj = npp1 + 1; hj <= m; hj++ ) cl = fmax( fabs( a[hj][j] ), cl );
      if ( cl > 0.0 ) {
        double clinv = 1.0 / cl;
        double d1 = a[npp1][j] * clinv;
        double sm = d1 * d1;
        for ( size_t hj = npp1 + 1; hj <= m; hj++ ) {
          d1 = a[hj][j] * clinv;
          sm += d1 * d1;
        }
        cl *= sqrt( sm );
        if ( a[npp1][j] > 0.0 ) cl = -cl;
        up = a[npp1][j] - cl;
        a[npp1][j] = cl;
      }

      double unorm = 0.0;
      if ( nsetp != 0 ) for ( size_t l = 1; l <= nsetp; l++ ) unorm += a[l][j] * a[l][j];
      unorm = sqrt( unorm );
      double d2 = unorm + fabs( a[npp1][j] ) * 0.01;
      if ( ( d2 - unorm ) > 0.0 ) {

        // col j is sufficiently independent
        // copy b into zz, update zz and solve for ztest (= proposed new value for xj)
        for ( size_t l = 1; l <= m; l++ ) zz[l] = b[l];

        // construct the transformation (h12)
        if ( fabs( a[npp1][j] ) > 0.0 ) {
          double hb = up * a[npp1][j];
          if ( isnotzero( hb ) ) {
            double sm = zz[npp1] * up;
            for ( k = npp1 + 1; k <= m; k++ ) sm += zz[k] * a[k][j];
            if ( isnotzero( sm ) ) {
              sm /= hb;
              zz[npp1] += sm * up;
              for ( k = npp1 + 1; k <= m; k++ ) zz[k] += a[k][j] * sm;
            }
          }
        }
        if ( ( zz[npp1] / a[npp1][j] ) > 0.0 ) break;
      }

      // reject j as a candidate to be moved from set z to set p
      // restore a npp1 j, set wj = 0 and loop back to test dual coeffs again
      a[npp1][j] = asave;
      w[j] = 0.0;
    }
    if ( wmax <= 0.0 ) break;

    // the index j = index[iz] has been selected to be moved from set Z to set p
    // update b, update indices, apply householder transformations to cols in new set z
    // zero subdiagonal elts in col j, set wj = 0
    for ( size_t l = 1; l <= m; l++ ) b[l] = zz[l];
    index[iz] = index[iz1];
    index[iz1] = j;
    iz1++;
    nsetp = npp1;
    npp1++;
    if ( iz1 <= iz2 ) for ( size_t jz = iz1; jz <= iz2; jz++ ) {
      jj = index[jz];
      if ( fabs( a[nsetp][j] ) > 0.0 ) {
        double hb = up * a[nsetp][j];
        if ( isnotzero( hb ) ) {
          double sm = a[nsetp][jj] * up;
          for ( k = npp1; k <= m; k++ ) sm += a[k][jj] * a[k][j];
          if ( isnotzero( sm ) ) {
            sm /= hb;
            a[nsetp][jj] += sm * up;
            for ( k = npp1; k <= m; k++ ) a[k][jj] += a[k][j] * sm;
          }
        }
      }
    }
    if ( nsetp != m ) for ( size_t l = npp1; l <= m; l++ ) a[l][j] = 0.0;
    w[j] = 0.0;

    // solve the triangular system; store the solution temporarily in z
    for ( size_t l = 1; l <= nsetp; l++ ) {
      size_t ip = nsetp + 1 - l;
      if ( l != 1 ) for ( size_t ii = 1; ii <= ip; ii++ ) zz[ii] -= a[ii][jj] * zz[ip + 1];
      jj = index[ip];
      zz[ip] /= a[ip][jj];
    }

    // secondary loop begins here
    for ( iter = 1; iter <= *MAXITER; iter++ ) {

      // see if all new constrained coeffs are feasible; if not, compute alpha
      double alpha = 2.0;
      for ( size_t ip = 1; ip <= nsetp; ip++ ) {
        size_t l = index[ip];
        if ( zz[ip] <= 0.0 ) {
          double t = -x[l] / ( zz[ip] - x[l] );
          if ( alpha > t ) {
            alpha = t;
            jj = ip;
          }
        }
      }

      // if all new constrained coeffs are feasible then still alpha == 2
      // if so, then exit from the secondary loop to main loop
      if ( isequal( alpha, 2.0 ) ) break;

      // use alpha (0.0 < alpha < 1.0) to interpolate between old x and new zz
      for ( size_t ip = 1; ip <= nsetp; ip++ ) {
        size_t l = index[ip];
        x[l] += alpha * ( zz[ip] - x[l] );
      }

      // modify a and b and the index arrays to move coefficient k from set p to set z
      k = index[jj];
      bool pfeasible = true;
      while ( pfeasible ) {
        x[k] = 0.0;
        if ( jj != nsetp ) {
          jj++;
          for ( j = jj; j <= nsetp; j++ ) {
            size_t ii = index[j];
            index[j - 1] = ii;
            double cc = 0.0;
            double ss = 0.0;
            if ( fabs( a[j - 1][ii] ) > fabs( a[j][ii] ) ) {
              double xr = a[j][ii] / a[j - 1][ii];
              double yr = sqrt( 1.0 + xr * xr );
              cc = sign( 1.0 / yr, a[j - 1][ii] );
              ss = cc * xr;
              a[j - 1][ii] = fabs( a[j - 1][ii] ) * yr;
            }
            else {
              if ( isnotzero( a[j][ii] ) ) {
                double xr = a[j - 1][ii] / a[j][ii];
                double yr = sqrt( 1.0 + xr * xr );
                ss = sign( 1.0 / yr, a[j][ii] );
                cc = ss * xr;
                a[j - 1][ii] = fabs( a[j][ii] ) * yr;
              }
              else {
                a[j - 1][ii] = 0.0;
                cc = 0.0;
                ss = 1.0;
              }
            }
            a[j][ii] = 0.0;
            for ( size_t l = 1; l <= n; l++ ) {
              if ( l != ii ) {
                double temp = a[j - 1][l];
                a[j - 1][l] = cc * temp + ss * a[j][l];
                a[j][l] = -ss * temp + cc * a[j][l];
              }
            }
            double temp = b[j - 1];
            b[j - 1] = cc * temp + ss * b[j];
            b[j] = -ss * temp + cc * b[j];
          }
        }
        npp1 = nsetp;
        nsetp--;
        iz1--;
        index[iz1] = k;

        // see if the remaining coeffs in set P are feasible
        // they should be because of the way alpha was determined
        // if any are infeasible it is due to round-off error
        // any that are nonpositive will be set to zero and moved from set p to set z
        pfeasible = false;
        for ( jj = 1; jj <= nsetp; jj++ ) {
          k = index[jj];
          if ( x[k] <= 0.0 ) {
            pfeasible = true;
            break;
          }
        }
      } // while pfeasible == true

        // copy b into zz, then solve again and loop back
      for ( k = 1; k <= m; k++ ) zz[k] = b[k];
      for ( size_t l = 1; l <= nsetp; l++ ) {
        size_t ip = nsetp + 1 - l;
        if ( l != 1 ) for ( size_t ii = 1; ii <= ip; ii++ ) zz[ii] -= a[ii][jj] * zz[ip + 1];
        jj = index[ip];
        zz[ip] /= a[ip][jj];
      }
    } // end of secondary loop

    if ( iter >= *MAXITER ) {
      info = 1;
      break;
    }

    for ( size_t ip = 1; ip <= nsetp; ip++ ) {
      k = index[ip];
      x[k] = zz[ip];
    }
  } // end of main loop

  // just for testing
  *MAXITER = iter;
  double sm = 0.0;
  if ( npp1 <= m ) for ( k = npp1; k <= m; k++ ) sm += b[k] * b[k];
  *FCRIT = sqrt( sm );

  // de-allocate memory
  freematrix( a );
  freevector( b );
  freevector( w );
  freevector( zz );
  freevector_t( index );

  return info;
} // nnls


int nnals( const size_t n, const size_t m, double** const x, double* const b, double* const y, size_t *MAXITER, double *FCRIT )
// algorithm NNALS: nonnegative alternating least squares
// convergence on dssq(residuals) using machine precision (DBL_EPSILON)
// maximum number of iterations equals 2**10 = 1024
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08

  if ( *MAXITER == 0 ) *MAXITER = 1024;
  if ( iszero( *FCRIT ) ) *FCRIT = EPS;
  double bj;

  // set residuals: r = y - xb
  double* r = getvector( n, 0.0 );
  gemv( false, n, m, -1.0, x, b, 1.0, r );
  axpy( n, 1.0, &y[1], 1, &r[1], 1 );

  // set weighted sum-of-squares xj
  double* ssqx = getvector( m, 0.0 );
  for ( size_t j = 1; j <= m; j++ ) ssqx[j] = ssq( n, &x[1][j], m );

  // main iteration loop
  double fold = ssq( n, &r[1], 1 );
  double fnew = fold;
  size_t iter = 0;
  for ( iter = 1; iter <= *MAXITER; iter++ ) {

    // variables loop
    for ( size_t j = 1; j <= m; j++ ) {

      // substract contribution of variable j from r
      bj = b[j];
      if ( isnotzero( bj ) ) axpy( n, bj, &x[1][j], m, &r[1], 1 );

      // compute cross-product between xj and r
      double cross = dot( n, &r[1], 1, &x[1][j], m );

      // compute bj
      if ( cross < DBL_EPSILON || ssqx[j] < DBL_EPSILON ) bj = 0.0;
      else bj = cross / ssqx[j];

      // add contribution of variable j to r
      if ( isnotzero( bj ) ) axpy( n, -bj, &x[1][j], m, &r[1], 1 );
      b[j] = bj;
    }

    // administration
    fnew = ssq( n, &r[1], 1 );
    double diff = fold - fnew;
    double sumf = fold + fnew;
    if ( diff < -TOL ) continue;                                      // continue while real divergence
    if ( diff <= *FCRIT * ( sumf + DBL_EPSILON ) / 2.0 ) break; // check ssq residuals
    fold = fnew;
  } // main loop

  // just for testing
  *MAXITER = iter;
  *FCRIT = sqrt( fnew );

  // free memory, and return to caller
  freevector( r );
  freevector( ssqx );
  return 0;
} // nnals

static void lstsq( const size_t m, double** xtx, double* xty, bool* p, double* b )
// solve y = Xb for b and return b
{
  size_t n = 0;
  for ( size_t i = 1; i <= m; i++ ) if ( p[i] == true ) n++;
  if ( n == 0 ) return;
  double** ztz = getmatrix( n, n, 0.0 );
  double* zty = getvector( n, 0.0 );
  for ( size_t i = 1, ii = 0; i <= m; i++ ) if ( p[i] == true ) {
    zty[++ii] = xty[i];
    for ( size_t j = 1, jj = 0; j <= m; j++ ) if ( p[j] == true ) ztz[ii][++jj] = xtx[i][j];
  }
  solve( n, ztz, zty );
  freematrix( ztz );
  for ( size_t i = 1, ii = 0; i <= m; i++ ) b[i] = ( p[i] == true ? zty[++ii] : 0.0 );
  freevector( zty );
} // lstsq

static bool anytruele( const size_t n, double* x, bool* b, const double BOUND )
{
  for ( size_t i = 1; i <= n; i++ ) if ( b[i] == true && x[i] <= BOUND ) return( true );
  return( false );
} // anytruele

static bool anyfalsegt( const size_t n, double* x, bool* b, const double BOUND )
{
  for ( size_t i = 1; i <= n; i++ ) if ( b[i] == false && x[i] > BOUND ) return( true );
  return( false );
} // anyfalsegt

int fastnnls( const size_t n, const size_t m, double** x, double* b, double* y, size_t *MAXITER, double *FCRIT )
// fast nonnegative least squares procedure based on Bro and de Jong (1997)
{
  if ( *MAXITER == 0 ) *MAXITER = 30 * m;

  // allocate memory
  double** xtx = getmatrix( m, m, 0.0 );
  double* xty = getvector( m, 0.0 );
  double* r = getvector( m, 0.0 );         // Lagrange multipliers
  bool* pnew = getbvector( m, false );     // all in passive set
  double* s = getvector( m, 0.0 );         // beta help

  // setup x'x and x'y
  for ( size_t i = 1; i <= m; i++ ) {
    b[i] = 0.0;                            // initialize b to zero (needed for updating)
    double work = 0.0;
    for ( size_t k = 1; k <= n; k++ ) {
      work += x[k][i] * y[k];
    }
    r[i] = xty[i] = work;                  // initial Lagrange multipliers (for b = 0)
    for ( size_t j = 1; j <= m; j++ ) {
      double tmp = 0.0;
      for ( size_t k = 1; k <= n; k++ ) tmp += x[k][i] * x[k][j];
      xtx[i][j] = tmp;
    }
  }

  // set convergence criterion
  double sm = 0.0;
  for ( size_t j = 1; j <= m; j++ ) {
    double work = asum( m, &xtx[1][j], m );
    if ( work > sm ) sm = work;
  }

  if ( iszero( *FCRIT ) ) *FCRIT = 10.0 * ( double )( m ) * sm * DBL_EPSILON;

  // outer loop
  size_t iter = 0;
  while ( anyfalsegt( m, r, pnew, *FCRIT ) ) {
    size_t index = 0;
    double val = -DBL_MAX;
    for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == false && r[i] > val ) {
      val = r[i];
      index = i;
    }
    pnew[index] = true;             // move to active set
    lstsq( m, xtx, xty, pnew, s );  // least squares solution active set

    // inner loop
    while ( anytruele( m, s, pnew, *FCRIT ) && iter < *MAXITER ) {
      iter++;
      double alpha = DBL_MAX;
      for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == true && s[i] <= *FCRIT ) {
        double work = b[i] / ( b[i] - s[i] );
        if ( work < alpha ) alpha = work;
      }
      for ( size_t i = 1; i <= m; i++ ) b[i] += alpha * ( s[i] - b[i] );
      for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == true && b[i] <= *FCRIT ) pnew[i] = false;
      lstsq( m, xtx, xty, pnew, s );
    }
    for ( size_t i = 1; i <= m; i++ ) b[i] = s[i];
    for ( size_t i = 1; i <= m; i++ ) r[i] = xty[i] - dot( m, &xtx[i][1], 1, &b[1], 1 );
  }

  // just for testing
  *MAXITER = iter;
  double rssq = ssq( m, &r[1], 1 );
  *FCRIT = sqrt( rssq );

  // de-allocate memory
  freematrix( xtx );
  freevector( xty );
  freebvector( pnew );
  freevector( r );
  freevector( s );

  return 0;
} // fastnnls

static int fastnnwls( const size_t n, const size_t m, double** x, double* b, double* y, double* w )
// fast nonnegative weighted least squares procedure based on Bro and de Jong (1997)
{
  // set constants
  const size_t MAXITER = 30 * m;

  // allocate memory
  double** xtwx = getmatrix( m, m, 0.0 );
  double* xtwy = getvector( m, 0.0 );
  double* r = getvector( m, 0.0 );         // Lagrange multipliers
  bool* pnew = getbvector( m, false );
  double* s = getvector( m, 0.0 );

  // setup x'wx and x'wy
  for ( size_t i = 1; i <= m; i++ ) {
    b[i] = 0.0;                            // initialize b to zero (needed for updating)
    double work = 0.0;
    for ( size_t k = 1; k <= n; k++ ) work += x[k][i] * w[k] * y[k];
    r[i] = xtwy[i] = work;                 // initial Lagrange multipliers (for b = 0)
    for ( size_t j = 1; j <= m; j++ ) {
      double tmp = 0.0;
      for ( size_t k = 1; k <= n; k++ ) tmp += x[k][i] * w[k] * x[k][j];
      xtwx[i][j] = tmp;
    }
  }

  // set convergence criterion
  double sm = 0.0;
  for ( size_t j = 1; j <= m; j++ ) {
    double work = asum( m, &xtwx[1][j], m );
    if ( work > sm ) sm = work;
  }
  const double TOL = 10.0 * ( double )( m ) * sm * DBL_EPSILON;

  // outer loop
  size_t iter = 0;
  while ( anyfalsegt( m, r, pnew, TOL ) ) {
    size_t index = 0;
    double val = -LDBL_MAX;
    for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == false && r[i] > val ) {
      val = r[i];
      index = i;
    }
    pnew[index] = true;               // move to active set
    lstsq( m, xtwx, xtwy, pnew, s );  // least squares solution active set

    // inner loop
    while ( anytruele( m, s, pnew, TOL ) && iter < MAXITER ) {
      iter++;
      double alpha = DBL_MAX;
      for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == true && s[i] <= TOL ) {
        double work = b[i] / ( b[i] - s[i] );
        if ( work < alpha ) alpha = work;
      }
      for ( size_t i = 1; i <= m; i++ ) b[i] += alpha * ( s[i] - b[i] );
      for ( size_t i = 1; i <= m; i++ ) if ( b[i] <= TOL ) pnew[i] = false;
      lstsq( m, xtwx, xtwy, pnew, s );
    }
    for ( size_t i = 1; i <= m; i++ ) b[i] = s[i];
    for ( size_t i = 1; i <= m; i++ ) r[i] = xtwy[i] - dot( m, &xtwx[i][1], 1, &b[1], 1 );
  }

  // de-allocate memory
  freematrix( xtwx );
  freevector( xtwy );
  freebvector( pnew );
  freevector( r );
  freevector( s );

  return 0;
} // fastnnwls

int nnccd( const size_t n, const size_t m, double** x, double *b, double *y, size_t *MAXITER, double *FCRIT )
// non-negative columnwise coordinate descent
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  if ( *MAXITER == 0 ) *MAXITER = 1024;
  if ( iszero( *FCRIT ) ) *FCRIT = TINY;

  double* r = getvector( n, 0.0 );
  gemv( false, n, m, 1.0, x, b, 1.0, r );
  axpy( n, -1.0, &y[1], 1, &r[1], 1 );

  double* ssqx = getvector( m, 0.0 );
  for ( size_t j = 1; j <= m; j++ ) ssqx[j] = ssq( n, &x[1][j], m );

  size_t iter = 0;
  double check = 0.0;
  for ( iter = 1; iter <= *MAXITER; iter++ ) {
    check = 0.0;
    for ( size_t j = 1; j <= m; j++ ) {
      const double sum = dot( n, &r[1], 1, &x[1][j], m );
      const double change = fmax( -sum / ssqx[j], -b[j] );
      b[j] += change;
      axpy( n, change, &x[1][j], m, &r[1], 1 );
      check = fmax( check, fabs( change ) );
    }
    if ( check < *FCRIT ) break;
  }

  // just for testing
  *MAXITER = iter;
  *FCRIT = check;

  freevector( ssqx );
  freevector( r );

  return 0;
} // nnccd

// static int nnwccd( const size_t n, const size_t m, double** x, double *b, double *y, double* w )
// // non-negative weighted columnwise coordinate descent
// {
//   const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
//   const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
//   const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
//   const size_t MAXITER = 1024;
//   double* r = getvector( n, 0.0 );
//   gemv( false, n, m, 1.0, x, b, 1.0, r );
//   axpy( n, -1.0, &y[1], 1, &r[1], 1 );
//   double* wssqx = getvector( m, 0.0 );
//   for ( size_t j = 1; j <= m; j++ ) wssqx[j] = wssq( n, &x[1][j], m, &w[1], 1 );  // weighted
//   size_t iter = 0;
//   for ( iter = 1; iter <= MAXITER; iter++ ) {
//     double check = 0.0;
//     for ( size_t j = 1; j <= m; j++ ) {
//       const double sum = wdot( n, &r[1], 1, &x[1][j], m, &w[1], 1 );  // weighted
//       const double change = fmax( -sum / wssqx[j], -b[j] );
//       b[j] += change;
//       axpy( n, change, &x[1][j], m, &r[1], 1 );
//       check = fmax( check, fabs( change ) );
//     }
//     if ( check < TINY ) break;
//   }
//   freevector( wssqx );
//   freevector( r );
//   return 0;
// } // nnwccd
// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// geometric functions
//

int ccdu( const size_t n, const size_t m, double** x, double *b, double *y, size_t *MAXITER, double *FCRIT )
{
  double* xb = getvector( n, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) {
    double sum = 0.0;
    for ( size_t j = 1; j <= m; j++ ) sum += x[i][j] * b[j];
    xb[i] = sum;
  }
  double* s = getvector( m, 0.0 );
  for ( size_t j = 1; j <= m; j++ ) {
    double sum = 0.0;
    for ( size_t i = 1; i <= n; i++ ) sum += x[i][j] * x[i][j];
    if ( sum == 0.0 ) s[j] = 1.0;
    else s[j] = sum;
  }
  int itel = 1;
  double meps = 0.0;
  while ( true ) {
    meps = 0.0;
    double chng = 0.0;
    for ( size_t j = 1; j <= m; j++ ) {
      double sum = 0.0;
      for ( size_t i = 1; i <= n; i++ ) sum += x[i][j] * ( xb[i] - y[i] );
      chng = -sum / s[j];
      chng = fmax( -b[j], chng );
      meps = fmax( meps, fabs( chng ) );
      b[j] += chng;
      for ( size_t i = 1; i <= n; i++ ) {
        double sum = 0.0;
        for ( size_t j = 1; j <= m; j++ ) sum += x[i][j] * b[j];
        xb[i] = sum;
      }
    }
    if ( ( itel == *MAXITER ) || ( meps < *FCRIT ) ) break;
    itel = itel + 1;
  }
  freevector( s );
  freevector( xb );
  *MAXITER = itel;
  *FCRIT = meps;
  return 0;
}


// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// transformation functions
//

void nnlinear( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r )
// function: y = a + b * x
// transform x such that min ||a+b*x-y|| for b >= 0.0 and a+b*x >= 0.0 for all x
// return a + b * x in z (only if b > 0)
{
  double xmin = DBL_MAX;
  if ( isnull( w ) ) {
    if ( symmetric == true ) {
      for ( size_t i = 2; i <= n; i++ ) {
        for ( size_t j = 1; j < i; j++ ) if ( x[i][j] < xmin ) xmin = x[i][j];
      }
    }
    else {
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) if ( x[i][j] < xmin ) xmin = x[i][j];
      }
    }
  }
  else {
    if ( symmetric == true ) {
      for ( size_t i = 2; i <= n; i++ ) {
        for ( size_t j = 1; j < i; j++ ) if ( isnotzero( w[i][j] ) && x[i][j] < xmin ) xmin = x[i][j];
      }
    }
    else {
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) if ( isnotzero( w[i][j] ) && x[i][j] < xmin ) xmin = x[i][j];
      }
    }
  }

  double sumw = 0.0;
  double wsumx = 0.0;
  double wsumy = 0.0;
  double wssqx = 0.0;
  double cross = 0.0;
  if ( isnull( w ) ) {
    if ( symmetric == true ) {
      sumw = ( double )( n * ( n - 1 ) / 2 );
      for ( size_t i = 2; i <= n; i++ ) {
        for ( size_t j = 1; j < i; j++ ) {
          const double xi = x[i][j] - xmin;
          const double yi = mconst * y[i][j];
          wsumx += xi;
          wsumy += yi;
          wssqx += xi * xi;
          cross += xi * yi;
        }
      }
    }
    else {
      sumw = ( double )( n * n - n );
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
          const double xi = x[i][j] - xmin;
          const double yi = mconst * y[i][j];
          wsumx += xi;
          wsumy += yi;
          wssqx += xi * xi;
          cross += xi * yi;
        }
      }
    }
  }
  else {
    if ( symmetric == true ) {
      for ( size_t i = 2; i <= n; i++ ) {
        for ( size_t j = 1; j < i; j++ ) {
          const double wi = w[i][j];
          if ( isnotzero( wi ) ) {
            const double xi = x[i][j] - xmin;
            const double yi = mconst * y[i][j];
            sumw += wi;
            wsumx += wi * xi;
            wsumy += wi * yi;
            wssqx += wi * xi * xi;
            cross += wi * xi * yi;
          }
        }
      }
    }
    else {
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
          const double wi = w[i][j];
          if ( isnotzero( wi ) ) {
            const double xi = x[i][j] - xmin;
            const double yi = mconst * y[i][j];
            sumw += wi;
            wsumx += wi * xi;
            wsumy += wi * yi;
            wssqx += wi * xi * xi;
            cross += wi * xi * yi;
          }
        }
      }
    }
  }
  const double work = wssqx * sumw - wsumx * wsumx;
  double b = ( isnotzero( work ) ? ( cross * sumw - wsumx * wsumy ) / work : 0.0 );
  if ( b < 0.0 ) b = 0.0;
  double a = ( wsumy - b * wsumx ) / sumw;
  if ( a < 0.0 ) {
    a = 0.0;
    b = cross / wssqx;
    if ( b < 0.0 ) b = 0.0;
  }
  if ( isnotzero( b ) ) {
    a -= b * xmin;
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) r[i][j] = a + b * x[i][j];
  }
} // nnlinear

static void monotone( const size_t n, double* x, double* w )
// monotone or isotonic regression by pool-adjacent-violators algorithm
// author: Frank M.T.A. Busing
{
  // allocate memory
  double* __restrict rx = &x[0];
  double* __restrict rw = &w[0];
  size_t* __restrict idx = ( size_t* ) calloc( n + 1, sizeof( size_t ) );

  // initialize
  idx[0] = 0;
  idx[1] = 1;
  size_t b = 1;
  double xbm1 = rx[1];
  double wbm1 = rw[1];

  // loop over elements
  for ( size_t i = 2; i <= n; i++ ) {

    // next block
    b++;
    double xb = rx[i];
    double wb = rw[i];

    // check for violation
    if ( xbm1 > xb ) {
      b--;
      double sb = wbm1 * xbm1 + wb * xb;
      wb += wbm1;
      xb = sb / wb;

      // check for up-block violations
      while ( i < n && xb >= x[i + 1] ) {
        i++;
        sb += w[i] * x[i];
        wb += w[i];
        xb = sb / wb;
      }

      // check for down-block violations
      while ( b > 1 && x[b - 1] > xb ) {
        b--;
        sb += w[b] * x[b];
        wb += w[b];
        xb = sb / wb;
      }
    }
    x[b] = xbm1 = xb;
    w[b] = wbm1 = wb;
    idx[b] = i;
  }

  // final expansion
  size_t from = n;
  for ( size_t k = b; k > 0; k-- ) {
    const size_t to = idx[k - 1] + 1;
    const double xk = rx[k];
    for ( size_t i = from; i >= to; i-- ) rx[i] = xk;
    from = to - 1;
  }

  // deallocate memory
  free( idx );

} // monotone

static size_t getindex( const size_t n, const size_t row, const size_t col )
{
  return ( row - 1 ) * n + col;
}

static size_t rowindex( const size_t n, const size_t index )
{
  return 1 + ( index - 1 ) / n;
}

static size_t colindex( const size_t n, const size_t index)
{
  return 1 + ( index - 1 ) % n;
}

size_t setindices( const bool symmetric, const size_t n, double** delta, double** w, size_t* index, size_t* ntb, size_t* tbl )
{
  // set list length including missing values
  const size_t nn = ( symmetric == true ? n * ( n - 1 ) / 2 : n * n - n );

  // allocate memory
  double* vdelta = getvector( nn, 0.0 );
  double* vw = getvector( nn, 0.0 );

  // set index
  size_t count = 0;
  size_t last = nn;
  if ( symmetric == true ) {
    for ( size_t i = 2, k = 1; i <= n; i++ ) {
      for ( size_t j = 1; j < i; j++, k++ ) {
        if ( isnull( w ) || isnotzero( w[i][j] ) ) {
          count++;
          index[count] = getindex( n, i, j );
          vdelta[count] = delta[i][j];
        }
        else {
          index[last] = getindex( n, i, j );
          last--;
        }
      }
    }
  }
  else {
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
        if ( isnull( w ) || isnotzero( w[i][j] ) ) {
          count++;
          index[count] = getindex( n, i, j );
          vdelta[count] = delta[i][j];
        }
        else {
          index[last] = getindex( n, i, j );
          last--;
        }
      }
    }
  }

  // sort vector and transfer indices
  dsort( count, vdelta, index );

  // create tieblocks
  *ntb = 1;
  tbl[*ntb] = 1;
  for ( size_t i = 2; i <= count; i++ ) {
    if ( isequal( vdelta[i], vdelta[i - 1] ) ) tbl[*ntb]++;
    else {
      (*ntb)++;
      tbl[*ntb] = 1;
    }
  }

  // de-allocate memory
  freevector( vdelta );
  freevector( vw );

  // return number of non-missing elements
  return count;
}

void ordinal1( const bool symmetric, const size_t n, double** d, const double mconst, double** w, const size_t count, size_t* index, const size_t ntb, size_t* tbl, double** gamma )
// ordinal transformation untying ties
{
  double* vd = getvector( count, 0.0 );
  double* vw = getvector( count, 1.0 );
  for ( size_t k = 1; k <= count; k++ ) {
    const size_t row = rowindex( n, index[k] );
    const size_t col = colindex( n, index[k] );
    vd[k] = mconst * d[row][col];
    if ( isnotnull( w ) ) vw[k] = w[row][col];
  }
  for ( size_t b = 1, k = 0; b <= ntb; b++ ) {
    if ( tbl[b] > 1 ) dsort( tbl[b], &vd[k], &index[k] );  // index sorted but within tie-block thus should be okay
    if ( isnotnull( w ) ) for ( size_t j = 1; j <= tbl[b]; j++ ) vw[k+j] = w[rowindex( n, index[k+j] )][colindex( n, index[k+j] )];
    k += tbl[b];
  }
  monotone( count, vd, vw );
  freevector( vw );
  if ( symmetric == true ) {
    for ( size_t k = 1; k <= count; k++ ) {
      const size_t row = rowindex( n, index[k] );
      const size_t col = colindex( n, index[k] );
      gamma[row][col] = gamma[col][row] = vd[k];
    }
  }
  else {
    for ( size_t k = 1; k <= count; k++ ) {
      const size_t row = rowindex( n, index[k] );
      const size_t col = colindex( n, index[k] );
      gamma[row][col] = vd[k];
    }
  }
  freevector( vd );
} // ordinal1

void ordinal2( const bool symmetric, const size_t n, double** d, const double mconst, double** w, const size_t count, size_t* index, const size_t ntb, size_t* tbl, double** gamma )
// ordinal transformation keeping ties tied
{
  double* vd = getvector( ntb, 0.0 );
  double* vw = getvector( ntb, 1.0 );
  for ( size_t b = 1, k = 1; b <= ntb; b++ ) {
    double d1 = 0.0;
    double d2 = 0.0;
    for ( size_t j = 1; j <= tbl[b]; j++, k++ ) {
      const size_t row = rowindex( n, index[k] );
      const size_t col = colindex( n, index[k] );
      if ( isnull( w ) ) {
        d1 += mconst * d[row][col];
        d2 += 1.0;
      }
      else {
        d1 += w[row][col] * mconst * d[row][col];
        d2 += w[row][col];
      }
    }
    if ( iszero( d2 ) ) {
      vd[b] = 0.0;
      vw[b] = 0.0;
    }
    else {
      vd[b] = d1 / d2;
      vw[b] = d2;
    }
  }
  monotone( ntb, vd, vw );
  freevector( vw );
  if ( symmetric == true ) {
    for ( size_t b = 1, k = 1; b <= ntb; b++ ) for ( size_t j = 1; j <= tbl[b]; j++, k++ ) {
      const size_t row = rowindex( n, index[k] );
      const size_t col = colindex( n, index[k] );
      gamma[row][col] = gamma[col][row] = vd[b];
    }
  }
  else {
    for ( size_t b = 1, k = 1; b <= ntb; b++ ) for ( size_t j = 1; j <= tbl[b]; j++, k++ ) {
      const size_t row = rowindex( n, index[k] );
      const size_t col = colindex( n, index[k] );
      gamma[row][col] = vd[b];
    }
  }
  freevector( vd );
} // ordinal2

static size_t unique( const size_t n, double* x, double* w )
{
  // skip missings
  size_t count = 0;
  for ( size_t i = 1; i <= n; i++ ) {
    if ( isnotzero( w[i] ) ) {
      count++;
      x[count] = x[i];
    }
  }

  // sort data
  size_t* index = getvector_t( count, ( size_t )( 0 ) );
  dsort( count, x, index );
  freevector_t( index );

  // skip duplicates
  size_t size = 1;
  x[size] = x[1];
  for ( size_t i = 2; i <= count; i++ ) {
    if ( isnotequal( x[i], x[i - 1] ) ) {
      size++;
      x[size] = x[i];
    }
  }
  return size;
} // unique

static double quantile( const size_t n, double* x, const double p, const int type )
// reference: Hyndman and Fan (1996)
{
  double alpha = 1.0;
  double beta = 1.0;

  if ( type == 4 ) alpha = 0.0;
  else if ( type == 5 ) alpha = beta = 0.5;
  else if ( type == 6 ) alpha = beta = 0.0;
  else if ( type == 8 ) alpha = beta = 1.0 / 3.0;
  else if ( type == 9 ) alpha = beta = 3.0 / 8.0;

  const double nppm = alpha + p * ( ( double )( n + 1 ) - alpha - beta );
  const double j = floor( nppm + DBL_EPSILON );
  const double gamma = nppm - j;
  const size_t j_t = ( size_t )( j );

  if ( j_t == 0 ) return( x[1] );
  else if ( j_t >= n ) return( x[n] );
  else return( ( 1.0 - gamma ) * x[j_t] + gamma * x[j_t + 1] );

} // quantile

static double* getboundaries( const size_t n, double* x, double* w, const bool anchor )
{
  double xmin = DBL_MAX;
  double xmax = -DBL_MAX;
  for ( size_t i = 1; i <= n; i++ ) if ( isnotzero( w[i] ) ) {
    if ( x[i] < xmin ) xmin = x[i];
    if ( x[i] > xmax ) xmax = x[i];
  }
  double* boundaries = getvector( 2, 0.0 );
  boundaries[1] = ( anchor == true ? xmin : 0.0 );
  boundaries[2] = xmax;
  return( boundaries );
} // getboundaries

static bool checkandorderknotsequence( const size_t ninner, double* iknots, double* boundaries )
{
  const double xmin = boundaries[1];
  const double xmax = boundaries[2];
  size_t* index = getvector_t( ninner, ( size_t )( 0 ) );
  dsort( ninner, iknots, index );
  freevector_t( index );
  if ( iknots[1] < xmin ) return false;
  if ( iknots[ninner] > xmax ) return false;
  return true;
} // checkknotsequence

static void knotsequenceinterval( const size_t n, double* x, double* boundaries, const size_t ninner, double* iknots )
{
  if ( ninner == 0 ) return;
  const double xmin = boundaries[1];
  const double xmax = boundaries[2];
  const double xrange = xmax - xmin;
  const double interval = xrange / ( double )( ninner + 1 );
  for ( size_t k = 1; k <= ninner; k++ ) iknots[k] = xmin + ( double )( k ) * interval;
} // knotsequenceinterval

static void knotsequencepercentile( const size_t n, double* x, double* w, const size_t ninner, double* iknots )
{
  if ( ninner == 0 ) return;
  double* tmpx = getvector( n, 0.0 );
  copyall( n, &x[1], &tmpx[1] );
  const size_t m = unique( n, tmpx, w );
  for ( size_t k = 1; k <= ninner; k++ ) {
    const double q = ( double )( k ) / ( double )( ninner + 1 );
    iknots[k] = quantile( m, tmpx, q, 7 );
  }
  freevector( tmpx );
} // knotsequencepercentile

static void knotsequencemidpercentile( const size_t n, double* x, double* w, const size_t ninner, double* iknots )
{
  if ( ninner == 0 ) return;
  double* tmpx = getvector( n, 0.0 );
  copyall( n, &x[1], &tmpx[1] );
  const size_t m = unique( n, tmpx, w );
  if ( ninner == 1 ) iknots[1] = 0.5;
  else if ( ninner == 2 ) {
    iknots[1] = 1.0/3.0;
    iknots[2] = 2.0/3.0;
  }
  else if ( ninner == 3 ) {
    iknots[1] = 0.05;
    iknots[2] = 0.5;
    iknots[3] = 0.95;
  }
  else if ( ninner == 4 ) {
    iknots[1] = 0.05;
    iknots[2] = 0.35;
    iknots[3] = 0.65;
    iknots[4] = 0.95;
  }
  else if ( ninner == 5 ) {
    iknots[1] = 0.05;
    iknots[2] = 0.275;
    iknots[3] = 0.5;
    iknots[4] = 0.725;
    iknots[5] = 0.95;
  }
  else if ( ninner == 6 ) {
    iknots[1] = 0.05;
    iknots[2] = 0.23;
    iknots[3] = 0.41;
    iknots[4] = 0.59;
    iknots[5] = 0.77;
    iknots[6] = 0.95;
  }
  else if ( ninner == 7 ) {
    iknots[1] = 0.025;
    iknots[2] = 0.1833;
    iknots[3] = 0.3417;
    iknots[4] = 0.5;
    iknots[5] = 0.6583;
    iknots[6] = 0.8167;
    iknots[7] = 0.975;
  }
  else for ( size_t k = 1; k <= ninner; k++ ) iknots[k] = ( double )( k ) / ( double )( ninner + 1 );
  for ( size_t k = 1; k <= ninner; k++ ) iknots[k] = quantile( m, tmpx, iknots[k], 7 );
  freevector( tmpx );
} // knotsequencemidpercentile

static void getknotsequence( const size_t n, double* x, double* w, const size_t ninner, double* iknots, double* boundaries, const int knotstype )
{
  if ( ninner == 0 ) return;
  if ( knotstype == KNOTSTYPE.USERPROVIDED ) checkandorderknotsequence( ninner, iknots, boundaries );
  else if ( knotstype == KNOTSTYPE.INTERVAL ) knotsequenceinterval( n, x, boundaries, ninner, iknots );
  else if ( knotstype == KNOTSTYPE.PERCENTILE ) knotsequencepercentile( n, x, w, ninner, iknots );
  else if ( knotstype == KNOTSTYPE.MIDPERCENTILE ) knotsequencemidpercentile( n, x, w, ninner, iknots );
}

static double* extendedknotsequence( const size_t n, double* x, const size_t ninner, double* iknots, double* boundaries, const size_t degree )
{
  const double xmin = boundaries[1];
  const double xmax = boundaries[2];
  const size_t order = degree + 1;
  double* eknots = getvector( ninner + 2 * order, 0.0 );
  size_t t = 1;
  for ( size_t k = 1; k <= order; k++, t++ ) eknots[t] = xmin;
  for ( size_t k = 1; k <= ninner; k++, t++ ) eknots[t] = iknots[k];
  for ( size_t k = 1; k <= order; k++, t++ ) eknots[t] = xmax;
  return( eknots );
} // extendedknotsequence

static size_t bisect( const size_t nknots, double* knots, const double xi )
{
  size_t l = 1;
  if ( nknots > 2 ) {
    size_t u = nknots;
    while ( ( u - l ) > 1 ) {
      size_t mid = ( size_t )( floor( ( double )( u + l ) / 2.0 ) );
      if ( xi < knots[mid] ) u = mid;
      else l = mid;
    }
  }
  return( l );
}

static double** bsplinebasis( const size_t n, double* x, double* w, const size_t degree, const size_t ninner, double* iknots, double* boundaries )
{
  const size_t order = degree + 1;
  const size_t nknots = 2 + ninner;
  const size_t ncoefs = ninner + degree + 1;

  double* knots = extendedknotsequence( n, x, ninner, iknots, boundaries, 0 );
  size_t* l = getvector_t( n, ( size_t )( 0 ) );
  for ( size_t i = 1; i <= n; i++ ) l[i] = bisect( nknots, knots, x[i] );
  freevector( knots );

  double** bbase = getmatrix( n, ncoefs, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) bbase[i][l[i]] = 1.0;

  double* eknots = extendedknotsequence( n, x, ninner, iknots, boundaries, degree );
  for ( size_t k = 1; k <= degree; k++ ) {
    const size_t k_offset = degree - k;
    for ( size_t i = 1; i <= n; i++ ) if ( isnotzero( w[i] ) ) {
      double saved = 0.0;
      for ( size_t j = 1; j <= k; j++ ) {
        const size_t j_index = l[i] + j - 1;
        const size_t i1 = j_index + k_offset + 1;
        const size_t i2 = j_index + order;
        const double den = eknots[i2] - eknots[i1];
        const double term = bbase[i][j_index] / den;
        bbase[i][j_index] = saved + ( eknots[i2] - x[i] ) * term;
        saved = ( x[i] - eknots[i1] ) * term;
      }
      bbase[i][l[i] + k] = saved;
    }
  }
  freevector( eknots );
  return( bbase );
} // bsplinebasis

static void cumsum( const size_t n, const size_t m, double** x )
{
  if ( m == 1 ) return;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = m; j > 1; j-- ) x[i][j - 1] += x[i][j];
  }
}

static double** getsplinebasis( const size_t n, double* x, double* w, const size_t ninner, double* iknots, double* boundaries, const size_t degree, const bool anchor, const bool monotone )
{
  const size_t ncoefs = ninner + degree + 1;

  double** b = bsplinebasis( n, x, w, degree, ninner, iknots, boundaries );
  double** basis = NULL;
  if ( monotone == false ) {
    if ( anchor == false ) {
      basis = getmatrix( n, ncoefs - 1, 0.0 );
      for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= ncoefs - 1; j++ ) basis[i][j] = b[i][j+1];
    }
    else {
      basis = getmatrix( n, ncoefs, 0.0 );
      for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= ncoefs - 1; j++ ) basis[i][j] = b[i][j];
      for ( size_t i = 1; i <= n; i++ ) basis[i][1] = 1.0;
    }
  }
  else {
    cumsum( n, ncoefs, b );
    if ( anchor == false ) {
      basis = getmatrix( n, ncoefs - 1, 0.0 );
      for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= ncoefs - 1; j++ ) basis[i][j] = b[i][j+1];
    }
    else {
      basis = getmatrix( n, ncoefs, 0.0 );
      for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= ncoefs; j++ ) basis[i][j] = b[i][j];
    }
  }
  freematrix( b );
  return( basis );
} // getsplinebasis

double** polynomialbasis( const bool symmetric, const size_t n, double** delta, double** w, const size_t ninner, double* iknots, const size_t degree, const bool anchor, const int knotstype, const bool monotone )
// setup polyniomial basis for mds
{
  const size_t nn = ( symmetric == true ? n * ( n - 1 ) / 2 : n * n - n );
  double* vdelta = getvector( nn, 0.0 );
  double* vw = getvector( nn, 1.0 );
  if ( symmetric == true ) {
    for ( size_t i = 2, k = 1; i <= n; i++ ) for ( size_t j = 1; j < i; j++, k++ ) vdelta[k] = delta[i][j];
    if ( isnotnull( w ) ) for ( size_t i = 2, k = 1; i <= n; i++ ) for ( size_t j = 1; j < i; j++, k++ ) {
      const double work = w[i][j];
      vw[k] = work;
      if ( iszero( work ) ) vdelta[k] = 0.0;
    }
  }
  else {
    for ( size_t i = 1, k = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++, k++ ) if ( i != j ) vdelta[k] = delta[i][j];
    if ( isnotnull( w ) ) for ( size_t i = 1, k = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++, k++ ) if ( i != j ) {
      const double work = w[i][j];
      vw[k] = work;
      if ( iszero( work ) ) vdelta[k] = 0.0;
    }
  }
  double* boundaries = getboundaries( nn, vdelta, vw, anchor );
  getknotsequence( nn, vdelta, vw, ninner, iknots, boundaries, knotstype );
  double** basis = getsplinebasis( nn, vdelta, vw, ninner, iknots, boundaries, degree, anchor, monotone );
  freevector( vdelta );
  freevector( vw );
  freevector( boundaries );
  return( basis );
} // polynomialbasis

void polynomialcoefficients( const bool symmetric, const size_t n, double** d, double** w, const size_t m, double** base, double* b, const double mconst, double** gamma )
// monotone bernstein transformation (weights are expected to be nonnegative)
{
  const size_t nn = ( symmetric == true ? n * ( n - 1 ) / 2 : n * n - n );
  double* vd = getvector( nn, 0.0 );
  double* vw = getvector( nn, 1.0 );
  if ( symmetric == true ) {
    for ( size_t i = 2, k = 1; i <= n; i++ ) for ( size_t j = 1; j < i; j++, k++ ) vd[k] = mconst * d[i][j];
    if ( isnotnull( w ) ) for ( size_t i = 2, k = 1; i <= n; i++ ) for ( size_t j = 1; j < i; j++, k++ ) vw[k] = w[i][j];
  }
  else {
    for ( size_t i = 1, k = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++, k++ ) if ( i != j ) vd[k] = mconst * d[i][j];
    if ( isnotnull( w ) ) for ( size_t i = 1, k = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++, k++ ) if ( i != j ) vw[k] = w[i][j];
  }

  // perform fast nonnegative weighted least squares to find regression weights
  size_t MAXITER = 0;
  double FCRIT = 0.0;
  //if ( isnull( w ) ) nnls( nn, m, base, vd, b );
  //else nnwls( nn, m, base, vd, b, vw );
  if ( isnull( w ) ) fastnnls( nn, m, base, b, vd, &MAXITER, &FCRIT );
  else fastnnwls( nn, m, base, b, vd, vw );
  // size_t MAXITER = ( size_t ) 4096;
  // double FCRIT = 0.000000000001;
  // if ( isnull( w ) ) nnccd( nn, m, base, b, vd, &MAXITER, &FCRIT );
  // else nnwccd( nn, m, base, b, vd, vw );
  freevector( vd );

  // set output vector
  if ( symmetric == true ) {
    for ( size_t i = 2, k = 1; i <= n; i++ ) for ( size_t j = 1; j < i; j++, k++ ) {
      if ( iszero( vw[k] ) ) gamma[j][i] = gamma[i][j] = 0.0;
      else {
        double work = 0.0;
        for ( size_t p = 1; p <= m + 1; p++ ) work += base[k][p] * b[p];
        gamma[j][i] = gamma[i][j] = work;
      }
    }
  }
  else {
    for ( size_t i = 1, k = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++, k++ ) if ( i != j ) {
      if ( iszero( vw[k] ) ) gamma[i][j] = 0.0;
      else {
        double work = 0.0;
        for ( size_t p = 1; p <= m + 1; p++ ) work += base[k][p] * b[p];
        gamma[i][j] = work;
      }
    }
  }
  freevector( vw );
} // nnpolynomial

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// optimal rotation functions
//

void rotation( const size_t n, const size_t p, double** z, double** r, double* ev )
// return principal axes rotation matrix R
// return identity matrix on error
{
  gemm( true, false, p, p, n, 1.0, z, z, 0.0, r );
  if ( evdcmp( p, r, ev ) != 0 ) {
    set( p * p, 0.0, &r[1][1], 1 );
    for ( size_t k = 1; k <= p; k++ ) r[k][k] = 1.0;
    return;
  }
  for ( size_t k = 1; k <= p; k++ ) {
    double work = 0.0;
    for ( size_t i = 1; i <= p; i++ ) work += z[1][i] * r[i][k];
    if ( work < 0.0 ) for ( size_t i = 1; i <= p; i++ ) r[i][k] *= -1.0;
  }
} // rotation

void weightedrotation( const size_t n, const size_t p, double** z, double* w, double** r, double* ev )
// return principal axes rotation matrix R
// return identity matrix on error
{
  for ( size_t i = 1; i <= p; i++ ) {
    for ( size_t j = 1; j <= p; j++ ) {
      double work = 0.0;
      for ( size_t k = 1; k <= n; k++ ) work += z[k][i] * w[k] * z[k][j];
      r[i][j] = work;
    }
  }
  if ( evdcmp( p, r, ev ) != 0 ) {
    set( p * p, 0.0, &r[1][1], 1 );
    for ( size_t k = 1; k <= p; k++ ) r[k][k] = 1.0;
    return;
  }
  for ( size_t k = 1; k <= p; k++ ) {
    double work = 0.0;
    for ( size_t i = 1; i <= p; i++ ) work += z[1][i] * r[i][k];
    if ( work < 0.0 ) for ( size_t i = 1; i <= p; i++ ) r[i][k] *= -1.0;
  }
} // weightedrotation

void rotate( const size_t n, const size_t p, double** z )
// rotate Z to principal axes, plus
{
  double* ev = getvector( max_t( n, p ), 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  rotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  freematrix( r );
} // rotate

void weightedrotate( const size_t n, const size_t p, double** z, double* w )
// rotate Z to principal axes
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  weightedrotation( n, p, z, w, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  freematrix( r );
} // weightedrotate

void rotateplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1 )
// rotate Z to principal axes, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  rotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  gemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  copy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  freematrix( r );
} // rotateplus

void weightedrotateplus( const size_t n, const size_t p, double** z, double* w, const size_t n1, double** z1 )
// rotate Z to principal axes, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  weightedrotation( n, p, z, w, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  gemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  copy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  freematrix( r );
} // rotateplus

void rotateplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2 )
// rotate Z to principal axes, plus, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  rotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  gemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  copy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  double** tz2 = getmatrix( n2, p, 0.0 );
  gemm( false, false, n2, p, p, 1.0, z2, r, 0.0, tz2 );
  copy( n2 * p, &tz2[1][1], 1, &z2[1][1], 1 );
  freematrix( tz2 );
  freematrix( r );
} // rotateplusplus

void rotateplusplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2, const size_t n3, double** z3 )
// rotate Z to principal axes, plus, plus, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  rotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  gemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  copy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  double** tz2 = getmatrix( n2, p, 0.0 );
  gemm( false, false, n2, p, p, 1.0, z2, r, 0.0, tz2 );
  copy( n2 * p, &tz2[1][1], 1, &z2[1][1], 1 );
  freematrix( tz2 );
  double** tz3 = getmatrix( n3, p, 0.0 );
  gemm( false, false, n3, p, p, 1.0, z3, r, 0.0, tz3 );
  copy( n3 * p, &tz3[1][1], 1, &z3[1][1], 1 );
  freematrix( tz3 );
  freematrix( r );
} // rotateplusplus
// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// R print functions
//

void printerror( char* title )
{
  #ifdef R
    Rprintf( "error: %s\n", title );
  #else
    printf( "error: %s\n", title );
  #endif
} // printpass

void printpass( const size_t nr )
{
  #ifdef R
    Rprintf( "pass %zu\n", nr );
  #else
    printf( "pass %zu\n", nr );
  #endif
} // printpass

void printscalar_t( char* title, const size_t a )
{
  #ifdef R
    Rprintf( "%s", title );
    Rprintf( " = %zu\n", a );
  #else
    printf( "%s", title );
    printf( " = %zu\n", a );
  #endif
} // printscalar_t

void printscalar( char* title, const double a )
{
  #ifdef R
    Rprintf( "%s", title );
    Rprintf( " = %f\n", a );
  #else
    printf( "%s", title );
    printf( " = %f\n", a );
  #endif
} // printscalar

void printivector( char* title, const size_t n, size_t* a )
{
  #ifdef R
    Rprintf( "%s", title );
    Rprintf( " = " );
    for ( size_t i = 1; i <= n; i++ ) Rprintf( " %zu", a[i] );
    Rprintf( "\n" );
  #else
    printf( "%s", title );
    printf( " = " );
    for ( size_t i = 1; i <= n; i++ ) printf( " %zu", a[i] );
    printf( "\n" );
  #endif
} // printvector

void printvector( char* title, const size_t n, double* a )
{
  #ifdef R
    Rprintf( "%s", title );
    Rprintf( " = " );
    for ( size_t i = 1; i <= n; i++ ) Rprintf( " %f", a[i] );
    Rprintf( "\n" );
  #else
    printf( "%s", title );
    printf( " = " );
    for ( size_t i = 1; i <= n; i++ ) printf( " %f", a[i] );
    printf( "\n" );
  #endif
} // printvector

void printmatrix( char* title, const size_t n, const size_t m, double** a )
{
  #ifdef R
    Rprintf( "%s", title );
    Rprintf( " = \n" );
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) Rprintf( " %f", a[i][j] );
      Rprintf( "\n" );
    }
  #else
    printf( "%s", title );
    printf( " = \n" );
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) printf( " %f", a[i][j] );
      printf( "\n" );
    }
  #endif
} // printmatrix

void echoprogress( const size_t iter, const double fold, const double fhalf, const double fnew )
{
  if ( iter == 0 ) {
    #ifdef R
      Rprintf( "iter = %6zu; fnew = %0.15f\n", iter, fold );
    #else
      printf( "iter = %6zu; fnew = %0.15f\n", iter, fold );
    #endif
  }
  else {
    #ifdef R
      Rprintf( "iter = %6zu; fold = %0.15f; fhalf = %0.15f; fnew = %0.15f; diff = %0.15f\n", iter, fold, fhalf, fnew, fold - fnew);
    #else
      printf( "iter = %6zu; fold = %0.15f; fhalf = %0.15f; fnew = %0.15f; diff = %0.15f\n", iter, fold, fhalf, fnew, fold - fnew);
    #endif
  }
} // echoprogress

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// c i/o functions
//

double** readmatrix( char* infilename, size_t *n, size_t *m )
{
  #ifdef R
    return( 0 );
  #else
    FILE *infile;
    infile = fopen( infilename, "r" );
    if ( !infile ) return 0;

    int count = 0;
    for( ;; ) {
      int c = fgetc( infile );
      if( c == EOF || c == '\n' ) break;
      count++;
    }
    count += 512;
    char line[count];

    rewind( infile );
    fgets( line, sizeof( line ), infile );
    char *scan = line;
    double dummy;
    int offset = 0;
    *m = 0;
    while ( sscanf( scan, "%lf%n", &dummy, &offset ) == 1 ) {
      scan += offset;
     (*m)++;
    }

    *n = 1;
    while ( fgets( line, sizeof( line ), infile ) ) (*n)++;

    rewind( infile );
    double** data = getmatrix( *n, *m, 0.0 );
    for( size_t i = 1; i <= *n; i++ ) {
      for( size_t j = 1; j <= *m; j++ ) {
        double d = 0.0;
        fscanf( infile, "%lf", &d );
        data[i][j] = d;
      }
    }
    fclose( infile );
    return( data );
  #endif
} // readmatrix

char* getdatetime(void)
{
  #ifdef R
    return( 0 );
  #else
    time_t t = time( NULL );
    struct tm *tm = localtime( &t );
    return asctime( tm );
  #endif
}








time_t setstarttime( void )
{
  #ifdef R
    return( 0 );
  #else
    struct timespec current_time;
    if ( clock_gettime( CLOCK_REALTIME, &current_time ) != 0 ) return 0;
    return current_time.tv_sec * CLOCKS_PER_SEC + current_time.tv_nsec / ( 1000000000 / CLOCKS_PER_SEC ); 
  #endif
} // setstarttime

double getelapsedtime( const time_t starttime )
// get current time in time_t format
// return time difference in seconds in double format
{
  #ifdef R
    return( 0.0 );
  #else
    struct timespec current_time;
    if ( clock_gettime( CLOCK_REALTIME, &current_time ) != 0 ) return 0.0;
    time_t endtime = current_time.tv_sec * CLOCKS_PER_SEC + current_time.tv_nsec / ( 1000000000 / CLOCKS_PER_SEC ); 
    return( ( double )( endtime - starttime ) / 1000.0 );
  #endif
} // getelapsedtime

