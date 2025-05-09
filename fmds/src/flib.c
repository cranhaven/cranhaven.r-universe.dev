//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "flib.h"
#include <math.h>

#ifdef _WIN32
#endif

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
} // isnotzero

bool isalmostzero( const double x )
{
  const double TINY = pow( 10.0, ( log10( DBL_EPSILON ) + log10( sqrt( DBL_EPSILON ) ) ) / 2.0 );  // 1.8189894035458617e-12
  return( ( x < TINY ) && ( x > -TINY ) );
} // isalmostzero

bool isnotalmostzero( const double x )
{
  const double TINY = pow( 10.0, ( log10( DBL_EPSILON ) + log10( sqrt( DBL_EPSILON ) ) ) / 2.0 );  // 1.8189894035458617e-12
  return( ( x > TINY ) || ( x < -TINY ) );
} // isnotalmostzero

bool issysmis( const double a )
{
  return isequal( a, SYSMIS );
} // issysmis

bool isnotsysmis( const double a )
{
  return isnotequal( a, SYSMIS );
} // isnotsysmis

double sign( const double r, const double s )
// returns r with sign of s
{
  if ( s == 0.0 ) return r;
  else return ( s < 0.0 ) ? -fabs( r ) : fabs( r );  // added fabs() 20250107
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

static double nnlog( const double a )
{
  return ( a <= DBL_EPSILON ? 0.0 : log( a ) );
} // nnlog

double roundat( const double a, const int d )
{
  double m = pow( 10.0, ( double )( d ) - 1.0 );
  double f = m * a;
  double r = round( f );
  return r / m;
} // round

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
// This is a fixed-increment version of Java 8's SplittableRandom generator
// See http://dx.doi.org/10.1145/2714064.2660195 and http://docs.oracle.com/javase/8/docs/api/java/util/SplittableRandom.html
// It is a very fast generator passing BigCrush, and it can be useful if for some reason you absolutely want 64 bits of state.
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

size_t nextsize_t( void )
// This is xoshiro256+ 1.0, our best and fastest generator for floating-point numbers. 
// We suggest to use its upper bits for floating-point generation, as it is slightly faster than xoshiro256++/xoshiro256**. 
// It passes all tests we are aware of except for the lowest three bits, which might fail linearity tests (and just those), so if low linear complexity is not considered an issue (as it is usually the case) it can be used to generate 64-bit outputs, too.
// We suggest to use a sign test to extract a random Boolean value, and right shifts to extract subsets of bits.
// The state must be seeded so that it is not everywhere zero. 
// If you have a 64-bit seed, we suggest to seed a splitmix64 generator and use its output to fill s.
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

// PCG Random Number Generation for C.
// Copyright 2014 Melissa O'Neill <oneill@pcg-random.org>
// struct pcg_state_setseq_64 {  // Internals are *Private*.
//   uint64_t state;             // RNG state.  All values are possible.
//   uint64_t inc;               // Controls which RNG sequence (stream) is selected. Must *always* be odd.
// };
// typedef struct pcg_state_setseq_64 pcg32_random_t;
// #define PCG32_INITIALIZER { 0x853c49e6748fea9bULL, 0xda3e39cb94b95bdbULL }
// static pcg32_random_t pcg32_global = PCG32_INITIALIZER;
// static uint32_t pcg32_random_r( pcg32_random_t* rng )
// {
//   uint64_t oldstate = rng->state;
//   rng->state = oldstate * 6364136223846793005ULL + rng->inc;
//   uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
//   uint32_t rot = oldstate >> 59u;
//   return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
// }
// static void pcg32_srandom_r( pcg32_random_t* rng, uint64_t initstate, uint64_t initseq )
// {
//   if ( initstate == 0 ) {
//     int rounds = 5;
//     initstate = time( NULL ) ^ ( intptr_t )&printf;
//     if ( initseq == 0 ) initseq = ( intptr_t )&rounds;
//   }
//   rng->state = 0U;
//   rng->inc = ( initseq << 1u ) | 1u;
//   pcg32_random_r( rng );
//   rng->state += initstate;
//   pcg32_random_r( rng );
// }
// void pcg32_srandom( uint64_t seed, uint64_t seq )
// {
//   pcg32_srandom_r(&pcg32_global, seed, seq);
// }
// uint32_t pcg32_random( )
// {
//   return pcg32_random_r(&pcg32_global);
// }
// double pcg32_next( )
// {
//   return( ldexp( pcg32_random_r( &pcg32_global ), -32 ) );
// }

size_t duniform( const size_t n1, const size_t n2 )
// draw discrete uniform number between n1 and n2
{
  return n1 + ( size_t )( floorl( nextdouble( ) * ( double )( n2 - n1 + 1 ) ) );
} // duniform

double stdnormal( void )
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

double stdlognormal( void )
{
  return exp( stdnormal( ) );
} // stdlognormal

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

void draw_t( const size_t n, size_t* a, const size_t m, size_t* b, const bool replace )
{
  if ( replace == false ) {
    for ( size_t i = 1; i <= m; i++ ) {
      size_t k = i + ( size_t )( nextdouble( ) * ( double )( n - i + 1 ) );
      size_t j = a[i];
      b[i] = a[i] = a[k];
      a[k] = j;
    }
  }
  if ( replace == true ) {
    for ( size_t i = 1; i <= m; i++ ) {
      size_t k = 1 + ( size_t )( nextdouble( ) * ( double )( n ) );
      b[i] = a[k];
    }
  }
} // draw_t

double choose( double n, double k )
{
  if ( n < k ) return( 0.0 );
  if ( iszero( k ) ) return 1.0;
  return( n * choose( n - 1, k - 1 ) ) / k;
}   

double combination( long n, long k ) {
  if ( n < k ) return( 0.0 );
  if ( k == 0 ) return( 1.0 );
  double sum = 0.0;
  for( long i = 0; i < k; i++ ) {
      sum += log10( ( double )( n - i ) );
      sum -= log10( ( double )( i + 1 ) );
  }
  return pow( 10.0, sum );
}

size_t expecteddraws( const size_t n, const size_t m )
{
  long seed = 0;
  randomize( &seed );
  const size_t nrepls = 128;
  const size_t maxsize_t = SSIZE_MAX;
  const size_t nn = n * n;
  size_t** w = getmatrix_t( n, n, 0 );
  size_t* s = getvector_t( m, 0 );
  size_t* pop = getvector_t( n, 0 );
  for ( size_t i = 1; i <= n; i++ ) pop[i] = i;
  size_t mnn = 0;
  size_t r = 0;
  for ( size_t k = 1; k <= nrepls; k++ ) {
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) w[i][j] = 0;
    size_t sn = 0;
    for ( r = 1; r <= maxsize_t; r++ ) {
      draw_t( n, pop, m, s, false );
      for ( size_t i = 1; i <= m; i++ ) {
        for ( size_t j = 1; j <= m; j++ ) {
          const size_t si = s[i];
          const size_t sj = s[j];
          if ( w[si][sj] == 0 ) {
            w[si][sj] = 1;
            sn++;
          }
        }
      }
      if ( sn == nn ) break;
    }
    mnn = mnn + r;
  }
  freematrix_t( w );
  freevector_t( s );
  freevector_t( pop );
  return( mnn / nrepls );
}

size_t binarysearch( const size_t n, double* x, const double p )
{
  size_t a = 1;
  size_t b = n;
  while ( ( b - a ) > 1 ) {
    const size_t c = ( a + b ) / 2;
    if ( p <= x[c] ) b = c;
    else a = c;
  }
  return ( p <= x[a] ? a : b );
}

size_t wheel( const size_t n, double* cump, const double r )
{
  size_t k = 1;
  for ( size_t i = 1; i <= n; i++ ) if ( r <= cump[i] ) {
    k = i;
    break;
  }
  return k;
} // wheel

void randomZ( const size_t n, const size_t p, double** z, long seed )
// draw random configuration uniformly distributed within unit circle
{
  randomize( &seed );
  for ( size_t i = 1; i <= n; i++ ) {
    for ( ; ; ) {
      double work = 0.0;
      for ( size_t j = 1; j <= p; j++ ) {
        double d = nextdouble( );
        work += d * d;
        z[i][j] = d;
      }
      if ( work <= 1.0 ) break;
    }
  }
} // randomZ

#define DIST_E 2.71828182845904523536
#define DIST_PI 3.14159265358979323846
#define DIST_SQRT2 1.41421356237309504880

double normal_pdf( const double x, const double mean, const double stddev) 
{
  if ( stddev <= 0.0 ) return NAN;
  const double n = ( x - mean ) / stddev;
  return ( 1.0 / ( stddev * sqrt( 2.0 * DIST_PI ) ) ) * pow( DIST_E, -0.5 * n * n );
}

double normal_cdf( const double x, const double mean, const double stddev ) 
{
  if (stddev <= 0) return NAN;
  return 0.5 * ( 1.0 + erf( ( x - mean ) / ( stddev * DIST_SQRT2 ) ) );
}

double normal_ppf( const double p, const double mean, const double stddev ) 
// Wichura, M. J. (1988). Algorithm AS 241: The Percentage Points of the Normal Distribution. Journal of the Royal Statistical Society. Series C (Applied Statistics), 37(3), 477-484.
{
  if ( p < 0.0 || p > 1.0 || stddev <= 0.0 || isnan( mean ) || isnan( stddev ) ) return NAN;
  if ( iszero( p ) ) return -INFINITY;
  if ( isequal( p, 1.0 ) ) return INFINITY;

  double q = p - 0.5;
  if ( fabs( q ) < 0.425 ) {
    const double r = 0.180625 - q * q;
    return mean + stddev * q *
      (((((((2.5090809287301226727e3 * r + 3.3430575583588128105e4) * r + 6.7265770927008700853e4) * r + 4.5921953931549871457e4) * r + 1.3731693765509461125e4) * r + 1.9715909503065514427e3) * r + 1.3314166789178437745e2) * r + 3.3871328727963666080e0) /
      (((((((5.2264952788528545610e3 * r + 2.8729085735721942674e4) * r + 3.9307895800092710610e4) * r + 2.1213794301586595867e4) * r + 5.3941960214247511077e3) * r + 6.8718700749205790830e2) * r + 4.2313330701600911252e1) * r + 1);
  } else {
    double r = q < 0.0 ? p : 1 - p;
    r = sqrt( -log( r ) );
    const double sign = q < 0 ? -1 : 1;
    if (r < 5) {
      r -= 1.6;
      return mean + stddev * sign *
        (((((((7.74545014278341407640e-4 * r + 2.27238449892691845833e-2) * r + 2.41780725177450611770e-1) * r + 1.27045825245236838258e0) * r + 3.64784832476320460504e0) * r + 5.76949722146069140550e0) * r + 4.63033784615654529590e0) * r + 1.42343711074968357734e0) /
        (((((((1.05075007164441684324e-9 * r + 5.47593808499534494600e-4) * r + 1.51986665636164571966e-2) * r + 1.48103976427480074590e-1) * r + 6.89767334985100004550e-1) * r + 1.67638483018380384940e0) * r + 2.05319162663775882187e0) * r + 1);
    } else {
      r -= 5;
      return mean + stddev * sign *
        (((((((2.01033439929228813265e-7 * r + 2.71155556874348757815e-5) * r + 1.24266094738807843860e-3) * r + 2.65321895265761230930e-2) * r + 2.96560571828504891230e-1) * r + 1.78482653991729133580e0) * r + 5.46378491116411436990e0) * r + 6.65790464350110377720e0) /
        (((((((2.04426310338993978564e-15 * r + 1.42151175831644588870e-7) * r + 1.84631831751005468180e-5) * r + 7.86869131145613259100e-4) * r + 1.48753612908506148525e-2) * r + 1.36929880922735805310e-1) * r + 5.99832206555887937690e-1) * r + 1);
    }
  }
}

double students_t_pdf( const double x, const double n ) {
  if ( n <= 0.0 ) return NAN;
  if ( n == INFINITY ) return normal_pdf( x, 0.0, 1.0 );
  return tgamma( ( n + 1.0 ) / 2.0 ) / ( sqrt( n * DIST_PI ) * tgamma( n / 2.0 ) ) * pow( 1.0 + x * x / n, -( n + 1.0 ) / 2.0 );
}

double students_t_cdf( const double x, const double n ) {
// Hill, G. W. (1970). Algorithm 395: Student's t-distribution. Communications of the ACM, 13(10), 617-619.
  if ( n < 1.0 ) return NAN;
  if ( isnan( x ) ) return NAN;
  if ( !isfinite( x ) ) return x < 0.0 ? 0.0 : 1.0;
  if ( n == INFINITY ) return normal_cdf( x, 0.0, 1.0 );
  const double start = x < 0.0 ? 0.0 : 1.0;
  const double sign = x < 0.0 ? 1.0 : -1.0;
  double z = 1.0;
  const double t = x * x;
  double y = t / n;
  double b = 1.0 + y;
  if ( n > floor( n ) || ( n >= 20 && t < n ) || n > 200.0 ) {
    if ( y > 10e-6) y = log( b );
    const double a = n - 0.5;
    b = 48.0 * a * a;
    y = a * y;
    y = ( ( ( ( ( -0.4 * y - 3.3 ) * y - 24.0 ) * y - 85.5 ) / ( 0.8 * y * y + 100.0 + b ) + y + 3.0 ) / b + 1.0 ) * sqrt( y );
    return start + sign * normal_cdf( -y, 0.0, 1.0 );
  }
  double m = floor(n);
  if ( m < 20.0 && t < 4.0 ) {
    y = sqrt( y );
    double a = y;
    if ( isequal( m, 1.0 ) ) a = 0.0;
    if ( m > 1.0 ) {
      m -= 2.0;
      while ( m > 1.0) {
        a = ( m - 1.0 ) / ( b * m ) * a + y;
        m -= 2.0;
      }
    }
    a = iszero( m ) ? a / sqrt( b ) : ( atan( y ) + a / b ) * ( 2.0 / DIST_PI );
    return start + sign * ( z - a ) / 2.0;
  }
  double a = sqrt( b );
  y = a * m;
  double j = 0.0;
  while (a != z) {
    j += 2.0;
    z = a;
    y *= ( j - 1.0 ) / ( b * j );
    a += y / ( m + j );
  }
  z = 0.0;
  y = 0.0;
  a = -a;
  while ( m > 1.0 ) {
    a = ( m - 1.0 ) / ( b * m ) * a + y;
    m -= 2.0;
  }
  a = iszero( m ) ? a / sqrt( b ) : ( atan( y ) + a / b ) * ( 2.0 / DIST_PI );
  return start + sign * ( z - a ) / 2.0;
}

double students_t_ppf( const double p, const double n ) 
// Hill, G. W. (1970). Algorithm 396: Student's t-quantiles. Communications of the ACM, 13(10), 619-620.
{
  if ( p < 0.0 || p > 1.0 || n < 1.0 ) return NAN;
  if ( n == INFINITY ) return normal_ppf( p, 0.0, 1.0 );
  double sign = p < 0.5 ? -1.0 : 1.0;
  double sp = p < 0.5 ? 1.0 - p : p;
  sp = 2.0 * ( 1.0 - sp );
  if ( isequal( n, 2.0 ) ) return sign * sqrt( 2.0 / ( sp * ( 2.0 - sp ) ) - 2.0 );
  double half_pi = DIST_PI / 2.0;
  if ( isequal( n, 1.0 ) ) {
    sp *= half_pi;
    return sign * cos( sp ) / sin( sp );
  }
  double a = 1.0 / ( n - 0.5 );
  double b = 48.0 / ( a * a );
  double c = ( ( 20700.0 * a / b - 98.0 ) * a - 16.0 ) * a + 96.36;
  double d = ( ( 94.5 / ( b + c ) - 3.0 ) / b + 1.0 ) * sqrt( a * half_pi ) * n;
  double x = d * sp;
  double y = pow( x, 2.0 / n );
  if (y > 0.05 + a) {
    x = normal_ppf( sp * 0.5, 0.0, 1.0 );
    y = x * x;
    if ( n < 5.0 ) c += 0.3 * ( n - 4.5 ) * ( x + 0.6 );
    c = ( ( ( 0.05 * d * x - 5.0 ) * x - 7.0 ) * x - 2.0 ) * x + b + c;
    y = ( ( ( ( ( 0.4 * y + 6.3 ) * y + 36.0 ) * y + 94.5 ) / c - y - 3.0 ) / b + 1.0 ) * x;
    y = a * y * y;
    y = y > 0.002 ? exp( y ) - 1.0 : 0.5 * y * y + y;
  } 
  else y = ( ( 1.0 / ( ( ( n + 6.0 ) / ( n * y ) - 0.089 * d - 0.822 ) * ( n + 2.0 ) * 3.0 ) + 0.5 / ( n + 4.0 ) ) * y - 1.0 ) * ( n + 1.0 ) / ( n + 2.0 ) + 1.0 / y;
  return sign * sqrt( n * y );
}

double randomDelta( const size_t n, const size_t m, int* vdist, double* vssq, const int edist, const double epr, const long seed, double** delta )
{
  double fit = 0.0;

  // re-initialize random number generator
  long tmpseed = seed;
  randomize( &tmpseed );

  // draw multivariate data z
  double** z = getmatrix( n, m, 0.0 );
  for ( size_t j = 1; j <= m; j++ ) {
         if ( vdist[j] == 1 ) for ( size_t i = 1; i <= n; i++ ) z[i][j] = nextdouble( );
    else if ( vdist[j] == 2 ) for ( size_t i = 1; i <= n; i++ ) z[i][j] = stdnormal( );
    else if ( vdist[j] == 3 ) for ( size_t i = 1; i <= n; i++ ) z[i][j] = stdlognormal( );
    else                      for ( size_t i = 1; i <= n; i++ ) z[i][j] = 0.0;

    // center variables
    double sm = 0.0;
    for ( size_t i = 1; i <= n; i++ ) sm += z[i][j];
    const double mn = sm / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) z[i][j] -= mn;

    // normalize variables
    const double ssq = dssq( n, &z[1][j], m );
    const double nrm = ( iszero( ssq ) ? 0.0 : sqrt( vssq[j] ) / sqrt( ( double )( n ) * ssq ) );
    dscal( n, nrm, &z[1][j], m );
  }

  // compute distance between cases
  euclidean1( n, m, z, delta );
  freematrix( z );

  // add symmetric error
  if ( isnotzero( epr ) ) {

    double** e = getmatrix( n, n, 0.0 );

    // set symmetric error
    const double ssqdelta = dssq( n * n, &delta[1][1], 1 );
         if ( edist == 1 ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) e[i][j] = e[j][i] = nextdouble( );
    else if ( edist == 2 ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) e[i][j] = e[j][i] = stdnormal( );
    else if ( edist == 3 ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) e[i][j] = e[j][i] = stdlognormal( );
    else                   for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) e[i][j] = e[j][i] = 1.0;

    // normalize error
    const double ssqerror = dssq( n * n, &e[1][1], 1 );
    const double nrm = ( iszero( ssqdelta ) ? sqrt( 1.0 / ssqerror ) : sqrt( ssqdelta / ssqerror ) );
    dscal( n * n, nrm, &e[1][1], 1 );

    // add error to distances
    const double alpha2 = pow( 1.0 - epr, 2.0 );
    const double beta2 = 1.0 - alpha2;
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= n; j++ ) e[i][j] = alpha2 * delta[i][j] + beta2 * e[i][j];
    }

    // compute fit
    if ( iszero( ssqdelta ) ) fit = 1.0;
    else {
      const double scl = scale( n, n, delta, e );
      double norm = 0.0;
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
          const double dij = scl * delta[i][j];
          const double diff = dij - e[i][j];
          fit += diff * diff;
          norm += dij * dij;
        }
      }
      fit /= norm;
    }

    dcopy( n * n, &e[1][1], 1, &delta[1][1], 1 );
    freematrix( e );
  }

  return( sqrt( fit ) );
} // randomDelta

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// basic linear algebra functions (subroutines)
//

double dmin( const size_t n, const double* const a, const size_t inca )
// return minimum value of vector
{
  double s = DBL_MAX;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] < s ) s = a[ia];
    ia += inca;
  }
  return s;
} // dmin

double dmax( const size_t n, const double* const a, const size_t inca )
// return maximum value of vector
{
  double s = -DBL_MAX;
  size_t ia = 0;
  for ( size_t i = n; i--; ) {
    if ( a[ia] > s ) s = a[ia];
    ia += inca;
  }
  return s;
} // dmax

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

static double damax( const size_t n, const double* const a, const size_t inca )
// returns the absolute maximum of a vector
{
  if ( n == 0 ) return 0.0;
  double d = 0.0;
  if ( inca != 1 ) {
    size_t ia = 0;
    for ( size_t i = n; i--; ) {
      double work = fabs( a[ia] );
      if ( work > d ) d = work;
      ia += inca;
    }
    return d;
  }
  size_t i = n >> 3;
  size_t j = 0;
  size_t k = n & 7;
  double work = 0.0;
  while ( i-- ) {
    work = fabs( a[j] ); if ( work > d ) d = work;
    work = fabs( a[j + 1] ); if ( work > d ) d = work;
    work = fabs( a[j + 2] ); if ( work > d ) d = work;
    work = fabs( a[j + 3] ); if ( work > d ) d = work;
    work = fabs( a[j + 4] ); if ( work > d ) d = work;
    work = fabs( a[j + 5] ); if ( work > d ) d = work;
    work = fabs( a[j + 6] ); if ( work > d ) d = work;
    work = fabs( a[j + 7] ); if ( work > d ) d = work;
    j += 8;
  }
  switch ( k ) {
    case 7: work = fabs( a[j] ); if ( work > d ) d = work; j++;
    case 6: work = fabs( a[j] ); if ( work > d ) d = work; j++;
    case 5: work = fabs( a[j] ); if ( work > d ) d = work; j++;
    case 4: work = fabs( a[j] ); if ( work > d ) d = work; j++;
    case 3: work = fabs( a[j] ); if ( work > d ) d = work; j++;
    case 2: work = fabs( a[j] ); if ( work > d ) d = work; j++;
    case 1: work = fabs( a[j] ); if ( work > d ) d = work; j++;
  }
  return d;
} // damax

static void dswap( const size_t n, double* const a, const size_t inca, double* const b, const size_t incb )
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

double dsum( const size_t n, const double* const a, const size_t inca )
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

double dwsum( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw )
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

void dscal( const size_t n, const double c, double* const a, const size_t inca )
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

double ddot( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb )
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

double dwdot( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw )
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
} // dwdot

double dssq( const size_t n, const double* const a, const size_t inca )
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

double dwssq( const size_t n, const double* const a, const size_t inca, const double* const w, const size_t incw )
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

void daxpy( const size_t n, const double c, double* a, const size_t inca, double* b, const size_t incb )
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

double dsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb )
// dsse returns the euclidean norm of the difference between vectors a and b, i.e., dsse = [a-b]'[a-b]
{
  if ( n == 0 ) return 0.0;
  double d;
  double s = 0.0;
  if ( inca != 1 || incb != 1 ) {
    size_t ia = 0;
    size_t ib = 0;
    for ( size_t i = n; i--; ) {
      d = a[ia] - b[ib];
      s += d * d;
      ia += inca;
      ib += incb;
    }
    return sqrt( s );
  }
  size_t i = n >> 3;
  size_t j = 0;
  const size_t k = n & 7;
  while ( i-- ) {
    d = a[j] - b[j]; s += d * d;
    d = a[j + 1] - b[j + 1]; s += d * d;
    d = a[j + 2] - b[j + 2]; s += d * d;
    d = a[j + 3] - b[j + 3]; s += d * d;
    d = a[j + 4] - b[j + 4]; s += d * d;
    d = a[j + 5] - b[j + 5]; s += d * d;
    d = a[j + 6] - b[j + 6]; s += d * d;
    d = a[j + 7] - b[j + 7]; s += d * d;
    j += 8;
  }
  switch ( k ) {
    case 7: d = a[j] - b[j]; s += d * d; j++;
    case 6: d = a[j] - b[j]; s += d * d; j++;
    case 5: d = a[j] - b[j]; s += d * d; j++;
    case 4: d = a[j] - b[j]; s += d * d; j++;
    case 3: d = a[j] - b[j]; s += d * d; j++;
    case 2: d = a[j] - b[j]; s += d * d; j++;
    case 1: d = a[j] - b[j]; s += d * d; j++;
  }
  return s;
} // dsse

double drsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb )
// rmse returns the euclidean norm of the difference between vectors a and b, i.e., rmse = sqrt ( [a-b]'[a-b] )
{
  return( sqrt( dsse( n, a, inca, b, incb ) ) );
} // drsse

double dwsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw )
// dwsse returns the weighted euclidean norm of a difference vector, i.e., dwsse = [a-b]'diag(w)[a-b]
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
  return s;
} // wrmse

double drwsse( const size_t n, const double* const a, const size_t inca, const double* const b, const size_t incb, const double* const w, const size_t incw )
// dnrm2 returns the weighted euclidean norm of a difference vector, i.e., dnrm2 = sqrt ( a-b'*diag(w)*a-b )
{  // profile
  return( sqrt( dwsse( n, a, inca, b, incb, w, incw ) ) );
} // drwsse

void dset( const size_t n, const double b, double* const a, const size_t inca )
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

void dcopy( const size_t n, const double* const a, const size_t inca, double* const b, const size_t incb )
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
  memcpy( b, a, n * sizeof( a[1] ) );
  return;
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

void dgemv( const bool transa, const size_t nra, const size_t nca, const double alpha, double** const a, double* const b, const double beta, double* const c )
// c = alpha * trans( a ) * b + beta * c
{
  if ( nra == 0 || nca == 0 || ( iszero( alpha ) && isequal( beta, 1.0 ) ) ) return;
  if ( transa == false ) {
    double* r = getvector( nra, 0.0 );
    double* ab = getvector( nca, 0.0 );
    daxpy( nra, beta, &c[1], 1, &r[1], 1 );
    if ( isnotzero( alpha ) ) {
      daxpy( nca, alpha, &b[1], 1, &ab[1], 1 );
      for ( size_t i = 1; i <= nra; i++ ) {
        double work = 0.0;
        for ( size_t j = 1; j <= nca; j++ ) work += a[i][j] * ab[j];
        r[i] += work;
      }
    }
    dcopy( nra, &r[1], 1, &c[1], 1 );
    freevector( r );
    freevector( ab );
  }
  else if ( transa == true ) {
    double* r = getvector( nca, 0.0 );
    daxpy( nca, beta, &c[1], 1, &r[1], 1 );
    if ( isnotzero( alpha ) ) {
      for ( size_t j = 1; j <= nca; j++ ) {
        double work = 0.0;
        for ( size_t i = 1; i <= nra; i++ ) work += a[i][j] * b[i];
        r[j] += alpha * work;
      }
    }
    dcopy( nca, &r[1], 1, &c[1], 1 );
    freevector( r );
  }
} // gemv

void dgemm( const bool transa, const bool transb, const size_t nrc, const size_t ncc, const size_t nab, const double alpha, double** const a, double** const b, const double beta, double** const c )
// C = alpha * A(ta) * B(tb) + beta * C
{
  // input cannot be same as output
  assert( a != c );
  assert( b != c );

  // if alpha equals zero
  if ( iszero( alpha ) ) {
    if ( iszero( beta ) ) zeroall( nrc * ncc, &c[1][1] );
    else if ( isnotequal( beta, 1.0 ) ) dscal( nrc * ncc, beta, &c[1][1], 1 );
    return;
  }

  if ( isnotzero( beta ) ) dscal( nrc * ncc, beta, &c[1][1], 1 );
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
} // dgemm

double variance( const size_t n, const double* const a, const size_t inca )
// returns unweighted sample variance
{
  if ( n == 1 ) return 0.0;
  double w = 0.0;
  double mean = 0.0;
  double M2 = 0.0;
  size_t ia = 0;
  for ( size_t i = 0; i < n; i++ ) {
    w += 1.0;
    const double delta = a[ia] - mean;
    mean += delta / w;
    M2 += delta * ( a[ia] - mean );
    ia += inca;
  }
  return M2 / ( double )( n - 1 );
} // variance

double stddev( const size_t n, const double* const a, const size_t inca )
// returns sample standard deviation
{
  return sqrt( variance( n, a, inca ) );
} // stddev

void doublecenter( const size_t n, double** a )
// double center square matrix
{
  double** J = getmatrix( n, n, -1.0 / ( double )( n ) );
  for ( size_t i = 1; i <= n; i++ ) J[i][i] += 1.0;
  double** H = getmatrix( n, n, 0.0 );
  dgemm( false, false, n, n, n, 1.0, J, a, 0.0, H );
  dgemm( false, false, n, n, n, 1.0, H, J, 0.0, a );
  freematrix( J );
  freematrix( H );
} // doublecenter

double fdist1( const size_t p, double* x, double* y )
{
  double* __restrict rx = &x[0];
  double* __restrict ry = &y[0];
  double sm = 0.0;
  for ( size_t i = p; i--; ) {
    const double d = *rx - *ry;
    sm += d * d;
    rx++;
    ry++;
  }
  return sqrt( sm );
} // fdist1

double fdist( const size_t n, double* x, double* y, const size_t inc )
{
  if ( n == 2 ) {
    const double d1 = *x - *y;
    const double d2 = *( x + inc ) -  *( y + inc );
    const double d1d1 = d1 * d1;
    const double d2d2 = d2 * d2;
    const double sum = d1d1 + d2d2;
    return( sqrt( sum ) );
  }
  else if ( n == 1 ) {
    const double diff = *x - *y;
    return( diff < 0.0 ? -1.0 * diff : diff );
  }
  double d;
  double sm = 0.0;
  if ( inc != 1 ) {
    for ( size_t i = n; i--; ) {
      d = *x - *y;
      sm += d * d;
      x += inc;
      y += inc;
    }
    return sqrt( sm );
  }
  size_t i = n >> 3;
  const size_t k = n & 7;
  while ( i-- ) {
    d = *x - *y; sm += d * d;
    d = *( x + 1 ) - *( y + 1 ); sm += d * d;
    d = *( x + 2 ) - *( y + 2 ); sm += d * d;
    d = *( x + 3 ) - *( y + 3 ); sm += d * d;
    d = *( x + 4 ) - *( y + 4 ); sm += d * d;
    d = *( x + 5 ) - *( y + 5 ); sm += d * d;
    d = *( x + 6 ) - *( y + 6 ); sm += d * d;
    d = *( x + 7 ) - *( y + 7 ); sm += d * d;
    x += 8;
    y += 8;
  }
  switch ( k ) {
    case 7: d = *x - *y; sm += d * d; x++; y++;
    case 6: d = *x - *y; sm += d * d; x++; y++;
    case 5: d = *x - *y; sm += d * d; x++; y++;
    case 4: d = *x - *y; sm += d * d; x++; y++;
    case 3: d = *x - *y; sm += d * d; x++; y++;
    case 2: d = *x - *y; sm += d * d; x++; y++;
    case 1: d = *x - *y; sm += d * d;
  }
  return sqrt( sm );
} // fdist

void euclidean1( const size_t n, const size_t p, double** z, double** r )
// compute euclidean distances r between rows of a and b
{
  if ( p == 2 ) {
    r[1][1] = 0.0;
    for ( size_t i = 2; i <= n; i++ ) {
      r[i][i] = 0.0;
      const double zi1 = z[i][1];
      const double zi2 = z[i][2];
      for ( size_t j = 1; j <= i - 1; j++ ) {
        const double d1 = zi1 - z[j][1];
        const double d2 = zi2 - z[j][2];
        const double d1d1 = d1 * d1;
        const double d2d2 = d2 * d2;
        const double sum = d1d1 + d2d2;
        r[i][j] = r[j][i] = sqrt( sum );
      }
    }
  }
  else if ( p == 1 ) {
    r[1][1] = 0.0;
    for ( size_t i = 2; i <= n; i++ ) {
      r[i][i] = 0.0;
      const double zi1 = z[i][1];
      for ( size_t j = 1; j <= i - 1; j++ ) {
        const double diff = zi1 - z[j][1];
        r[i][j] = r[j][i] = ( diff < 0.0 ? -1.0 * diff : diff );
      }
    }
  }
  else {
    r[1][1] = 0.0;
    for ( size_t i = 2; i <= n; i++ ) {
      r[i][i] = 0.0;
      for ( size_t j = 1; j <= i - 1; j++ ) {
        double sum = 0.0;
        for ( size_t k = 1; k <= p; k++ ) {
          const double diff = z[i][k] - z[j][k];
          sum += diff * diff;
        }
        r[i][j] = r[j][i] = sqrt( sum );
      }
    }
  }

  // for ( size_t i = 1; i <= n; i++ ) {
  //   r[i][i] = 0.0;
  //   for ( size_t j = 1; j < i; j++ ) {
  //     double sum = 0.0;
  //     for ( size_t k = 1; k <= p; k++ ) {
  //       const double diff = a[i][k] - a[j][k];
  //       if ( isnotzero( diff ) ) sum += diff * diff;
  //     }
  //     r[i][j] = r[j][i] = sqrt( sum );
  //   }
  // }
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

void pdist( const size_t n, double* d, double* w, double* r )
// create probabilities from distances with weights
{
  const double TOL = sqrt( DBL_EPSILON );
  const double wsm = dwsum( n, &d[1], 1, &w[1], 1 ) + ( double )( n ) * TOL;
  for ( size_t i = 1; i <= n; i++ ) r[i] = wsm / ( w[i] * d[i] + TOL );
  double alpha = dsum( n, &r[1], 1 );
  if ( iszero( alpha ) ) alpha = TOL;
  dscal( n, 1.0 / alpha, &r[1], 1 );
} // pdist

static void dinsertion0( const size_t n, double* const a )
// fast insertion sort for small arrays
{
  for ( size_t i = n - 1; i > 0; --i ) {
    size_t j = i;
    const double val = a[i];
    for ( ; j < n && a[j + 1] < val; ++j ) a[j] = a[j + 1];
    a[j] = val;
  }
} // dinsertion

static void dinsertion( const size_t n, double* const a, size_t* const r )
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
} // dinsertion

static void dinsertion2( const size_t n, double* const a, double* const b, size_t* const r )
// fast insertion sort for small arrays
{
  for ( size_t i = n - 1; i > 0; --i ) {
    size_t j = i;
    const double vala = a[i];
    const double valb = b[i];
    const size_t ind = r[i];
    for ( ; j < n && a[j + 1] < vala; ++j ) {
      a[j] = a[j + 1];
      b[j] = b[j + 1];
      r[j] = r[j + 1];
    }
    a[j] = vala;
    b[j] = valb;
    r[j] = ind;
  }
} // dinsertion2

void dsort0( const size_t n, double* const a )
// replaces double vector with elements in increasing order
{
  // fast return
  if ( n <= 1 ) return;

  // fast sort for small vectors
  if ( n <= 16 ) {
    dinsertion0( n, a );
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

void dsort( const size_t n, double* const a, size_t* const r )
// replaces double vector with elements in increasing order
// also return the ordered indices in r
{
  // fast return
  if ( n <= 1 ) return;

  // fast sort for small vectors
  if ( n <= 16 ) {
    dinsertion( n, a, r );
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

void dsort2( const size_t n, double* const a, double* const b, size_t* const r )
// replaces double vector with elements in increasing order
// also return the ordered vector b and indices in r
{
  // fast return
  if ( n <= 1 ) return;

  // fast sort for small vectors
  if ( n <= 16 ) {
    dinsertion2( n, a, b, r );
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
            dmnmx = b[j];
            b[j] = b[j - 1];
            b[j - 1] = dmnmx;
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
          dtmp = b[i];
          b[i] = b[j];
          b[j] = dtmp;
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
} // dsort2

static void insertion_t( const size_t n, size_t* const a )
// fast insertion sort for small arrays
{
  for ( size_t i = n - 1; i > 0; --i ) {
    size_t j = i;
    const size_t val = a[i];
    for ( ; j < n && a[j + 1] < val; ++j ) a[j] = a[j + 1];
    a[j] = val;
  }
} // insertion_t

static void insertion_t2( const size_t n, size_t* const a, double* const b )
// fast insertion sort for small arrays
{
  for ( size_t i = n - 1; i > 0; --i ) {
    size_t j = i;
    const size_t vala = a[i];
    const double valb = b[i];
    for ( ; j < n && a[j + 1] < vala; ++j ) {
      a[j] = a[j + 1];
      b[j] = b[j + 1];
    }
    a[j] = vala;
    b[j] = valb;
  }
} // insertion_t

void sort_t( const size_t n, size_t* const a )
// replaces double vector with elements in increasing order
// also return the ordered indices in r
{
  // fast return
  if ( n <= 1 ) return;

  // fast sort for small vectors
  if ( n <= 16 ) {
    insertion_t( n, a );
    return;
  }

  const size_t select = 20;
  size_t i, j;
  size_t dmnmx;

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
          }
          else break;
        }
      }
    }
    else if ( endd - start > select ) {
      const size_t d1 = a[start];
      const size_t d2 = a[endd];
      i = ( start + endd ) / 2;
      const size_t d3 = a[i];
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
          size_t dtmp = a[i];
          a[i] = a[j];
          a[j] = dtmp;
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
} // sort_t

void sort_t2( const size_t n, size_t* const a, double* const b )
// replaces double vector with elements in increasing order
// also return the ordered indices in r
{
  // fast return
  if ( n <= 1 ) return;

  // fast sort for small vectors
  if ( n <= 16 ) {
    insertion_t2( n, a, b );
    return;
  }

  const size_t select = 20;
  size_t i, j;
  size_t dmnmx;

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
            const double dtmp = b[j];
            b[j] = b[j - 1];
            b[j - 1] = dtmp;
          }
          else break;
        }
      }
    }
    else if ( endd - start > select ) {
      const size_t d1 = a[start];
      const size_t d2 = a[endd];
      i = ( start + endd ) / 2;
      const size_t d3 = a[i];
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
          const size_t ttmp = a[i];
          a[i] = a[j];
          a[j] = ttmp;
          const double dtmp = b[i];
          b[i] = b[j];
          b[j] = dtmp;
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
} // sort_t

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

double scale( const size_t n, const size_t m, double** a, double** b )
{
  double upper = 0.0;
  double lower = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= m; j++ ) {
      const double aij = a[i][j];
      const double bij = b[i][j];
      upper += bij * bij;
      lower += aij * bij;
    }
  }
  return( upper / lower );
}

void center( const size_t n, const size_t p, double** z )
{
  for ( size_t k = 1; k <= p; k++ ) {
    double sm = 0.0;
    for ( size_t i = 1; i <= n; i++ ) sm += z[i][k];
    const double mn = sm / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) z[i][k] -= mn;
  }
} // center

double nstress( const size_t n, double** delta, double** d, double** w )
{
  if ( isnull( w ) ) {
    double upper = 0.0;
    double lower = 0.0;
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
        const double work = delta[i][j];
        const double dij = d[i][j];
        upper += dij * dij;
        lower += work * dij;
      }
    }
    if ( iszero( lower ) ) return( 1.0 );
    const double alpha = upper / lower;
    double fnew = 0.0;
    double scale = 0.0;
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
        const double gij = alpha * delta[i][j];
        const double work = gij - d[i][j];
        fnew += work * work;
        scale += gij * gij;
      }
    }
    if ( iszero( scale ) ) return( 1.0 );
    return( fnew / scale );
  }

  double upper = 0.0;
  double lower = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double wij = w[i][j];
      if ( isnotzero( wij ) ) {
        const double work = delta[i][j];
        const double dij = d[i][j];
        upper += wij * dij * dij;
        lower += wij * work * dij;
      }
    }
  }
  if ( iszero( lower ) ) return( 1.0 );
  const double alpha = upper / lower;
  double fnew = 0.0;
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double wij = w[i][j];
      if ( isnotzero( wij ) ) {
        const double gij = alpha * delta[i][j];
        const double work = gij - d[i][j];
        fnew += wij * work * work;
        scale += wij * gij * gij;
      }
    }
  }
  if ( iszero( scale ) ) return( 1.0 );
  return( fnew / scale );
} // nstress

double pearson( const size_t n, double* a, double* b, double* w )
// pearson’s correlation coefficient is a measure of linear association
// two variables can be perfectly related, but if the relationship is not linear,
// pearson’s correlation coefficient is not an appropriate statistic for measuring their association.
{
  // fast return
  if ( n == 0 ) return 0.0;
  if ( n == 1 ) return 0.0;
  if ( n == 2 ) return 1.0;

  // initialize
  double sigw = 0.0;
  double cros = 0.0;
  double suma = 0.0;
  double sumb = 0.0;
  double ssqa = 0.0;
  double ssqb = 0.0;

  // loop over data
  for ( size_t i = 1; i <= n; i++ ) {
    const double wi = w[i];
    const double ai = a[i];
    const double bi = b[i];
    sigw += wi;
    cros += wi * ai * bi;
    suma += wi * ai;
    sumb += wi * bi;
    ssqa += wi * ai * ai;
    ssqb += wi * bi * bi;
  }

  // final calculations
  const double upper = sigw * cros - suma * sumb;
  const double vara = sigw * ssqa - suma * suma;
  const double varb = sigw * ssqb - sumb * sumb;

  // check variances
  if ( vara <= 0.0 || varb <= 0.0 ) return 0.0;

  // pearson is between -1.0 and 1.0 and lower is greater than zero
  const double stda = sqrt( vara );
  const double stdb = sqrt( varb );
  const double lower = stda * stdb;
  const double work = upper / lower;

  // return within boundaries (roundoff error)
  if ( work < -1.0 ) return -1.0;
  if ( work > 1.0 ) return  1.0;

  return work;
} // pearson

static double rcond( const size_t n, double** a, double** inva )
// return reciprocal condition number matrix a
{
  double norm1a = 0.0;
  for ( size_t j = 1; j <= n; j++ ) {
    double work = 0.0;
    for ( size_t i = 1; i <= n; i++ ) work += fabs( a[i][j] );
    norm1a = fmax( norm1a, work );
  }
  double norm1inva = 0.0;
  for ( size_t j = 1; j <= n; j++ ) {
    double work = 0.0;
    for ( size_t i = 1; i <= n; i++ ) work += fabs( inva[i][j] );
    norm1inva = fmax( norm1inva, work );
  }
  double rcond = ( 1.0 / norm1a ) / norm1inva;
  return rcond;
}

double covariance( const size_t n, const double* const x, const size_t incx, const double* const y, const size_t incy )
// computes the sample covariance using an online algorithm
{
  double accum = 0.0;
  double xbar = 0.0;
  double ybar = 0.0;
  size_t ix = 0;
  size_t iy = 0;
  double scale = 0.0;
  for ( size_t i = 0; i < n; i++ ) {
    scale += 1.0;
    const double diffx = x[ix] - xbar;
    const double diffy = y[iy] - ybar;
    xbar += diffx / scale;
    ybar += diffy / scale;
    accum += ( scale - 1.0 ) * ( diffx / scale ) * ( diffy / scale ) - accum / scale;
    ix += incx;
    iy += incy;
  }
  return accum;
} // covariance

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
  if ( a == 0 ) return;
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a );
  _Pragma("GCC diagnostic pop")
  a = 0;
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
  if ( a == 0 ) return;
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

double ***gettensor( const size_t ns, const size_t nr, const size_t nc, const double c )
// allocates tensor space on the heap
{
  double*** ptr = 0;
  if ( ns == 0 || nr == 0 || nc == 0 ) return ptr;
  double** temp = 0;
  double* block = 0;
  ptr = ( double *** ) calloc( ns, sizeof( double ** ) );
  temp = ( double ** ) calloc( ns * nr, sizeof( double* ) );
  block = ( double * ) calloc( ns * nr * nc, sizeof( double ) );
  ptr--;
  temp--;
  block--;
  for ( size_t k = 1, km1 = 0; k <= ns; k++, km1++, temp += nr ) {
    ptr[k] = temp;
    for ( size_t i = 1, im1 = 0; i <= nr; i++, im1++ ) {
      ptr[k][i] = &block[km1 * nr * nc + im1 * nc];
      for ( size_t j = 1; j <= nc; j++ ) ptr[k][i][j] = c;
    }
  }
  return ptr;
} // gettensor

void freetensor( double*** a )
// de-allocates tensor space from the heap
{
  if ( a == 0 ) return;
  _Pragma("GCC diagnostic push")
  _Pragma("GCC diagnostic ignored \"-Wfree-nonheap-object\"")
  free( ++a[1][1] ); 
  free( ++a[1] ); 
  free( ++a );
  _Pragma("GCC diagnostic pop")
} // freetensor

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// linear algebra functions
//

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

static int Choleski_LDU_Decomposition( double* a, size_t n )
{
  double *p_i = a + n;
  for ( size_t i = 1; i < n; p_i += n, i++ ) {
    double *p_j = a;
    for ( size_t j = 0; j < i; j++, p_j += n ) {
      for ( size_t k = 0; k < j; k++ ) *( p_i + j ) -= *( p_i + k ) * *( p_j + k );
    }
    double *p_k = a;
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

static int Tridiagonalize( double* V, double* d, double* e, size_t n )
// Symmetric Householder reduction to tridiagonal form, derived from the algol procedure tred2 
// by Bowdler, Martin, Reinsch, and Wilkinson, Handbook for auto. Comp., Vol.ii-Linear algebra, 
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

static int Diagonalize( double *V, double *d, double *e, size_t n )
// Symmetric tridiagonal QL algorithm, derived from the algol procedure tql2, 
// by Bowdler, Martin, Reinsch, and Wilkinson, Handbook for auto. Comp., Vol.ii-Linear algebra, 
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

static int chdcmp( double* a, size_t n )
{
  double *p_Lk0 = a;
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

int evdcmp( const size_t n, double** vecs, double* vals )
// eigenvalue decomposition a = vwv'
// matrix vecs is replaced by the eigenvector matrix vecs
// the eigenvalues are returned in vector vals
{
  int retval = 0;

  retval = Eigen_Value_Decomposition( vecs, vals, n );

  return retval;
} // evdcmp

static int lanczos_matrix( const size_t n, double** a, const size_t m, double* alpha, double* beta, double** v )
// tridiagonalization using Lanczos algorithm
// symmetric matrix A, n x n, reduced to tridiagonal matrix, m x m, with alpha as diagonal and beta as subdiagonal
// also return v for computing full eigenvectors
// function return zero on success, nonzero otherwise
{
  // fast return
  if ( n == 0 ) return 1;

  // constants
  const size_t MAXGS = 4;
  const double TOL = 1.8189894035458617e-12;
  const double UCRIT = 0.5;

  // allocate memory
  double* u = getvector( n, 0.0 );

  // initialize variables: v is a vector of unit norm
  double norm = sqrt( 6.0 / ( double )( n * ( n + 1 ) * ( 2 * n + 1 ) ) );
  for ( size_t i = 1; i <= n; i++ ) v[1][i] = norm * ( double )( i );

  // main loop
  double nnew = 0.0;
  for ( size_t j = 1; j <= m; j++ ) {
    for ( size_t i = 1; i <= n; i++ ) u[i] = ddot( n, &a[i][1], 1, &v[j][1], 1 );
    alpha[j] = ddot( n, &v[j][1], 1, &u[1], 1 );
    if ( j == m ) break;

    // re-orthogonalization (modified Gram-Schmidt)
    nnew = sqrt( dssq( n, &u[1], 1 ) );
    for ( size_t iter = 1; iter <= MAXGS; iter++ ) {
      double nold = nnew;
      for ( size_t i = 1; i <= j; i++ ) {
        double r = ddot( n, &u[1], 1, &v[i][1], 1 );
        daxpy( n, -1.0 * r, &v[i][1], 1, &u[1], 1 );
      }
      if ( iter == MAXGS ) dset( n, 0.0, &u[1], 1 );
      nnew = sqrt( dssq( n, &u[1], 1 ) );
      if ( nnew >= UCRIT * nold ) break;
    }
    beta[j] = nnew;
    if ( nnew < TOL ) break;
    daxpy( n, 1.0 / nnew, &u[1], 1, &v[j + 1][1], 1 );
  }

  // de-allocate memory
  freevector( u );

  return ( nnew < TOL ? 0 : 1 );
} // lanczos_matrix

static int lanczos_qr( const size_t n, double* alpha, double* beta, double* vals, double** vecs )
// implicit shift QR for finding eigenvalues and vectors from tridiagonal matrix with diagonal alpha and subdiagonal beta
{
  int retval = 0;
  const size_t MAXQR = 30;
  size_t iter = 0;
  dcopy( n, &alpha[1], 1, &vals[1], 1 );
  dset( n * n, 0.0, &vecs[1][1], 1 );
  for ( size_t i = 1; i <= n; i++ ) vecs[i][i] = 1.0;
  beta[n] = 0.0;
  for ( size_t l = 1; l <= n; l++ ) {
    for ( iter = 1; iter <= MAXQR; iter++ ) {
      size_t m = 0;
      for ( m = l; m <= n - 1; m++ ) {
        double dd = fabs( vals[m] ) + fabs( vals[m + 1] );
        if ( fabs( beta[m] ) + dd == dd ) break;
      }
      if ( m != l ) {
        double g = ( vals[l + 1] - vals[l] ) / ( 2.0 * beta[l] );
        double r = pythag( 1.0, g );
        g = vals[m] - vals[l] + beta[l] / ( g + sign( r, g ) );
        double s = 1.0;
        double c = 1.0;
        double p = 0.0;
        for ( size_t i = m - 1; i >= l; i-- ) {
          double f = s * beta[i];
          double b = c * beta[i];
          if ( fabs( f ) >= fabs( g ) ) {
            c = g / f;
            r = pythag( c, 1.0 );
            beta[i + 1] = f * r;
            s = 1.0 / r;
            c = c * s;
          }
          else {
            s = f / g;
            r = pythag( s, 1.0 );
            beta[i + 1] = g * r;
            c = 1.0 / r;
            s = s * c;
          }
          g = vals[i + 1] - p;
          r = ( vals[i] - g ) * s + 2.0 * c * b;
          p = s * r;
          vals[i + 1] = g + p;
          g = c * r - b;
          for ( size_t j = 1; j <= n; j++ ) {
            f = vecs[j][i + 1];
            vecs[j][i + 1] = s * vecs[j][i] + c * f;
            vecs[j][i] = c * vecs[j][i] - s * f;
          }
        }
        vals[l] = vals[l] - p;
        beta[l] = g;
        beta[m] = 0.0;
      }
    }
  }
  return retval;
} // lanczos_qr

static int lanczos_ritz( const size_t n, const size_t m, double** v, double** q, double** vecs )
// determine Ritz vectors (eigenvectors of A based on eigenvectors of Q and Lanczos' V)
{
  int retval = 0;
  for ( size_t j = 1; j <= n; j++ ) {
    for ( size_t i = 1; i <= m; i++ ) {
      double work = 0.0;
      for ( size_t k = 1; k <= m; k++ ) work += v[k][j] * q[k][i];
      vecs[j][i] = work;
    }
  }
  return retval;
} // lanczos_ritz

static int lanczos_sort( const size_t n, const size_t m, double** vecs, double* vals )
// sort eigenvalues and corresponding vectors
{
  int retval = 0;
  for ( size_t i = 1; i <= m - 1; i++ ) {
    size_t ii = i;
    double work = vals[i];
    for ( size_t j = i + 1; j <= m; j++ ) if ( vals[j] > work ) work = vals[ii = j];
    if ( ii != i ) {
      vals[ii] = vals[i];
      vals[i] = work;
      for ( size_t j = 1; j <= n; j++ ) {
        double d = vecs[j][i];
        vecs[j][i] = vecs[j][ii];
        vecs[j][ii] = d;
      }
    }
  }
  return retval;
} // lanczos_sort

int evdcmp_lanczos( const size_t n, double** a, double** vecs, double* vals, const size_t k )
// lanczos truncated eigenvalue decomposition of symmetric matrix A, n x n
// eigenvalues returned in n-vector vals
// eigenvectors returned in n x k matrix vecs
// on failure, foolproof jacobi is used instead
// function return zero on success, nonzero otherwise
{
  int retval = 0;
  const size_t m = min_t( n, 10 + 2 * k );

  double* mvals = getvector( m, 0.0 );
  double** mvecs = getmatrix( n, m, 0.0 );

  double* alpha = getvector( m, 0.0 );
  double* beta = getvector( m, 0.0 );
  double** v = getmatrix( m, n, 0.0 );
  double** q = getmatrix( m, m, 0.0 );

  retval = lanczos_matrix( n, a, m, alpha, beta, v );
  if ( 0 == retval ) {
    retval = lanczos_qr( m, alpha, beta, mvals, q );
    if ( 0 == retval ) {
      retval = lanczos_ritz( n, m, v, q, mvecs );
      if ( 0 == retval ) {
        retval = lanczos_sort( n, m, mvecs, mvals );
        if ( 0 == retval ) {
          for ( size_t j = 1; j <= k; j++ ) {
            vals[j] = mvals[j];
            for ( size_t i = 1; i <= n; i++ ) vecs[i][j] = mvecs[i][j];
          }
        }
      }
    }
  }

  // de-allocate memory
  freevector( alpha );
  freevector( beta );
  freematrix( v );
  freematrix( q );

  freevector( mvals );
  freematrix( mvecs );

  return retval;
} // evdcmp_lanczos

static void Householders_Reduction_to_Bidiagonal_Form( double* A, size_t nrows, size_t ncols, double* U, double* V, double* diagonal, double* superdiagonal )
{
  // Copy A to U
  memcpy( U, A, sizeof( double ) * nrows * ncols );

  diagonal[0] = 0.0;
  double s = 0.0;
  double scale = 0.0;
  double *pui = U;
  for ( size_t i = 0, ip1 = 1; i < ncols; pui += ncols, i++, ip1++ ) {
    superdiagonal[i] = scale * s;

    // Perform Householder transform on columns.
    // Calculate the normed squared of the i-th column vector starting at row i.
    scale = 0.0;
    double *pu = pui;
    for ( size_t j = i; j < nrows; j++, pu += ncols ) scale += fabs( *( pu + i ) );
    if ( scale > 0.0 ) {
      double s2 = 0.0;
      pu = pui;
      for ( size_t j = i; j < nrows; j++, pu += ncols ) {
        *( pu + i ) /= scale;
        s2 += *( pu + i ) * *( pu + i );
      }

      // Chose sign of s which maximizes the norm
      s = ( *( pui + i ) < 0.0 ) ? sqrt( s2 ) : -sqrt( s2 );

      // Calculate -2/u'u
      double half_norm_squared = *( pui + i ) * s - s2;

      // Transform remaining columns by the Householder transform.
      *( pui + i ) -= s;
      for ( size_t j = ip1; j < ncols; j++ ) {
        double si = 0.0;
        pu = pui;
        for ( size_t k = i; k < nrows; k++, pu += ncols ) si += *( pu + i ) * *( pu + j );
        si /= half_norm_squared;
        pu = pui;
        for ( size_t k = i; k < nrows; k++, pu += ncols ) *( pu + j ) += si * *( pu + i );
      }
    }
    pu = pui;
    for ( size_t j = i; j < nrows; j++, pu += ncols ) *( pu + i ) *= scale;
    diagonal[i] = s * scale;

    // Perform Householder transform on rows.
    // Calculate the normed squared of the i-th row vector starting at column i.
    s = 0.0;
    scale = 0.0;
    if ( i >= nrows || i == ( ncols - 1 ) ) continue;
    for ( size_t j = ip1; j < ncols; j++ ) scale += fabs( *( pui + j ) );
    if ( scale > 0.0 ) {
      double s2 = 0.0;
      for ( size_t j = ip1; j < ncols; j++ ) {
        *( pui + j ) /= scale;
        s2 += *( pui + j ) * *( pui + j );
      }
      s = ( *( pui + ip1 ) < 0.0 ) ? sqrt( s2 ) : -sqrt( s2 );

      // Calculate -2/u'u
      double half_norm_squared = *( pui + ip1 ) * s - s2;

      // Transform the rows by the Householder transform.
      *( pui + ip1 ) -= s;
      for ( size_t k = ip1; k < ncols; k++ ) superdiagonal[k] = *( pui + k ) / half_norm_squared;
      if ( i < ( nrows - 1 ) ) {
        pu = pui + ncols;
        for ( size_t j = ip1; j < nrows; j++, pu += ncols ) {
          double si = 0.0;
          for ( size_t k = ip1; k < ncols; k++ ) si += *( pui + k ) * *( pu + k );
          for ( size_t k = ip1; k < ncols; k++ ) *( pu + k ) += si * superdiagonal[k];
        }
      }
      for ( size_t k = ip1; k < ncols; k++ ) *( pui + k ) *= scale;
    }
  }

  // Update V
  pui = U + ncols * ( ncols - 2 );
  double *pvi = V + ncols * ( ncols - 1 );
  *( pvi + ncols - 1 ) = 1.0;
  s = superdiagonal[ncols - 1];
  pvi -= ncols;
  double *pv = 0;
  for ( long long i = ncols - 2, ip1 = ncols - 1; i >= 0; i--, pui -= ncols, pvi -= ncols, ip1-- ) {
    if ( isnotzero( s ) ) {
      pv = pvi + ncols;
      for ( size_t j = ip1; j < ncols; j++, pv += ncols ) *( pv + i ) = ( *( pui + j ) / *( pui + ip1 ) ) / s;
      for ( size_t j = ip1; j < ncols; j++ ) {
        double si = 0.0;
        pv = pvi + ncols;
        for ( size_t k = ip1; k < ncols; k++, pv += ncols ) si += *( pui + k ) * *( pv + j );
        pv = pvi + ncols;
        for ( size_t k = ip1; k < ncols; k++, pv += ncols ) *( pv + j ) += si * *( pv + i );
      }
    }
    pv = pvi + ncols;
    for ( size_t j = ip1; j < ncols; j++, pv += ncols ) {
      *( pvi + j ) = 0.0;
      *( pv + i ) = 0.0;
    }
    *( pvi + i ) = 1.0;
    s = superdiagonal[i];
  }

  // Update U
  pui = U + ncols * ( ncols - 1 );
  double *pu = 0;
  for ( long long i = ncols - 1, ip1 = ncols; i >= 0; ip1 = i, i--, pui -= ncols ) {
    s = diagonal[i];
    for ( size_t j = ip1; j < ncols; j++ ) *( pui + j ) = 0.0;
    if ( isnotzero( s ) ) {
      for ( size_t j = ip1; j < ncols; j++ ) {
        double si = 0.0;
        pu = pui + ncols;
        for ( size_t k = ip1; k < nrows; k++, pu += ncols ) si += *( pu + i ) * *( pu + j );
        si = ( si / *( pui + i ) ) / s;
        pu = pui;
        for ( size_t k = i; k < nrows; k++, pu += ncols ) *( pu + j ) += si * *( pu + i );
      }
      pu = pui;
      for ( size_t j = i; j < nrows; j++, pu += ncols ) *( pu + i ) /= s;
    }
    else {
      pu = pui;
      for ( size_t j = i; j < nrows; j++, pu += ncols ) *( pu + i ) = 0.0;
    }
    *( pui + i ) += 1.0;
  }
} // Householders_Reduction_to_Bidiagonal_Form

static int Givens_Reduction_to_Diagonal_Form( size_t nrows, size_t ncols, double* U, double* V, double* diagonal, double* superdiagonal )
{
  const size_t MAX_ITERATION_COUNT = 64;

  double x = 0.0;
  for ( size_t i = 0; i < ncols; i++ ) {
    double y = fabs( diagonal[i] ) + fabs( superdiagonal[i] );
    if ( x < y ) x = y;
  }
  double epsilon = x * DBL_EPSILON;
  for ( long long llk = ncols - 1; llk >= 0; llk-- ) {
    size_t k = ( size_t )( llk );
    size_t iteration_count = 0;
    while ( true ) {
      bool rotation_test = true;
      size_t m = k;
      for ( long long llm = k; llm >= 0; llm--, m-- ) {
        if ( fabs( superdiagonal[m] ) <= epsilon ) {
          rotation_test = false; 
          break;
        }
        if ( fabs( diagonal[m - 1] ) <= epsilon ) break;
      }
      if ( rotation_test == true ) {
        double c = 0.0;
        double s = 1.0;
        for ( size_t i = m; i <= k; i++ ) {
          double f = s * superdiagonal[i];
          superdiagonal[i] *= c;
          if ( fabs( f ) <= epsilon ) break;
          double g = diagonal[i];
          double h = sqrt( f * f + g * g );
          diagonal[i] = h;
          c = g / h;
          s = -f / h;
          double *pu = U;
          for ( size_t j = 0; j < nrows; j++, pu += ncols ) {
            double y = *( pu + m - 1 );
            double z = *( pu + i );
            *( pu + m - 1 ) = y * c + z * s;
            *( pu + i ) = -y * s + z * c;
          }
        }
      }
      double z = diagonal[k];
      if ( m == k ) {
        if ( z < 0.0 ) {
          diagonal[k] = -z;
          double *pv = V;
          for ( size_t j = 0; j < ncols; j++, pv += ncols ) *( pv + k ) = -*( pv + k );
        }
        break;
      }
      else {
        if ( iteration_count >= MAX_ITERATION_COUNT ) return -1;
        iteration_count++;
        x = diagonal[m];
        double y = diagonal[k - 1];
        double g = superdiagonal[k - 1];
        double h = superdiagonal[k];
        double f = ( ( y - z ) * ( y + z ) + ( g - h ) * ( g + h ) ) / ( 2.0 * h * y );
        g = sqrt( f * f + 1.0 );
        if ( f < 0.0 ) g = -g;
        f = ( ( x - z ) * ( x + z ) + h * ( y / ( f + g ) - h ) ) / x;

        // Next QR Transformtion
        double c = 1.0;
        double s = 1.0;
        for ( size_t i = m + 1; i <= k; i++ ) {
          g = superdiagonal[i];
          y = diagonal[i];
          h = s * g;
          g *= c;
          z = sqrt( f * f + h * h );
          superdiagonal[i - 1] = z;
          c = f / z;
          s = h / z;
          f = x * c + g * s;
          g = -x * s + g * c;
          h = y * s;
          y *= c;
          double *pv = V;
          for ( size_t j = 0; j < ncols; j++, pv += ncols ) {
            x = *( pv + i - 1 );
            z = *( pv + i );
            *( pv + i - 1 ) = x * c + z * s;
            *( pv + i ) = -x * s + z * c;
          }
          z = sqrt( f * f + h * h );
          diagonal[i - 1] = z;
          if ( isnotzero( z ) ) {
            c = f / z;
            s = h / z;
          }
          f = c * g + s * y;
          x = -s * g + c * y;
          double *pu = U;
          for ( size_t j = 0; j < nrows; j++, pu += ncols ) {
            y = *( pu + i - 1 );
            z = *( pu + i );
            *( pu + i - 1 ) = c * y + s * z;
            *( pu + i ) = -s * y + c * z;
          }
        }
        superdiagonal[m] = 0.0;
        superdiagonal[k] = f;
        diagonal[k] = x;
      }
    }
  }
  return 0;
} // Givens_Reduction_to_Diagonal_Form

static void Sort_by_Decreasing_Singular_Values( size_t nrows, size_t ncols, double* singular_values, double* U, double* V )
{
  for ( size_t i = 0; i < ncols - 1; i++ ) {
    size_t max_index = i;
    for ( size_t j = i + 1; j < ncols; j++ ) if ( singular_values[j] > singular_values[max_index] ) max_index = j;
    if ( max_index == i ) continue;
    double temp = singular_values[i];
    singular_values[i] = singular_values[max_index];
    singular_values[max_index] = temp;
    double *p1 = U + max_index;
    double *p2 = U + i;
    for ( size_t j = 0; j < nrows; j++, p1 += ncols, p2 += ncols ) {
      temp = *p1;
      *p1 = *p2;
      *p2 = temp;
    }
    p1 = V + max_index;
    p2 = V + i;
    for ( size_t j = 0; j < ncols; j++, p1 += ncols, p2 += ncols ) {
      temp = *p1;
      *p1 = *p2;
      *p2 = temp;
    }
  }
} // Sort_by_Decreasing_Singular_Values

static int Singular_Value_Decomposition( double* A, size_t nrows, size_t ncols, double* U, double* singular_values, double* V )
{
  double* dummy_array = getvector( ncols, 0.0 );
  Householders_Reduction_to_Bidiagonal_Form( A, nrows, ncols, U, V, singular_values, &dummy_array[1] );
  int retval = Givens_Reduction_to_Diagonal_Form( nrows, ncols, U, V, singular_values, &dummy_array[1] );
  freevector( dummy_array );
  if ( retval >= 0 ) Sort_by_Decreasing_Singular_Values( nrows, ncols, singular_values, U, V );
  return retval;
} // Singular_Value_Decomposition

int svdcmp( const size_t n, const size_t m, double** const a, double** const u, double* w, double** const v )
// singular value decomposition: a = uwvt,
// where a(n,m), u(n,n), w(MIN(n,m)), and v(m,m)
{
  int retval = 0;

  if ( n > m ) {
    double** uu = getmatrix( n, m, 0.0 );
    retval = Singular_Value_Decomposition( &a[1][1], n, m, &uu[1][1], &w[1], &v[1][1] );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= m; j++ ) u[i][j] = uu[i][j];
    freematrix( uu );
  }
  else if ( n == m ) {
    retval = Singular_Value_Decomposition( &a[1][1], n, m, &u[1][1], &w[1], &v[1][1] );
  }
  else {
    double** aa = getmatrix( m, n, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= m; j++ ) aa[j][i] = a[i][j];
    double** vv = getmatrix( m, n, 0.0 );
    retval = Singular_Value_Decomposition( &aa[1][1], m, n, &vv[1][1], &w[1], &u[1][1] );
    for ( size_t i = 1; i <= m; i++ ) for ( size_t j = 1; j <= n; j++ ) v[i][j] = vv[i][j];
    freematrix( vv );
    freematrix( aa );
  }

  return retval;
} // svdcmp

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
  if ( isalmostzero( det ) ) {
    const double norma = fmax( fabs( a[1][1] ) + fabs( a[2][1] ), fabs( a[1][2] ) + fabs( a[2][2] ) );
    const double norminva = fmax( fabs( a[2][2] / det ) + fabs( - a[2][1] / det ), fabs( - a[1][2] / det ) + fabs( a[1][1] / det ) );
    if ( ( 1.0 / norma ) / norminva < DBL_EPSILON ) return 2;
  }
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

static int luinverse( const size_t n, double** const a ) 
{
  int retval = 0;

  double** inva = getmatrix( n, n, 0.0 );
  dcopy( n * n, &a[1][1], 1, &inva[1][1], 1);

  bool doublecheck = false;

  size_t* idx = getvector_t( n, 0 );
  for ( size_t j = 1; j <= n; j++ ) {
    size_t jp = j - 1 + iamax( n - j + 1, &inva[j][j], n );
    idx[j] = jp;
    if ( isnotzero( inva[jp][j] ) ) {
      if ( jp != j ) dswap( n, &inva[j][1], 1, &inva[jp][1], 1 );
      if ( j < n ) for ( size_t i = j + 1; i <= n; i++ ) inva[i][j] /= inva[j][j];
    }
    else {
      freematrix( inva );
      freevector_t( idx );
      return retval = ( int )( j );
    }
    if ( j < n ) {
      for ( size_t i = j + 1; i <= n; i++ ) {
        for ( size_t ii = j + 1; ii <= n; ii++ ) inva[i][ii] -= inva[i][j] * inva[j][ii];
      }
    }
  }
  for ( size_t j = 1; j <= n; j++ ) {
    if ( iszero( inva[j][j] ) ) {
      freematrix( inva );
      freevector_t( idx );
      return retval = ( int )( j );
    }
    else if ( fabs( inva[j][j] ) < sqrt( DBL_EPSILON ) ) doublecheck = true;
  }
  for ( size_t j = 1; j <= n; j++ ) {
    inva[j][j] = 1.0 / inva[j][j];
    const double ajj = -inva[j][j];
    for ( size_t i = 1; i <= j - 1; i++ ) {
      if ( isnotzero( inva[i][j] ) ) {
        const double temp = inva[i][j];
        for ( size_t ii = 1; ii <= i - 1; ii++ ) inva[ii][j] += temp * inva[ii][i];
        inva[i][j] *= inva[i][i];
      }
    }
    dscal( j - 1, ajj, &inva[1][j], n );
  }
  double* v = getvector( n, 0.0 );
  for ( size_t j = n - 1; j >= 1; j-- ) {
    for ( size_t i = j + 1; i <= n; i++ ) {
      v[i] = inva[i][j];
      inva[i][j] = 0.0;
    }
    for ( size_t jj = j + 1, k = 1; jj <= n; jj++, k++ ) {
      const double work = -1.0 * v[jj];
      for ( size_t ii = 1; ii <= n; ii++ ) inva[ii][j] += work * inva[ii][jj];
    }
  }
  for ( size_t j = n - 1; j >= 1; j-- ) {
    size_t jp = idx[j];
    if ( jp != j ) dswap( n, &inva[1][j], n, &inva[1][jp], n );
  }
  freevector_t( idx );
  freevector( v );

  // check reciprocal of condition number
  if ( retval == 0 && doublecheck == true ) {
    double rcna = rcond( n, a, inva );
    if ( rcna < DBL_EPSILON ) retval = -1;  // dismiss solution
  }

  if ( retval == 0 ) dcopy( n * n, &inva[1][1], 1, &a[1][1], 1 );
  freematrix( inva );

  return retval;
}

int inverse( const size_t n, double** a )
// compute inverse from real square matrix a
// inverse is returned in matrix a
{
  int retval = 0;
  double** inva = getmatrix( n, n, 0.0 );
  dcopy( n * n, &a[1][1], 1, &inva[1][1], 1 );
  if ( n == 1 ) retval = inverse1x1( n, a );
  if ( n == 2 ) retval = inverse2x2( n, a );
  if ( n == 3 ) retval = inverse3x3( n, a );
  if ( n == 4 ) retval = inverse4x4( n, a );
  if ( n >= 5 || retval != 0 ) {
    dcopy( n * n, &inva[1][1], 1, &a[1][1], 1 );
    retval = luinverse( n, a );
    if ( retval != 0 ) {
      dcopy( n * n, &inva[1][1], 1, &a[1][1], 1 );
      double** u = getmatrix( n, n, 0.0 );
      double* w = getvector( n, 0.0 );
      double** v = getmatrix( n, n, 0.0 );
      retval = svdcmp( n, n, a, u, w, v );
      if ( retval == 0 ) {
        const double TOL = ( double )( n ) * w[1] * DBL_EPSILON;
        double** vw = getmatrix( n, n, 0.0 );
        for ( size_t in = 1; in <= n; in++ ) {
          double alpha = ( w[in] <= TOL ? 0.0 : 1.0 / w[in] );
          daxpy( n, alpha, &v[1][in], n, &vw[1][in], n );
        }
        dgemm(false, true, n, n, n, 1.0, vw, u, 0.0, a );
        freematrix( vw );
      }
      else dcopy( n * n, &inva[1][1], 1, &a[1][1], 1 );
      freematrix( u );
      freevector( w );
      freematrix( v );
    }
  }
  freematrix( inva );
  return retval;
} // inverse

int mpinverse( const size_t n, const size_t m, double** const a, double** const inva )
// compute moore-penrose generalized inverse based on singular value decomposition a = uwv'
// inverse vw^{-1}u' is returned in inva
{
  int retval = 0;

  // allocate memory
  double** b = getmatrix( n, m, 0.0 );
  dcopy( n * m, &a[1][1], 1, &b[1][1], 1 );
  double** u = getmatrix( n, n, 0.0 );
  double* w = getvector( min_t( n, m ), 0.0 );
  double** v = getmatrix( m, m, 0.0 );

  // reduce matrix a by orthogonal transformations u and v to diagonal form
  retval = svdcmp( n, m, b, u, w, v );
  if ( retval == 0 ) {

    // determine tolerance limit
    const double TOL = ( double )( max_t( m, n ) ) * w[1] * DBL_EPSILON;

    // determine inverse
    if ( n >= m ) {
      double** vw = getmatrix( m, n, 0.0 );
      for ( size_t im = 1; im <= m; im++ ) {
        double alpha = ( w[im] <= TOL ? 0.0 : 1.0 / w[im] );
        daxpy( m, alpha, &v[1][im], m, &vw[1][im], n );
      }
      dgemm(false, true, m, n, n, 1.0, vw, u, 0.0, inva );
      freematrix( vw );
    }
    else {
      double** wut = getmatrix( m, n, 0.0 );
      for ( size_t in = 1; in <= n; in++ ) {
        double alpha = ( w[in] <= TOL ? 0.0 : 1.0 / w[in] );
        daxpy( n, alpha, &u[1][in], n, &wut[in][1], 1 );
      }
      dgemm(false, false, m, n, m, 1.0, v, wut, 0.0, inva );
      freematrix( wut );
    }
  }

  // de-allocate memory
  freematrix( b );
  freematrix( u );
  freevector( w );
  freematrix( v );

  // return value
  return retval;
} // mpinverse

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
  dcopy( n, &b[1], 1, &c[1], 1 );
  dgemv( false, n, n, 1.0, a, c, 0.0, b );
  freevector( c );
  return retval;
} // solve4x4

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

static int ldlsolve( const size_t n, double** a, double* b )
{
  int retval = 0;
  retval = Choleski_LDU_Decomposition( &a[1][1], n );
  if ( retval == 0 ) {
    retval = Choleski_LDU_Solve( &a[1][1], &b[1], &b[1], n );
  }
  return retval;
} // ldlsolve

int solve( const size_t n, double** a, double* b )
{
  int retval = 1;
  double* save = getvector( n, 0.0 );
  dcopy( n, &b[1], 1, &save[1], 1 );
  if ( n == 1 ) retval = solve1x1( n, a, b );
  if ( n == 2 ) retval = solve2x2( n, a, b );
  if ( n == 3 ) retval = solve3x3( n, a, b );
  if ( n == 4 ) retval = solve4x4( n, a, b );
  if ( 0 != retval ) {
    if ( n <= 5 ) dcopy( n, &save[1], 1, &b[1], 1 );
    retval = llsolve( n, a, b );
    if ( 0 != retval ) {
      dcopy( n, &save[1], 1, &b[1], 1 );
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
 
const size_t MAX_ITERATION_COUNT = 64;

static void Interchange_Rows( double* A, int row1, int row2, int ncols )
{
  double *pA1, *pA2;
  pA1 = A + row1 * ncols;
  pA2 = A + row2 * ncols;
  for ( int i = 0; i < ncols; i++ ) {
    const double temp = *pA1;
    *pA1++ = *pA2;
    *pA2++ = temp;
  }
} // Interchange_Rows

static void Interchange_Columns( double* A, int col1, int col2, int nrows, int ncols )
{
  double *pA1, *pA2;
  pA1 = A + col1;
  pA2 = A + col2;
  for ( int i = 0; i < nrows; pA1 += ncols, pA2 += ncols, i++ ) {
    const double temp = *pA1;
    *pA1 = *pA2;
    *pA2 = temp;
  }
} // Interchange_Columns

static int Search_Rows( double* A, int* p, int n )
{
  double* pA;
  int j;
  int bottom_center_block = n - 1;
  pA = A + bottom_center_block * n;
  for ( int i = bottom_center_block; i >= 0; i--, pA -= n ) {
    for ( j = 0; j < i; j++ ) if ( *( pA + j ) != 0.0 ) break;
    if ( j < i ) continue;
    for ( j = i + 1; j < n; j++ ) if ( *( pA + j ) != 0.0 ) break;
    if ( j < n ) continue;
    Interchange_Rows( A, i, bottom_center_block, n );
    Interchange_Columns( A, i, bottom_center_block, n, n );
    p[bottom_center_block] = i;
    bottom_center_block--;
  }
  return bottom_center_block;
} // Search_Rows

static int Search_Cols( double* A, int* p, int bottom_center_block, int n )
{
  double* pA;
  int j;
  int top_center_block = 0;
  pA = A + 1;
  for ( int i = 0; i < bottom_center_block; pA = A + ++i ) {
    for ( j = 0; j < i; j++, pA += n ) if ( *pA != 0.0 ) break;
    if ( j < i ) continue;
    pA += n;
    for ( j = i + 1; j < n; j++, pA += n ) if ( *pA != 0.0 ) break;
    if ( j < n ) continue;
    if ( i > top_center_block ) {
      Interchange_Rows( A, i, top_center_block, n );
      Interchange_Columns( A, i, top_center_block, n, n );
      p[top_center_block] = i;
      i--;
    }
    top_center_block++;
  }
  return top_center_block;
} // Search_Cols

static double Power_of_2( double x )
{
  long k = ( long )( log( 0.5 * x ) / log( 4.0 ) );
  double y = pow( 2.0, k );
  while ( x <= ( y * y * 0.5) ) y /= 2.0;
  while ( x > (2.0 * y * y) ) y *= 2.0;
  return y;
} // Power_of_2

static void Balance_Matrix( double* A, int n )
{
  int i, j;
  int top_center_block, bottom_center_block;
  double* pA, * pC, * pTop;
  double col_norm, row_norm;
  double x, y, z;
  int* p = ( int* ) malloc( ( n + 2 ) * sizeof( int ) );
  for ( i = 0; i < n; i++ ) p[i] = i;
  double* d = ( double* ) malloc( n * sizeof( double ) );
  for ( i = 0; i < n; i++ ) d[i] = 1.0;
  bottom_center_block = Search_Rows( A, p, n );
  p[n + 1] = bottom_center_block;
  top_center_block = Search_Cols( A, p, bottom_center_block, n );
  p[n] = top_center_block;
  if ( top_center_block >= bottom_center_block ) return;
  bool converged = false;
  while ( !converged ) {
    converged = true;
    pTop = A + top_center_block * n;
    pA = pTop;
    for ( i = top_center_block; i <= bottom_center_block; i++, pA += n ) {
      col_norm = 0.0;
      row_norm = 0.0;
      pC = pTop + i;
      for ( j = top_center_block; j <= bottom_center_block; j++, pC += n ) {
        if ( j == i ) continue;
        col_norm += fabs( *pC );
        row_norm += fabs( *( pA + j ) );
      }
      x = row_norm / col_norm;
      y = Power_of_2( x );
      z = 1.0 / y;
      if ( ( col_norm * y + row_norm * z ) < 0.95 * ( col_norm + row_norm ) ) {
        d[i] *= y;
        pC = A + i;
        for ( j = 0; j <= bottom_center_block; j++, pC += n ) *pC *= y;
        for ( j = top_center_block; j < n; j++ ) *( pA + j ) *= z;
        converged = false;
      }
    }
  }
  free( p );
  free( d );
} // Balance_Matrix

static void Identity_Matrix( double* A, int n )
{
  for ( int i = 0; i < n - 1; i++ ) {
    *A++ = 1.0;
    for ( int j = 0; j < n; j++ ) *A++ = 0.0;
  }
  *A = 1.0;
} // Identity_Matrix

static void Hessenberg_Elementary_Transform( double* H, double* S, int* perm, int n )
{
  int i, j;
  double* pS, * pH;
  Identity_Matrix( S, n );
  for ( i = n - 2; i >= 1; i-- ) {
    pH = H + n * ( i + 1 );
    pS = S + n * ( i + 1 );
    for ( j = i + 1; j < n; pH += n, pS += n, j++ ) {
      *( pS + i ) = *( pH + i - 1 );
      *( pH + i - 1 ) = 0.0;
    }
    if ( perm[i] != i ) {
      pS = S + n * i;
      pH = S + n * perm[i];
      for ( j = i; j < n; j++ ) {
        *( pS + j ) = *( pH + j );
        *( pH + j ) = 0.0;
      }
      *( pH + i ) = 1.0;
    }
  }
} // Identity_Matrix

static int Hessenberg_Form_Elementary( double* A, double* S, int n )
{
  int i, j, col, row;
  int* perm;
  double* p_row, * pS_row;
  double max;
  double s;
  double* pA, * pB, * pC, * pS;
  if ( n <= 1 ) {
    *S = 1.0; return 0;
  }
  if ( n == 2 ) {
    *S++ = 1.0; *S++ = 0.0; *S++ = 1.0; *S = 0.0; return 0;
  }
  perm = ( int* ) malloc( n * sizeof( int ) );
  if ( perm == NULL ) return -1;
  p_row = A + n;
  pS_row = S + n;
  for ( col = 0; col < ( n - 2 ); p_row += n, pS_row += n, col++ ) {
    row = col + 1;
    perm[row] = row;
    for ( pA = p_row + col, max = 0.0, i = row; i < n; pA += n, i++ )
      if ( fabs( *pA ) > max ) {
        perm[row] = i; max = fabs( *pA );
      }
    if ( perm[row] != row ) {
      Interchange_Rows( A, row, perm[row], n );
      Interchange_Columns( A, row, perm[row], n, n );
    }
    pA = p_row + n;
    pS = pS_row + n;
    for ( i = col + 2; i < n; pA += n, pS += n, i++ ) {
      s = *( pA + col ) / *( p_row + col );
      for ( j = 0; j < n; j++ )
        *( pA + j ) -= *( p_row + j ) * s;
      *( pS + col ) = s;
      for ( j = 0, pB = A + col + 1, pC = A + i; j < n; pB += n, pC += n, j++ )
        *pB += s * *pC;
    }
  }
  pA = A + n + n;
  pS = S + n + n;
  for ( i = 2; i < n; pA += n, pS += n, i++ ) dcopy( i - 1, pA, 1, pS, 1 );
  Hessenberg_Elementary_Transform( A, S, perm, n );
  free( perm );
  return 0;
} // Hessenberg_Form_Elementary

static void One_Real_Eigenvalue( double* Hrow, double* eigen_real, double* eigen_imag, int row, double shift )
{
  Hrow[row] += shift;
  eigen_real[row] = Hrow[row];
  eigen_imag[row] = 0.0;
} // One_Real_Eigenvalue

static void Update_Row( double* Hrow, double cos, double sin, int n, int row )
{
  double x;
  double* Hnextrow = Hrow + n;
  int i;
  for ( i = row; i < n; i++ ) {
    x = Hrow[i];
    Hrow[i] = cos * x + sin * Hnextrow[i];
    Hnextrow[i] = cos * Hnextrow[i] - sin * x;
  }
} // Update_Row

static void Update_Column( double* H, double cos, double sin, int n, int col )
{
  double x;
  int i;
  int next_col = col + 1;
  for ( i = 0; i <= next_col; i++, H += n ) {
    x = H[col];
    H[col] = cos * x + sin * H[next_col];
    H[next_col] = cos * H[next_col] - sin * x;
  }
} // Update_Column

static void Update_Transformation( double* S, double cos, double sin, int n, int k )
{
  double x;
  int i;
  int k1 = k + 1;
  for ( i = 0; i < n; i++, S += n ) {
    x = S[k];
    S[k] = cos * x + sin * S[k1];
    S[k1] = cos * S[k1] - sin * x;
  }
} // Update_Transformation

static void Two_Eigenvalues( double* H, double* S, double* eigen_real, double* eigen_imag, int n, int row, double shift )
{
  double p, q, x, discriminant, r;
  double cos, sin;
  double* Hrow = H + n * row;
  double* Hnextrow = Hrow + n;
  int nextrow = row + 1;
  p = 0.5 * ( Hrow[row] - Hnextrow[nextrow] );
  x = Hrow[nextrow] * Hnextrow[row];
  discriminant = p * p + x;
  Hrow[row] += shift;
  Hnextrow[nextrow] += shift;
  if ( discriminant > 0.0 ) {
    q = sqrt( discriminant );
    if ( p < 0.0 ) q = p - q; else q += p;
    eigen_real[row] = Hnextrow[nextrow] + q;
    eigen_real[nextrow] = Hnextrow[nextrow] - x / q;
    eigen_imag[row] = 0.0;
    eigen_imag[nextrow] = 0.0;
    r = sqrt( Hnextrow[row] * Hnextrow[row] + q * q );
    sin = Hnextrow[row] / r;
    cos = q / r;
    Update_Row( Hrow, cos, sin, n, row );
    Update_Column( H, cos, sin, n, row );
    Update_Transformation( S, cos, sin, n, row );
  }
  else {
    eigen_real[nextrow] = eigen_real[row] = Hnextrow[nextrow] + p;
    eigen_imag[row] = sqrt( fabs( discriminant ) );
    eigen_imag[nextrow] = -eigen_imag[row];
  }
} // Two_Eigenvalues

static void Product_and_Sum_of_Shifts( double* H, int n, int max_row, double* shift, double* trace, double* det, int iteration )
{
  double* pH = H + max_row * n;
  double* p_aux;
  int i;
  int min_col = max_row - 1;
  if ( ( iteration % 10 ) == 0 ) {
    *shift += pH[max_row];
    for ( i = 0, p_aux = H; i <= max_row; p_aux += n, i++ ) p_aux[i] -= pH[max_row];
    p_aux = pH - n;
    *trace = fabs( pH[min_col] ) + fabs( p_aux[min_col - 1] );
    *det = *trace * *trace;
    *trace *= 1.5;
  }
  else {
    p_aux = pH - n;
    *trace = p_aux[min_col] + pH[max_row];
    *det = p_aux[min_col] * pH[max_row] - p_aux[max_row] * pH[min_col];
  }
} // Product_and_Sum_of_Shifts

static int Two_Consecutive_Small_Subdiagonal( double* H, int min_row, int max_row, int n, double trace, double det )
{
  double x, y, z, s;
  double* pH;
  int i, k;
  for ( k = max_row - 2, pH = H + k * n; k >= min_row; pH -= n, k-- ) {
    x = ( pH[k] * ( pH[k] - trace ) + det ) / pH[n + k] + pH[k + 1];
    y = pH[k] + pH[n + k + 1] - trace;
    z = pH[n + n + k + 1];
    s = fabs( x ) + fabs( y ) + fabs( z );
    x /= s;
    y /= s;
    z /= s;
    if ( k == min_row ) break;
    if ( ( fabs( pH[k - 1] ) * ( fabs( y ) + fabs( z ) ) ) <= DBL_EPSILON * fabs( x ) * ( fabs( pH[k - 1 - n] ) + fabs( pH[k] ) + fabs( pH[n + k + 1] ) ) ) break;
  }
  for ( i = k + 2, pH = H + i * n; i <= max_row; pH += n, i++ ) pH[i - 2] = 0.0;
  for ( i = k + 3, pH = H + i * n; i <= max_row; pH += n, i++ ) pH[i - 3] = 0.0;
  return k;
} // Two_Consecutive_Small_Subdiagonal

static void Double_QR_Step( double* H, int min_row, int max_row, int min_col, double trace, double det, double* S, int n )
{
  double s, x = 0.0, y, z;
  double a, b, c;
  double* pH;
  double* tH;
  double* pS;
  int i, j, k;
  int last_test_row_col = max_row - 1;
  k = min_col;
  pH = H + min_col * n;
  a = ( pH[k] * ( pH[k] - trace ) + det ) / pH[n + k] + pH[k + 1];
  b = pH[k] + pH[n + k + 1] - trace;
  c = pH[n + n + k + 1];
  s = fabs( a ) + fabs( b ) + fabs( c );
  a /= s;
  b /= s;
  c /= s;
  for ( ; k <= last_test_row_col; k++, pH += n ) {
    if ( k > min_col ) {
      c = ( k == last_test_row_col ) ? 0.0 : pH[n + n + k - 1];
      x = fabs( pH[k - 1] ) + fabs( pH[n + k - 1] ) + fabs( c );
      if ( x == 0.0 ) continue;
      a = pH[k - 1] / x;
      b = pH[n + k - 1] / x;
      c /= x;
    }
    s = sqrt( a * a + b * b + c * c );
    if ( a < 0.0 ) s = -s;
    if ( k > min_col ) pH[k - 1] = -s * x;
    else if ( min_row != min_col ) pH[k - 1] = -pH[k - 1];
    a += s;
    x = a / s;
    y = b / s;
    z = c / s;
    b /= a;
    c /= a;
    for ( j = k; j < n; j++ ) {
      a = pH[j] + b * pH[n + j];
      if ( k != last_test_row_col ) {
        a += c * pH[n + n + j];
        pH[n + n + j] -= a * z;
      }
      pH[n + j] -= a * y;
      pH[j] -= a * x;
    }
    j = k + 3;
    if ( j > max_row ) j = max_row;
    for ( i = 0, tH = H; i <= j; i++, tH += n ) {
      a = x * tH[k] + y * tH[k + 1];
      if ( k != last_test_row_col ) {
        a += z * tH[k + 2];
        tH[k + 2] -= a * c;
      }
      tH[k + 1] -= a * b;
      tH[k] -= a;
    }
    for ( i = 0, pS = S; i < n; pS += n, i++ ) {
      a = x * pS[k] + y * pS[k + 1];
      if ( k != last_test_row_col ) {
        a += z * pS[k + 2];
        pS[k + 2] -= a * c;
      }
      pS[k + 1] -= a * b;
      pS[k] -= a;
    }
  }
} // Double_QR_Step

static void Double_QR_Iteration( double* H, double* S, int min_row, int max_row, int n, double* shift, int iteration )
{
  int k;
  double trace, det;
  Product_and_Sum_of_Shifts( H, n, max_row, shift, &trace, &det, iteration );
  k = Two_Consecutive_Small_Subdiagonal( H, min_row, max_row, n, trace, det );
  Double_QR_Step( H, min_row, max_row, k, trace, det, S, n );
} // Double_QR_Iteration

static int QR_Hessenberg_Matrix_Eigenvalues( double* H, double* S, double* eigen_real, double* eigen_imag, int n, int max_iteration_count )
{
  int i;
  int row;
  int iteration;
  int found_eigenvalue;
  double shift = 0.0;
  double* pH;
  for ( row = n - 1; row >= 0; row-- ) {
    found_eigenvalue = 0;
    for ( iteration = 1; iteration <= max_iteration_count; iteration++ ) {
      for ( i = row, pH = H + row * n; i > 0; i--, pH -= n )
        if ( fabs( *( pH + i - 1 ) ) <= DBL_EPSILON * ( fabs( *( pH - n + i - 1 ) ) + fabs( *( pH + i ) ) ) ) break;
      switch ( row - i ) {
        case 0:
          One_Real_Eigenvalue( pH, eigen_real, eigen_imag, i, shift );
          found_eigenvalue = 1;
          break;
        case 1:
          row--;
          Two_Eigenvalues( H, S, eigen_real, eigen_imag, n, row, shift );
          found_eigenvalue = 1;
          break;
        default:
          Double_QR_Iteration( H, S, i, row, n, &shift, iteration );
      }
      if ( found_eigenvalue ) break;
    }
    if ( iteration > max_iteration_count ) return -1;
  }
  return 0;
} // QR_Hessenberg_Matrix_Eigenvalues

int max_eigen_hessenberg( const size_t n, double** const a, double* mx )
{
  int retval = 0;
  double maxe = 0.0;
  double** aa = getmatrix( n, n, 0.0 );
  dcopy( n * n, &a[1][1], 1, &aa[1][1], 1 );
  Balance_Matrix( &aa[1][1], ( int )( n ) );
  double** s = getmatrix( n, n, 0.0 );
  retval = Hessenberg_Form_Elementary( &aa[1][1], &s[1][1], ( int )( n ) );
  if ( retval == 0 ) {
    double* wr = getvector( n, 0.0 );
    double* wi = getvector( n, 0.0 );
    retval = QR_Hessenberg_Matrix_Eigenvalues( &aa[1][1], &s[1][1], &wr[1], &wi[1], ( int )( n ), MAX_ITERATION_COUNT );
    if ( retval == 0 ) {
      maxe = -DBL_MAX;
      for ( size_t i = 1; i <= n; i++ ) {
        if ( iszero( wi[i] ) ) {
          if ( wr[i] > maxe ) maxe = wr[i];
        }
      }
    }
    freevector( wr );
    freevector( wi );
  }
  freematrix( s );
  freematrix( aa );
  ( *mx ) = maxe;
  return retval;
} // Max_Eigen_Value

static void balance( const size_t n, double** a )
{
  const double RADIX = 2.0;
  const double sqrdx = RADIX * RADIX; 
  bool restart = true; 
  while ( restart == true ) {
    restart = false; 
    for ( size_t i = 1; i <= n; i++ ) {
      double r = 0.0; 
      double c = 0.0; 
      for ( size_t j = 1; j <= n; j++ ) if ( j != i ) {
        c += fabs ( a[j][i] ); 
        r += fabs ( a[i][j] ); 
      }
      if ( c && r ) {                  // replace with iszero()'s
        double g = r / RADIX; 
        double f = 1.0; 
        const double s = c + r; 
        while ( c < g ) {
          f *= RADIX; 
          c *= sqrdx; 
        }
        g = r * RADIX; 
        while ( c > g ) {
          f /= RADIX; 
          c /= sqrdx; 
        }
        if ( ( c + r ) / f < 0.95 * s ) {
          restart = true; 
          g = 1.0 / f; 
          for ( size_t j = 1; j <= n; j++ ) a[i][j] *= g; 
          for ( size_t j = 1; j <= n; j++ ) a[j][i] *= f; 
        }
      }
    }
  }
} // balance

static void elmhes( const size_t n, double** a )
{
  for ( size_t m = 2; m < n; m++ ) {
    double x = 0.0; 
    size_t i = m; 
    for ( size_t j = m; j <= n; j++ ) if ( fabs( a[j][m-1] ) > fabs( x ) ) {
      x = a[j][m-1]; 
      i = j; 
    }
    if ( i != m ) {
      for ( size_t j = m - 1; j <= n; j++ ) {
        const double tmp = a[i][j];
        a[i][j] = a[m][j];
        a[m][j] = tmp;
      }
      for ( size_t j = 1; j <= n; j++ ) {
        const double tmp = a[j][i];
        a[j][i] = a[j][m];
        a[j][m] = tmp;
      }
    }
    if ( x ) {                                        // replace with isnotzero()
      double y = 0.0;
      for ( size_t i = m + 1; i <= n; i++ ) {
        if ( ( y = a[i][m - 1] ) ) {
          y /= x; 
          a[i][m - 1]= y; 
          for ( size_t j = m; j <= n; j++ ) a[i][j] -= y*a[m][j]; 
          for ( size_t j = 1; j <= n; j++ ) a[j][m] += y*a[j][i]; 
        }
      }
    }
  }
} // elmhes

static int hqr( const size_t n, double** const a, double* mx )
{
  int retval = 0;
  long nn, m, l, k, j, i, mmin;
  long its = 0; 
  nn = ( long )( n ); 

  double maxe = 0.0;

  double* wr = getvector( n, 0.0 );
  double* wi = getvector( n, 0.0 );
 
  double p = 0.0; 
  double q = 0.0; 
  double r = 0.0; 
  double s = 0.0; 
  double t = 0.0; 
  double u = 0.0; 
  double v = 0.0; 
  double w = 0.0; 
  double x = 0.0; 
  double y = 0.0; 
  double z = 0.0; 
  double anorm = 0.0; 

  for ( i = 1; i <= n; i++ ) for ( j = max_t( i - 1, 1 ); j <= n; j++ ) anorm += fabs( a[i][j] ); 
  while ( nn >= 1 ) {
    if ( its == 30 ) break;
    else its = 0; 
    do {
      for ( l = nn; l >= 2; l-- ) {
        s = fabs( a[l-1][l-1] ) + fabs( a[l][l] ); 
        if ( s == 0.0 ) s = anorm;                                     // iszero()
        if ( ( double )( fabs( a[l][l-1] ) + s ) == s ) {              // iszero()
          a[l][l-1]= 0.0; 
          break; 
        }
      }
      x = a[nn][nn]; 
      if ( l == nn ) {
        wr[nn] = x + t; 
        wi[nn--] = 0.0; 
      } else {
        y = a[nn-1][nn-1]; 
        w = a[nn][nn-1] * a[nn-1][nn]; 
        if ( l == ( nn - 1 ) ) {
          p = 0.5 * ( y - x ); 
          q = p * p + w; 
          z = sqrt( fabs( q ) ); 
          x += t; 
          if ( q >= 0.0 ) {
            z = p + sign( z, p );           // correct way ???
            wr[nn-1] = wr[nn] = x + z; 
            if ( z ) wr[nn] = x - w / z;       // iszero()
            wi[nn-1] = wi[nn] = 0.0; 
          } else {
            wr[nn-1] = wr[nn] = x + p; 
            wi[nn-1] = -( wi[nn] = z ); 
          }
          nn -= 2; 
        } else {
          if ( its == 30 ) break; 
          if ( its == 10 || its == 20 ) {
            t += x; 
            for ( i = 1; i <= nn; i ++ ) a[i][i] -= x; 
            s = fabs( a[nn][nn-1] ) + fabs( a[nn-1][nn-2] ); 
            y = x = 0.75 * s; 
            w = -0.4375 * s * s; 
          }
          ++its;
          for ( m = ( nn - 2 ); m >= l; m-- ) {
            z = a[m][m]; 
            r = x - z; 
            s = y - z; 
            p = ( r * s - w ) / a[m+1][m] + a[m][m+1]; 
            q = a[m+1][m+1] - z - r - s; 
            r = a[m+2][m+1]; 
            s = fabs( p ) + fabs( q ) + fabs( r ); 
            p /= s; 
            q /= s; 
            r /= s; 
            if ( m == l ) break; 
            u = fabs( a[m][m-1] ) * ( fabs( q ) + fabs( r ) ); 
            v = fabs( p ) * ( fabs( a[m-1][m-1] ) + fabs( z ) + fabs( a[m+1][m+1] ) ); 
            if ( ( double )( u + v ) == v ) break;                                         // iszero()
          }
          for ( i = m + 2; i <= nn; i ++ ) a[i][i-2] = 0.0; 
          for ( i = m + 3; i <= nn; i ++ ) a[i][i-3] = 0.0; 
          for ( k = m; k <= nn - 1; k ++ ) {
            if ( k != m ) {
              p = a[k][k-1]; 
              q = a[k+1][k-1]; 
              r = ( k == ( nn - 1 ) ? 0.0 : a[k+2][k-1] ); 
              if ( ( x = fabs ( p )+ fabs ( q )+ fabs ( r ) ) != 0.0 ) {    // iszero()
                p /= x; 
                q /= x; 
                r /= x; 
              }
            }
            if ( ( s = sign( sqrt( p * p + q * q + r * r ), p ) ) != 0.0 ) {   // iszero()
              if ( k == m ) {
                if ( l != m ) a[k][k-1] = -a[k][k-1]; 
              } else a[k][k-1] = -s * x; 
              p += s; 
              x = p / s; 
              y = q / s; 
              z = r / s; 
              q /= p; 
              r /= p; 
              for ( j = k; j <= nn; j ++ ) {
                p = a[k][j] + q * a[k+1][j]; 
                if ( k != ( nn - 1 ) ) {
                  p += r * a[k+2][j]; 
                  a[k+2][j] -= p * z; 
                }
                a[k+1][j] -= p * y; 
                a[k][j] -= p * x; 
              }
              mmin = ( nn < k + 3 ? nn : k + 3 ); 
              for ( i = l; i <= mmin; i ++ ) {
                p = x * a[i][k] + y * a[i][k+1]; 
                if ( k != ( nn - 1 ) ) {
                  p += z * a[i][k+2]; 
                  a[i][k+2] -= p * r; 
                }
                a[i][k+1] -= p * q; 
                a[i][k] -= p; 
              }
            }
          }
        }
      }
    } while ( l < nn - 1 ); 
  }
  if ( its < 30 ) {
    maxe = -DBL_MAX;
    for ( size_t i = 1; i <= n; i++ ) {
      if ( iszero( wi[i] ) ) {
        if ( wr[i] > maxe ) maxe = wr[i];
      }
    }
  }
  freevector( wr );
  freevector( wi );
  ( *mx ) = maxe;
  return retval;
} // hqr

int max_eigen_186( const size_t n, double** const a, double* mx )
{
  balance( n, a );
  elmhes( n, a );
  double maxe = 0.0;
  int retval = hqr( n, a, &maxe );
  ( *mx ) = maxe;
  return( retval );
} // max_eigen_186

static int arnoldi_matrix( const size_t n, double** a, size_t m, double** h )
// return Krylov subspace h for truncated eigenvalue decomposition of asymmetric matrix a
// in some cases, m can be reduced, anyway, h is upper-Hessenberg format
{
  // fast return
  if ( n == 0 ) return 1;

  // constants
  const size_t MAXGS = 4;
  const double TOL = 1.8189894035458617e-12;
  const double ZCRIT = 0.5;

  // memory allocation
  double** q = getmatrix( m, n, 0.0 );
  double* z = getvector( n, 0.0 );

  // initialize variables: v is a vector of unit norm
  double norm = sqrt( 6.0 / ( double )( n * ( n + 1 ) * ( 2 * n + 1 ) ) );
  for ( size_t i = 1; i <= n; i++ ) q[1][i] = norm * ( double )( i );

  size_t j = 0;
  double beta = 0.0;
  for ( j = 1; j <= m; j++ ) {
    for ( size_t i = 1; i <= n; i++ ) z[i] = ddot( n, &a[i][1], 1, &q[j][1], 1 );

    // re-orthogonalization (modified Gram-Schmidt)
    double nnew = sqrt( dssq( n, &z[1], 1 ) );
    for ( size_t iter = 1; iter <= MAXGS; iter++ ) {
      double nold = nnew;
      for ( size_t i = 1; i <= j; i++ ) {
        double alpha = ddot( n, &z[1], 1, &q[i][1], 1 );
        h[i][j] += alpha;
        daxpy( n, -1.0 * alpha, &q[i][1], 1, &z[1], 1 );
      }
      if ( iter == MAXGS ) dset( n, 0.0, &z[1], 1 );
      nnew = sqrt( dssq( n, &z[1], 1 ) );
      if ( nnew >= ZCRIT * nold ) break;  // normal return
    }
    if ( j == m ) break;  // normal return

    beta = nnew;
    h[j + 1][j] = beta;
    if ( beta < TOL ) break;
    daxpy( n, 1.0 / beta, &z[1], 1, &q[j + 1][1], 1 );
  }

  // de-allocate memory
  freematrix( q );
  freevector( z );

  return ( beta < DBL_EPSILON ? 1 : 0 );
} // arnoldi_matrix

int max_eigen_arnoldi( const size_t n, double** const a, const size_t nvecs, double* mx )
// reduces input matrix using Arnoldi
// and finds maximum eigenvalue with QR algorithm
{
  // initialize
  int retval = 0;
  double maxa = 0.0;

  // arnoldi reduction
  size_t snt = n;
  double dn = ( double )( snt );
  double work = 50.0 + 2.0 * sqrt( dn );
  size_t mdef = min_t( n, ( size_t )( work ) );
  size_t m = ( nvecs == 0 || nvecs > n ? mdef : max_t( 1, min_t( n, nvecs ) ) );
  double** hm = getmatrix( m, m, 0.0 );
  retval = arnoldi_matrix( n, a, m, hm );

  // maximum eigenvalue using QR
  if ( retval == 0 ) {
    double** hk = getmatrix( m, m, 0.0 );
    for ( size_t i = 1; i <= m; i++ ) for ( size_t j = 1; j <= m; j++ ) hk[i][j] = hm[i][j];
    double* wr = getvector( m, 0.0 );
    double* wi = getvector( m, 0.0 );

    dset( m * m, 0.0, &hm[1][1], 1 );
    dset( m, 1.0, &hm[1][1], m + 1 );
    retval = QR_Hessenberg_Matrix_Eigenvalues( &hk[1][1], &hm[1][1], &wr[1], &wi[1], ( int )( m ), ( int )( MAX_ITERATION_COUNT ) );

    freematrix( hk );

    // get maximum value
    if ( retval == 0 ) {
      maxa = -DBL_MAX;
      for ( size_t i = 1; i <= m; i++ ) {
        if ( iszero( wi[i] ) ) {
          if ( wr[i] > maxa ) maxa = wr[i];
        }
      }
    }
    freevector( wr );
    freevector( wi );
  }
  freematrix( hm );

  *mx = maxa;

  // return value
  return retval;
} // maxarnoldi

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

static int fastnnls( const size_t n, const size_t m, double** x, double** xtx, double* b, double* y )
// fast nonnegative least squares procedure based on Bro and de Jong (1997)
{
  const bool slowversion = xtx == NULL;
  if ( slowversion == true ) {
    xtx = getmatrix( m, m, 0.0 );
    dgemm( true, false, m, m, n, 1.0, x, x, 0.0, xtx );
  }

  // allocate memory
  double* xty = getvector( m, 0.0 );
  double* r = getvector( m, 0.0 );         // Lagrange multipliers
  bool* pnew = getbvector( m, false );     // all in passive set
  double* s = getvector( m, 0.0 );         // beta help

  // setup x'y
  for ( size_t i = 1; i <= m; i++ ) {
    b[i] = 0.0;                            // initialize b to zero (needed for updating)
    double work = 0.0;
    for ( size_t k = 1; k <= n; k++ ) {
      work += x[k][i] * y[k];
    }
    r[i] = xty[i] = work;                  // initial Lagrange multipliers (for b = 0)
  }

  // set tolerance criterion
  double sm = 0.0;
  for ( size_t j = 1; j <= m; j++ ) {
    const double work = asum( m, &xtx[1][j], m );
    if ( work > sm ) sm = work;
  }
  const double TOL = 10.0 * ( double )( m ) * sm * DBL_EPSILON;
  const size_t MAXITER = 30 * m;

  // outer loop
  size_t iter = 0;
  while ( anyfalsegt( m, r, pnew, TOL ) ) {
    size_t index = 0;
    double val = -DBL_MAX;
    for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == false && r[i] > val ) {
      val = r[i];
      index = i;
    }
    pnew[index] = true;             // move to active set
    lstsq( m, xtx, xty, pnew, s );  // least squares solution active set

    // inner loop
    while ( anytruele( m, s, pnew, TOL ) && iter < MAXITER ) {
      iter++;
      double alpha = DBL_MAX;
      for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == true && s[i] <= TOL ) {
        const double work = b[i] / ( b[i] - s[i] );
        if ( work < alpha ) alpha = work;
      }
      for ( size_t i = 1; i <= m; i++ ) b[i] += alpha * ( s[i] - b[i] );
      for ( size_t i = 1; i <= m; i++ ) if ( pnew[i] == true && b[i] <= TOL ) pnew[i] = false;
      lstsq( m, xtx, xty, pnew, s );
    }
    for ( size_t i = 1; i <= m; i++ ) b[i] = s[i];
    for ( size_t i = 1; i <= m; i++ ) r[i] = xty[i] - ddot( m, &xtx[i][1], 1, &b[1], 1 );
  }

  // de-allocate memory
  if ( slowversion == true ) freematrix( xtx );
  freevector( xty );
  freebvector( pnew );
  freevector( r );
  freevector( s );

  return 0;
} // fastnnls

static int fastnnwls( const size_t n, const size_t m, double** x, double** xtwx, double* b, double* y, double* w )
// fast nonnegative weighted least squares procedure based on Bro and de Jong (1997)
{
  const bool slowversion = xtwx == NULL;
  if ( slowversion == true ) {
    xtwx = getmatrix( m, m, 0.0 );
    for ( size_t i = 1; i <= m; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) {
        double tmp = 0.0;
        for ( size_t k = 1; k <= n; k++ ) tmp += x[k][i] * w[k] * x[k][j];
        xtwx[i][j] = tmp;
      }
    }
  }

  // allocate memory
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
  }

  // set convergence criterion
  double sm = 0.0;
  for ( size_t j = 1; j <= m; j++ ) {
    double work = asum( m, &xtwx[1][j], m );
    if ( work > sm ) sm = work;
  }
  const double TOL = 10.0 * ( double )( m ) * sm * DBL_EPSILON;
  const size_t MAXITER = 30 * m;

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
    for ( size_t i = 1; i <= m; i++ ) r[i] = xtwy[i] - ddot( m, &xtwx[i][1], 1, &b[1], 1 );
  }

  // de-allocate memory
  if ( slowversion == true ) freematrix( xtwx );
  freevector( xtwy );
  freebvector( pnew );
  freevector( r );
  freevector( s );

  return 0;
} // fastnnwls

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// transformation functions
//

void nnintercept( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r )
// function: y = a + x
// transform x such that min ||a+x-y|| for a+x >= 0.0 for all x
// return a + x in z
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
  if ( isnull( w ) ) {
    if ( symmetric == true ) {
      sumw = ( double )( n * ( n - 1 ) / 2 );
      for ( size_t i = 2; i <= n; i++ ) {
        for ( size_t j = 1; j < i; j++ ) {
          const double xi = x[i][j];
          const double yi = mconst * y[i][j];
          wsumx += xi;
          wsumy += yi;
        }
      }
    }
    else {
      sumw = ( double )( n * n - n );
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
          const double xi = x[i][j];
          const double yi = mconst * y[i][j];
          wsumx += xi;
          wsumy += yi;
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
            const double xi = x[i][j];
            const double yi = mconst * y[i][j];
            sumw += wi;
            wsumx += wi * xi;
            wsumy += wi * yi;
          }
        }
      }
    }
    else {
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
          const double wi = w[i][j];
          if ( isnotzero( wi ) ) {
            const double xi = x[i][j];
            const double yi = mconst * y[i][j];
            sumw += wi;
            wsumx += wi * xi;
            wsumy += wi * yi;
          }
        }
      }
    }
  }
  double a = ( wsumy - wsumx ) / sumw;
  if ( a < -xmin ) a = -xmin;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) r[i][j] = a + x[i][j];
} // nnintercept

void nnslope( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r )
// function: y = b * x
// transform x such that min ||b*x-y|| for b >= 0.0
{
  double wssqx = 0.0;
  double cross = 0.0;
  if ( isnull( w ) ) {
    if ( symmetric == true ) {
      for ( size_t i = 2; i <= n; i++ ) {
        for ( size_t j = 1; j < i; j++ ) {
          const double xi = x[i][j];
          const double yi = mconst * y[i][j];
          wssqx += xi * xi;
          cross += xi * yi;
        }
      }
    }
    else {
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
          const double xi = x[i][j];
          const double yi = mconst * y[i][j];
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
            const double xi = x[i][j];
            const double yi = mconst * y[i][j];
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
            const double xi = x[i][j];
            const double yi = mconst * y[i][j];
            wssqx += wi * xi * xi;
            cross += wi * xi * yi;
          }
        }
      }
    }
  }
  if ( iszero( wssqx ) ) wssqx = DBL_EPSILON;
  const double b = ( cross < 0.0 ? 0.0 : cross / wssqx );
  if ( isnotzero( b ) ) {
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) r[i][j] = b * x[i][j];
  }
} // nnslope

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

static double fpowerij( const double xij, const double yij, const double wij, const size_t na, double** alpha, double* beta, double* dyda, double* va, bool* ia )
{
  const double yfit = va[1] + va[2] * pow( xij, va[3] );
  dyda[1] = 1.0;
  dyda[2] = pow( xij, va[3] );
  dyda[3] = va[2] * pow( xij, va[3] ) * nnlog( xij );
  const double dy = yij - yfit;
  const double rstress = dy * dy * wij;
  for ( size_t j = 1, jj = 0; j <= na; j++ ) if ( ia[j] == true ) {
    jj++;
    double wt = dyda[j] * wij;
    for ( size_t k = 1, kk = 0; k <= j; k++ ) if ( ia[k] == true ) {
      kk++;
      alpha[jj][kk] += wt * dyda[k];
    }
    beta[jj] += dy * wt;
  }
  return rstress;
}

static double fpower( const bool symmetric, const size_t n, double** x, double** y, double** w, const double mconst, const size_t na, double* va, bool* ia, const size_t nfit, double** alpha, double* beta )
// nonnegative power transformation help function for levenberg-marquardt algorithm
{
  double* dyda = getvector( na, 0.0 );
  for ( size_t j = 1; j <= nfit; j++ ) {
    for ( size_t k = 1; k <= j; k++ ) alpha[j][k] = 0.0;
    beta[j] = 0.0;
  }
  double rstress = 0.0;
  if ( isnull( w ) ) {
    if ( symmetric == true ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) rstress += fpowerij( x[i][j], mconst * y[i][j], 1.0, na, alpha, beta, dyda, va, ia );
    else for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) rstress += fpowerij( x[i][j], mconst * y[i][j], 1.0, na, alpha, beta, dyda, va, ia );
  }
  else {
    if ( symmetric == true ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) rstress += fpowerij( x[i][j], mconst * y[i][j], w[i][j], na, alpha, beta, dyda, va, ia );
    else for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) rstress += fpowerij( x[i][j], mconst * y[i][j], w[i][j], na, alpha, beta, dyda, va, ia );
  }
  for ( size_t j = 2; j <= nfit; j++ ) {
    for ( size_t k = 1; k < j; k++ ) alpha[k][j] = alpha[j][k];
  }
  freevector( dyda );
  return rstress;
} // fpower

void nnpower( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, const bool use_a, const bool use_b, double** r )
// nonnegative power transformation: y-hat = a + b * x^c (a + b * x^c >= 0.0; b > 0.0; c > 0.0; c < bound)
{
  double xmin = DBL_MAX;
  double xmax = 0.0;
  if ( isnull( w ) ) {
    if ( symmetric == true ) { 
      for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) {
        if ( x[i][j] < xmin ) xmin = x[i][j]; 
        if ( x[i][j] > xmax ) xmax = x[i][j]; 
      }
    }
    else { 
      for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
        if ( x[i][j] < xmin ) xmin = x[i][j]; 
        if ( x[i][j] > xmax ) xmax = x[i][j]; 
      }
    }
  }
  else {
    if ( symmetric == true ) { 
      for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) if ( isnotzero( w[i][j] ) ) {
        if ( x[i][j] < xmin ) xmin = x[i][j]; 
        if ( x[i][j] > xmax ) xmax = x[i][j]; 
      }
    }
    else { 
      for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) if ( isnotzero( w[i][j] ) ) {
        if ( x[i][j] < xmin ) xmin = x[i][j]; 
        if ( x[i][j] > xmax ) xmax = x[i][j]; 
      }
    }
  }

  // set constants
  const size_t ZERO = 0;
  const size_t ONE = 1;
  const size_t MAXITER = 64;
  const size_t MAXINNER = 52;  // 2^52 = 4.503599627370496e+15

  // check constraints
  double a = 0.0;
  double b = 1.0;
  double c = 1.0;
  const double cbound = log( ( double )( INT_MAX ) ) / log( xmax );
  if ( c < DBL_EPSILON ) c = DBL_EPSILON;
  if ( c > cbound ) c = cbound;
  if ( use_b && b < DBL_EPSILON ) b = DBL_EPSILON;
  const double abound = -1.0 * b * pow( xmin, c );
  if ( use_a && a < abound ) a = abound;

  // determine sizes
  size_t na = 3;
  bool* ia = getbvector( na, false );
  ia[1] = use_a; 
  ia[2] = use_b; 
  ia[3] = true;
  double* va = getvector( na, 0.0 );
  double* atry = getvector( na, 0.0 );
  atry[1] = va[1] = ( use_a ? a : 0.0 );
  atry[2] = va[2] = ( use_b ? b : 1.0 ); 
  atry[3] = va[3] = c;
  size_t nfit = ONE + ( use_a ? ONE : ZERO ) + ( use_b ? ONE : ZERO );

  // allocate memory
  double** alpha = getmatrix( nfit, nfit, 0.0 );  // J'WJ
  double* beta = getvector( nfit, 0.0 );          // J'W(y-yhat)
  double** covar = getmatrix( nfit, nfit, 0.0 );  // J'WJ + lambda diag(J'WJ)
  double* da = getvector( nfit, 0.0 );            // h

  // initialize
  double DF = ( double )( n - na + 1 );
  double DROP = 1.0 / 3.0;
  double BOOST = 2.0;
  double LAMBDA = 0.1L;
  double fold = 0.0, fnew = 0.0, anew = DBL_MAX, rho = DBL_MAX;
  fold = fpower( symmetric, n, x, y, w, mconst, na, va, ia, nfit, alpha, beta );
  double mxbeta = damax( nfit, &beta[1], 1 );

  // complete Levenberg-Marquardt algorithm
  if ( mxbeta > DBL_EPSILON ) {
    for ( size_t iter = 1; iter <= MAXITER; iter++ ) {

      // one single step
      for ( size_t inner = 1; inner <= MAXINNER; inner++ ) {

        // solve: alpha ( 1 + lambda ) da = beta for da
        double scale = 0.0;
        for ( size_t j = 1; j <= nfit; j++ ) {
          for ( size_t k = 1; k <= nfit; k++ ) covar[j][k] = alpha[j][k];
          covar[j][j] = alpha[j][j] * ( 1.0 + LAMBDA );
          da[j] = beta[j];
        }
        solve( nfit, covar, da );
        for ( size_t j = 1, k = 0; j <= na; j++ ) if ( ia[j] == true ) atry[j] = va[j] + da[++k];
        for ( size_t j = 1; j <= nfit; j++ ) scale += da[j] * ( LAMBDA * alpha[j][j] * da[j] + beta[j] );

        // set constraints
        if ( atry[3] < DBL_EPSILON ) atry[3] = DBL_EPSILON;           // exponent not below zero
        if ( atry[3] > cbound ) atry[3] = cbound;                     // exponent not over 16
        if ( atry[2] < DBL_EPSILON ) atry[2] = DBL_EPSILON;           // scale not negative
        const double abound = -1.0 * atry[2] * pow( xmin, atry[3] );  // bound depends on scale and exponent
        if ( use_a && atry[1] < abound ) atry[1] = abound;            // data cannot become negative

        // reset da due to constraints
        for ( size_t j = 1, k = 0; j <= na; j++ ) if ( ia[j] == true ) da[++k] = atry[j] - va[j];
        
        // administration: update convergence statistics
        anew = sqrt( dssq( nfit, &da[1], 1 ) );// / nrm2( na, &va[1], 1 );
        if ( anew <= DBL_EPSILON ) break;
        fnew = fpower( symmetric, n, x, y, w, mconst, na, atry, ia, nfit, covar, da );
        rho = ( fold - fnew ) / scale;

        // administration: update damping and parameters
        if ( rho > DBL_EPSILON ) {
          LAMBDA = fmax( DROP * LAMBDA, DBL_EPSILON );
          BOOST = 2.0;
          DROP /= 3.0;
          fold = fnew;
          for ( size_t j = 1; j <= nfit; j++ ) {
            for ( size_t k = 1; k <= nfit; k++ ) alpha[j][k] = covar[j][k];
            beta[j] = da[j];
          }
          for ( size_t j = 1; j <= na; j++ ) va[j] = atry[j];
          mxbeta = damax( nfit, &beta[1], 1 );
          break;
        }
        else {
          LAMBDA = fmin( BOOST * LAMBDA, 1.0 / DBL_EPSILON );
          BOOST *= 2.0;
          DROP = 1.0 / 3.0;
          fnew = fold;       // why?
        }
      }

      // administration: check convergence
      if ( anew <= DBL_EPSILON ) break;
      if ( mxbeta <= DBL_EPSILON ) break;
      if ( fnew / DF < DBL_EPSILON ) break;
    }
  }

  // set transformed vector
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) r[i][j] = va[1] + va[2] * pow( x[i][j], va[3] );

  // de-allocate memory
  freebvector( ia );
  freevector( va );
  freevector( atry );
  freematrix( alpha );
  freevector( beta );
  freematrix( covar );
  freevector( da );

} // nnpower

static double fboxcoxij( const double xij, const double yij, const double wij, const double c, double *alpha, double *beta )
// nonnegative box-cox transformation help function for levenberg-marquardt algorithm
{
  double yfit = ( c < -DBL_EPSILON || c > DBL_EPSILON ? ( pow( xij + 1.0, c ) - 1.0 ) / c : log( xij + 1.0 ) );
  double dydc = ( c < -DBL_EPSILON || c > DBL_EPSILON ? ( c * pow( xij + 1.0, c ) * log( xij + 1.0 ) - pow( xij + 1.0, c ) + 1.0 ) / ( c * c ) : 1.0 / ( xij + 1.0 ) );
  double wi = wij;
  double dy = yij - yfit;
  double wt = dydc * wi;
  *alpha += wt * dydc;
  *beta += dy * wt;
  double rstress = dy * dy * wi;
  return rstress;
} // fboxcoxij

static double fboxcox( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, const double c, double *alpha, double *beta )
// nonnegative box-cox transformation help function for levenberg-marquardt algorithm
{
  *alpha = 0.0;
  *beta = 0.0;
  double rstress = 0.0;
  if ( isnull( w ) ) {
    if ( symmetric == true ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) rstress += fboxcoxij( x[i][j], mconst * y[i][j], 1.0, c, alpha, beta );
    else for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) rstress += fboxcoxij( x[i][j], mconst * y[i][j], 1.0, c, alpha, beta );
  }
  else {
    if ( symmetric == true ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) rstress += fboxcoxij( x[i][j], mconst * y[i][j], w[i][j], c, alpha, beta );
    else for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) rstress += fboxcoxij( x[i][j], mconst * y[i][j], w[i][j], c, alpha, beta );
  }
  return rstress;
} // fboxcox

void nnboxcox( const bool symmetric, const size_t n, double** x, double** y, const double mconst, double** w, double** r )
// nonnegative box-cox transformation: y-hat = ( (x+1)^c - 1 ) / c
{
  // set constants
  const size_t MAXITER = 64;
  const size_t MAXINNER = 52;  // 2^52 = 4.503599627370496e+15

  // initialize
  double vc = 1.0;
  double ctry = 1.0;
  double alpha = 0.0;
  double beta = 0.0;
  double DF = ( double )( n );
  double DROP = 1.0 / 3.0;
  double BOOST = 2.0;
  double LAMBDA = 0.1L;
  double fold = 0.0, fnew = 0.0, anew = LDBL_MAX, rho = LDBL_MAX;
  fold = fboxcox( symmetric, n, x, y, mconst, w, vc, &alpha, &beta );
  double mxbeta = fabs( beta );

  // complete Levenberg-Marquardt algorithm
  if ( mxbeta > DBL_EPSILON ) {
    for ( size_t iter = 1; iter <= MAXITER; iter++ ) {

      // one single step
      for ( size_t inner = 1; inner <= MAXINNER; inner++ ) {

        // solve: alpha ( 1 + lambda ) da = beta for da
        double covar = alpha * ( 1.0 + LAMBDA );
        double da = beta / covar;
        double scale = da * ( LAMBDA * alpha * da + beta );
        ctry = vc + da;

        // administration: update convergence statistics
        anew = fabs( da );
        if ( anew <= DBL_EPSILON ) break;
        fnew = fboxcox( symmetric, n, x, y, mconst, w, ctry, &covar, &da );
        rho = ( fold - fnew ) / scale;

        // administration: update damping and parameters
        if ( rho > DBL_EPSILON ) {
          LAMBDA = fmax( DROP * LAMBDA, DBL_EPSILON );
          BOOST = 2.0;
          DROP /= 3.0;
          fold = fnew;
          alpha = covar;
          beta = da;
          vc = ctry;
          mxbeta = fabs( beta );
          break;
        }
        else {
          LAMBDA = fmin( BOOST * LAMBDA, 1.0 / DBL_EPSILON );
          BOOST *= 2.0;
          DROP = 1.0 / 3.0;
          fnew = fold;
        }
      }

      // administration: check convergence
      if ( anew <= DBL_EPSILON ) break;
      if ( mxbeta <= DBL_EPSILON ) break;
      if ( fnew / DF < DBL_EPSILON ) break;
    }
  }

  // set transformed vector
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( i != j ) r[i][j]  = ( vc < -DBL_EPSILON || vc > DBL_EPSILON ? ( pow( x[i][j] + 1.0, vc ) - 1.0 ) / vc : log( x[i][j] + 1.0 ) );
} // nnboxcox

void monotone( const size_t n, double* x, double* w )
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

static void knotsequenceuniform( double* boundaries, const size_t ninner, double* iknots )
{
  if ( ninner == 0 ) return;
  const double xmin = boundaries[1];
  const double xmax = boundaries[2];
  const double xrange = xmax - xmin;
  const double interval = xrange / ( double )( ninner + 1 );
  for ( size_t k = 1; k <= ninner; k++ ) iknots[k] = xmin + ( double )( k ) * interval;
} // knotsequenceuniform

static void knotsequencepercentile( const size_t n, double* x, double* w, const size_t ninner, double* iknots )
{
  if ( ninner == 0 ) return;
  double* tmpx = getvector( n, 0.0 );
  dcopy( n, &x[1], 1, &tmpx[1], 1 );
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
  dcopy( n, &x[1], 1, &tmpx[1], 1 );
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
  if ( knotstype == KNOTSTYPE.NONE ) checkandorderknotsequence( ninner, iknots, boundaries );
  else if ( knotstype == KNOTSTYPE.UNIFORM ) knotsequenceuniform( boundaries, ninner, iknots );
  else if ( knotstype == KNOTSTYPE.PERCENTILE ) knotsequencepercentile( n, x, w, ninner, iknots );
  else if ( knotstype == KNOTSTYPE.MIDPERCENTILE ) knotsequencemidpercentile( n, x, w, ninner, iknots );
}

static double* extendedknotsequence( const size_t ninner, double* iknots, double* boundaries, const size_t degree )
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
  
  double* knots = extendedknotsequence( ninner, iknots, boundaries, 0 );
  size_t* l = getvector_t( n, ( size_t )( 0 ) );
  for ( size_t i = 1; i <= n; i++ ) l[i] = bisect( nknots, knots, x[i] );
  freevector( knots );

  double** bbase = getmatrix( n, ncoefs, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) bbase[i][l[i]] = 1.0;
  
  double* eknots = extendedknotsequence( ninner, iknots, boundaries, degree );
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
  freevector_t( l );
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

void polynomialcoefficients( const bool symmetric, const size_t n, double** d, double** w, const size_t m, double** base, double** bstbs, double* b, const double mconst, double** gamma )
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
  if ( isnull( w ) ) fastnnls( nn, m, base, bstbs, b, vd );
  else fastnnwls( nn, m, base, NULL, b, vd, vw );
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
// procrustes functions
//

static void procrustestranslation( const size_t n, const size_t p, double** a, double** b, double** r, double s, double* t )
{
  daxpy( n * p, -1.0, &b[1][1], 1, &a[1][1], 1 );
  double** rxt = getmatrix( p, n, 0.0 );
  dgemm( false, true, p, n, p, 1.0, r, a, 0.0, rxt );
  for ( size_t i = 1; i <= p; i++ ) t[i] = ( double )( dsum( n, &rxt[i][1], 1 ) );
  freematrix( rxt );
  dscal( p, 1.0 / ( s * ( double )( n ) ), &t[1], 1 );
} // translation

static void procrustesscaling( const size_t n, const size_t p, double** a, double** b, double ssq, double* s )
{
  double** atb = getmatrix( p, p, 0.0 );
  dgemm( true, false, p, p, n, 1.0, a, b, 0.0, atb );
  ( *s ) = dsum( p, &atb[1][1], p + 1 );
  if ( ssq <= DBL_EPSILON ) ( *s ) = 1.0;
  else {
    if ( ( *s ) <= sqrt( DBL_EPSILON ) ) ( *s ) = 1.0;
    else ( *s ) /= ssq;
  }
  freematrix( atb );
} // scaling

static int procrustesrotation( const size_t n, const size_t p, double** a, double** b, double** r )
{
  int info = 0;
  double** atb = getmatrix( p, p, 0.0 );
  dgemm( true, false, p, p, n, 1.0, a, b, 0.0, atb );
  double** u = getmatrix( p, p, 0.0 );
  double* w = getvector( p, 0.0 );
  double** v = getmatrix( p, p, 0.0 );
  if ( ( info = svdcmp( p, p, atb, u, w, v ) ) == 0 ) dgemm( false, true, p, p, p, 1.0, u, v, 0.0, r );
  freematrix( atb );
  freematrix( u );
  freematrix( v );
  freevector( w );
  return info;
} // rotation

double procrustes( const size_t n, const size_t p, double** a, double** b, double** res )
// this routine computes a new a, optimally rotated, scaled, and translated towards b
{
  double** cj = getmatrix( n, n, 0.0 );
  double work = 1.0 / ( double )( n );
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t ii = 1; ii <= n; ii++ ) cj[i][ii] = -1.0 * work;
    cj[i][i] = 1.0 - work;
  }
  double** jb = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, n, 1.0, cj, b, 0.0, jb );
  double** r = getmatrix( p, p, 0.0 );
  if ( procrustesrotation( n, p, a, jb, r ) != 0 ) {
    freematrix( cj );
    freematrix( jb );
    freematrix( r );
    return -1.0;
  }
  double** ja = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, n, 1.0, cj, a, 0.0, ja );
  double** atja = getmatrix( p, p, 0.0 );
  dgemm( true, false, p, p, n, 1.0, a, ja, 0.0, atja );
  double ss = dsum( p, &atja[1][1], p + 1 );
  double** ar = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, p, 1.0, a, r, 0.0, ar );
  double s = 1.0;
  procrustesscaling( n, p, ar, jb, ss, &s );
  dscal( n * p, s, &ar[1][1], 1 );
  double* t = getvector( p, 0.0 );
  procrustestranslation( n, p, ar, b, r, s, t );
  for ( size_t k = 1; k <= p; k++ ) {
    const double work = t[k];
    for ( size_t i = 1; i <= n; i++ ) res[i][k] = a[i][k] - work;
  }
  dgemm( false, false, n, p, p, 1.0, res, r, 0.0, ja );
  dscal( n * p, s, &res[1][1], 1 );
  freematrix( cj );
  freematrix( jb );
  freematrix( r );
  freematrix( ja );
  freematrix( atja );
  freematrix( ar );
  freevector( t );
  const double aa = dssq( n, &res[1][1], 1 );
  const double bb = dssq( n, &b[1][1], 1 );
  const double ab = ddot( n, &res[1][1], 1, &b[1][1], 1 );
  const double lower = sqrt( aa * bb );
  const double phi = ( iszero( lower ) ? 1.0 : ab / lower );
  return ( phi > 1.0 ? 1.0 : phi );
} // procrustes

void varimaxrotation( const size_t n, const size_t m, double** x, double** loadings, double** rotation ) 
{
  const bool NORMALIZE = true;
  const size_t MAXITER = 1024;
  const double EPS = 0.00001;

  if ( m < 2 ) {
    dcopy( n * m, &x[1][1], 1, &loadings[1][1], 1 );
    rotation[1][1] = 1.0;
    return;
  }

  double* nrm = getvector( n, 0.0 );
  if ( NORMALIZE == true ) {
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= m; j++ ) work += x[i][j] * x[i][j];
      nrm[i] = work = sqrt( work );
      for ( size_t j = 1; j <= m; j++ ) x[i][j] /= work;
    }
  }

  double* zj = getvector( m, 0.0 );
  double** B = getmatrix( m, m, 0.0 );
  double** u = getmatrix( m, m, 0.0 );
  double** v = getmatrix( m, m, 0.0 );
  double* d = getvector( m, 0.0 );

  for ( size_t i = 1; i <= m; i++ ) {
    for ( size_t j = 1; j <= m; j++ ) rotation[i][j] = 0.0;
    rotation[i][i] = 1.0;
  }
  double fold = DBL_MAX;

  for ( size_t iter = 1; iter <= MAXITER; iter++ ) {
    dgemm( false, false, n, m, m, 1.0, x, rotation, 0.0, loadings );
    for ( size_t j = 1; j <= m; j++ ) {
      double work = 0.0;
      for ( size_t i = 1; i <= n; i++ ) work += loadings[i][j] * loadings[i][j];
      zj[j] = work / ( double )( n );
    }
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) loadings[i][j] = pow( loadings[i][j], 3.0 ) -  loadings[i][j] * zj[j];
    }
    dgemm( true, false, m, m, n, 1.0, x, loadings, 0.0, B );
    svdcmp( m, m, B, u, d, v );
    dgemm( false, true, m, m, m, 1.0, u, v, 0.0, rotation );
    double fnew = dsum( m, &d[1], 1 ); 
    if ( fnew < fold * ( 1.0 + EPS ) ) break;
    fold = fnew;
  }

  dgemm( false, false, n, m, m, 1.0, x, rotation, 0.0, loadings );

  if ( NORMALIZE == true ) {
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) x[i][j] *= nrm[i];
    }
  }
  freevector( nrm );

  freevector( zj );
  freematrix( B );
  freematrix( u );
  freematrix( v );
  freevector( d );
}

static void principalaxesrotation( const size_t n, const size_t p, double** z, double** r, double* ev )
// return principal axes rotation matrix R
// return identity matrix on error
{
  dgemm( true, false, p, p, n, 1.0, z, z, 0.0, r );
  if ( evdcmp( p, r, ev ) != 0 ) {
    dset( p * p, 0.0, &r[1][1], 1 );
    for ( size_t k = 1; k <= p; k++ ) r[k][k] = 1.0;
    return;
  }
  for ( size_t k = 1; k <= p; k++ ) {
    double work = 0.0;
    for ( size_t i = 1; i <= p; i++ ) work += z[1][i] * r[i][k];
    if ( work < 0.0 ) for ( size_t i = 1; i <= p; i++ ) r[i][k] *= -1.0;
  }
} // principalaxesrotation

static void weightedprincipalaxesrotation( const size_t n, const size_t p, double** z, double* w, double** r, double* ev )
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
    dset( p * p, 0.0, &r[1][1], 1 );
    for ( size_t k = 1; k <= p; k++ ) r[k][k] = 1.0;
    return;
  }
  for ( size_t k = 1; k <= p; k++ ) {
    double work = 0.0;
    for ( size_t i = 1; i <= p; i++ ) work += z[1][i] * r[i][k];
    if ( work < 0.0 ) for ( size_t i = 1; i <= p; i++ ) r[i][k] *= -1.0;
  }
} // weightedprincipalaxesrotation

void rotate( const size_t n, const size_t p, double** z )
// rotate Z to principal axes, plus
{
  double* ev = getvector( max_t( n, p ), 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  principalaxesrotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  dcopy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  freematrix( r );
} // rotate

void weightedrotate( const size_t n, const size_t p, double** z, double* w )
// rotate Z to principal axes
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  weightedprincipalaxesrotation( n, p, z, w, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  dcopy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  freematrix( r );
} // weightedrotate

void rotateplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1 )
// rotate Z to principal axes, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  principalaxesrotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  dcopy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  dgemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  dcopy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  freematrix( r );
} // rotateplus

void weightedrotateplus( const size_t n, const size_t p, double** z, double* w, const size_t n1, double** z1 )
// rotate Z to principal axes, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  weightedprincipalaxesrotation( n, p, z, w, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  dcopy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  dgemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  dcopy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  freematrix( r );
} // rotateplus

void rotateplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2 )
// rotate Z to principal axes, plus, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  principalaxesrotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  dcopy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  dgemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  dcopy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  double** tz2 = getmatrix( n2, p, 0.0 );
  dgemm( false, false, n2, p, p, 1.0, z2, r, 0.0, tz2 );
  dcopy( n2 * p, &tz2[1][1], 1, &z2[1][1], 1 );
  freematrix( tz2 );
  freematrix( r );
} // rotateplusplus

void rotateplusplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2, const size_t n3, double** z3 )
// rotate Z to principal axes, plus, plus, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  principalaxesrotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  dgemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  dcopy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  dgemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  dcopy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  double** tz2 = getmatrix( n2, p, 0.0 );
  dgemm( false, false, n2, p, p, 1.0, z2, r, 0.0, tz2 );
  dcopy( n2 * p, &tz2[1][1], 1, &z2[1][1], 1 );
  freematrix( tz2 );
  double** tz3 = getmatrix( n3, p, 0.0 );
  dgemm( false, false, n3, p, p, 1.0, z3, r, 0.0, tz3 );
  dcopy( n3 * p, &tz3[1][1], 1, &z3[1][1], 1 );
  freematrix( tz3 );
  freematrix( r );
} // rotateplusplus

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// local neighborhood search functions
//

static void swaprows( const size_t m, double** a, const size_t r1, const size_t r2 )
{
  for ( size_t j = 1; j <= m; j++ ) {
    double temp = a[r1][j];
    a[r1][j] = a[r2][j];
    a[r2][j] = temp;
  }
}

static void swapcols( const size_t n, double** a, const size_t c1, const size_t c2 )
{
  for ( size_t i = 1; i <= n; i++ ) {
    double temp = a[i][c1];
    a[i][c1] = a[i][c2];
    a[i][c2] = temp;
  }
}

size_t pairwiseinterchange( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER )
{
  size_t changesmade = 0;
  for ( size_t iter = 1; iter <= MAXITER; iter++ ) {
    bool changed = false;
    for ( size_t i = 1; i < n; i++ ) {
      for ( size_t j = i + 1; j <= n; j++ ) {

        double fold = 0.0;
        for ( size_t k = 1; k <= n; k++ ) fold += delta[i][k] * d[i][k];
        for ( size_t k = 1; k <= n; k++ ) fold += delta[j][k] * d[j][k];
        fold -= delta[i][j] * d[i][j];

        double fnew = 0.0;
        for ( size_t k = 1; k <= n; k++ ) fnew += delta[i][k] * d[j][k];
        for ( size_t k = 1; k <= n; k++ ) fnew += delta[j][k] * d[i][k];
        fnew -= delta[i][j] * d[j][i];

        if ( fnew > fold ) {
          changed = true;
          changesmade++;
          swaprows( p, z, i, j );
          swaprows( n, d, i, j );
          swapcols( n, d, i, j );
        }
      }
    }
    if ( changed == false ) break;
  }
  return( changesmade );
} // pairwiseinterchange        

size_t objectinsertion( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER )
{
  // return for other than one dimension
  if ( p > 1 ) return( 0 );

  // allocate memory
  size_t* loc = getvector_t( n, 0.0 );
  double* srt = getvector( n, 0.0 );

  // determine location ordered objects
  for ( size_t i = 1; i <= n; i++ ) srt[i] = z[i][1];
  for ( size_t i = 1; i <= n; i++ ) loc[i] = i;
  dsort( n, srt, loc );

  // iteration loop
  size_t changesmade = 0;
  for ( size_t iter = 1; iter <= MAXITER; iter++ ) {
    bool changed = false;

    // for all objects do ...  
    for ( size_t i = 1; i <= n; i++ ) { 

      // current stress object i
      double fold = 0.0;
      for ( size_t j = 1; j <= n; j++ ) fold += pow( delta[i][j] - d[i][j], 2.0 );
      size_t moveto = 0;
  
      // check for stress before location 1
      const size_t right = loc[1];
      if ( i != right ) {
        double fnew = 0.0;
        for ( size_t k = 1; k <= n; k++ ) fnew += pow( delta[i][k] - d[right][k], 2.0 );
        if ( fnew < fold ) {
          moveto = 1;
          fold = fnew;
        }
      }

      // check for stress between locations 1 and n
      for ( size_t j = 2; j <= n; j++ ) {
        const size_t left = loc[j - 1];
        const size_t right = loc[j];
        if ( i == left || i == right ) continue;
        double fnew = 0.0;
        for ( size_t k = 1; k <= n; k++ ) {
          const double dave = 0.5 * ( d[left][k] + d[right][k] );
          fnew += pow( delta[i][k] - dave, 2.0 );
        }
        if ( fnew < fold ) {
          moveto = j;
          fold = fnew;
        }
      }
  
      // check for stress after location n
      const size_t left = loc[n];
      if ( i != left ) {
        double fnew = 0.0;
        for ( size_t k = 1; k <= n; k++ ) fnew += pow( delta[i][k] - d[left][k], 2.0 );
        if ( fnew < fold ) moveto = n + 1;
      }

      if ( moveto == 0 ) continue;  // no improvement
      changed = true;
      changesmade++;

      if ( moveto == 1 ) {
        const size_t right = loc[1];
        for ( size_t k = 1; k <= p; k++ ) z[i][k] = z[right][k];
      }
      else if ( moveto == n + 1 ) {
        const size_t left = loc[n];
        for ( size_t k = 1; k <= p; k++ ) z[i][k] = z[left][k];
      }
      else {
        const size_t left = loc[moveto - 1];
        const size_t right = loc[moveto];
        for ( size_t k = 1; k <= p; k++ ) z[i][k] = 0.5 * ( z[left][k] + z[right][k] );
      }
      for ( size_t j = 1; j <= n; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= p; k++ ) work += pow( z[i][k] - z[j][k], 2.0 );
        d[i][j] = d[j][i] = sqrt( work );
      }
      for ( size_t i = 1; i <= n; i++ ) srt[i] = z[i][1];
      for ( size_t i = 1; i <= n; i++ ) loc[i] = i;
      dsort( n, srt, loc );
    }

    if ( changed == false ) break;
  }

  // de-allocate memory
  freevector_t( loc );
  freevector( srt );

  return( changesmade );
} // objectinsertion        

size_t objectoverlay( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER )
{
  // iteration loop
  size_t changesmade = 0;
  for ( size_t iter = 1; iter <= MAXITER; iter++ ) {
    bool changed = false;
    for ( size_t i = 1; i <= n; i++ ) { 
      double fold = 0.0;
      for ( size_t j = 1; j <= n; j++ ) fold += pow( delta[i][j] - d[i][j], 2.0 );
      size_t moveto = 0;
      for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
        double fnew = 0.0;
        for ( size_t l = 1; l <= n; l++ ) if ( i != l ) fnew += pow( delta[i][l] - d[j][l], 2.0 );
        if ( fnew < fold ) {
          moveto = j;
          fold = fnew;
        }
      }
      if ( moveto == 0 ) continue;  // no improvement
      changed = true;
      changesmade++;
      for ( size_t k = 1; k <= p; k++ ) z[i][k] = z[moveto][k];
      for ( size_t j = 1; j <= n; j++ ) d[i][j] = d[moveto][j];
      for ( size_t j = 1; j <= n; j++ ) d[j][i] = d[j][moveto];
      d[i][moveto] = d[moveto][i] = 0.0;
    }
    if ( changed == false ) break;
  }
  return( changesmade );
} // objectoverlay

bool localneighborhoodsearch1( const size_t n, double** delta, const size_t p, double** z, double** d )
{
  if ( p != 1 ) return( false );

  //const double TOL = sqrt( DBL_EPSILON );

  double *x = getvector( n, 0.0 );
  size_t *r = getvector_t( n, 0 );
  for ( size_t i = 1; i <= n; i++ ) {
    x[i] = z[i][1];
    r[i] = i;
  }
  dsort( n, x, r );

  bool changed = true;
  while ( changed == true ) {
    changed = false;
    if ( pairwiseinterchange( n, delta, p, z, d, 0 ) ) changed = true;
  
  // only the fastest procedures should be used here
  // preferably deterministic
  // other procedure (stochastic) can be moved to mutation

  // pairwise interchange or (repaired) exchange mutation
  // loops over all combinations (slow?)

  // adaptive mutation (slow? but good?)
  // use the patterns to create offspring

  // three exchange mutation (mutation?)
  // same as pairwise interchange but with triple

  // re-insertion (fast?)
  // use block >= 1

  // rotation or invertion (mutation?)
  // use block > 1
  }

  freevector( x );
  freevector_t( r );

  return( changed );
}

bool localneighborhoodsearch2( const size_t n, double** delta, const size_t p, double** z )
{
  if ( p != 2 ) return( false );

  //const double TOL = sqrt( DBL_EPSILON );

  double *x = getvector( n, 0.0 );
  size_t *r = getvector_t( n, 0 );
  for ( size_t i = 1; i <= n; i++ ) {
    x[i] = z[i][1];
    r[i] = i;
  }
  dsort( n, x, r );

  bool changed = false;

  // only the fastest procedures should be used here
  // preferably deterministic
  // other procedure (stochastic) can be moved to mutation

  // pairwise interchange or (repaired) exchange mutation
  // loops over all combinations (slow?)

  // adaptive mutation (slow? but good?)
  // use the patterns to create offspring

  // three exchange mutation (mutation?)
  // same as pairwise interchange but with triple

  // re-insertion (fast?)
  // use block >= 1

  // rotation or invertion (mutation?)
  // use block > 1

  freevector( x );
  freevector_t( r );

  return( changed );
}

double rawstress( const size_t n, double** delta, const size_t p, double** z, double** d )
{
  if ( z == NULL && d == NULL ) return( DBL_MAX );
  if ( d == NULL ) {
    d = getmatrix( n, n, 0.0 );
    euclidean1( n, p, z, d );
  }
  double work = 0.0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) {
    const double diff = delta[i][j] - d[i][j];
    work += diff * diff;
  }
  freematrix( d );
  d = NULL;
  return( work );
}

double WRCWG( const size_t n, double** delta, const size_t p, double** z )
// within row and column weighted gradient
{
  if ( p != 1 ) return( 0.0 );

  double *x = getvector( n, 0.0 );
  size_t *r = getvector_t( n, 0 );
  for ( size_t i = 1; i <= n; i++ ) {
    x[i] = z[i][1];
    r[i] = i;
  }
  dsort( n, x, r );
  freevector( x );

  double value = 0.0;
  for ( size_t k = 1; k <= n - 2; k++ ) {
    for ( size_t l = k + 1; l <= n - 1; l++ ) {
      for ( size_t m = l + 1; m <= n; m++ ) {
        size_t kk = r[k];
        size_t ll = r[l];
        size_t mm = r[m];
        double a1 = delta[kk][mm];
        double a2 = delta[kk][ll];
        double a3 = delta[ll][mm];
        value += ( 2.0 * a1 - a2 - a3 );
      }
    }
  }
  freevector_t( r );
  return( value );
}

void maxmin( const size_t n, double** const d, const size_t nlm, size_t* lm )
{
  // allocate memory
  double* mn = getvector( n, 0.0 );

  // determine first landmark: largest data point
  for ( size_t i = 1; i <= n; i++ ) {
    double work = 0.0;
    for ( size_t j = 1; j <= n; j++ ) work += d[i][j];
    mn[i] = work / ( double )( n );
  }
  size_t landmark = 1;
  double mx = 0.0L;
  for ( size_t i = 1; i <= n; i++ ) if ( mn[i] > mx ) {
    mx = mn[i];
    landmark = i;
  }
  lm[1] = landmark;

  // use maxmin to find remaining landmarks
  for ( size_t i = 1; i <= n; i++ ) mn[i] = d[landmark][i];
  for ( size_t k = 2; k <= nlm; k++ ) {
    landmark = 1;
    mx = 0.0L;
    for ( size_t i = 1; i <= n; i++ ) if ( mn[i] > mx ) {
      mx = mn[i];
      landmark = i;
    }
    lm[k] = landmark;
    for ( size_t i = 1; i <= n; i++ ) mn[i] = fmin( mn[i], d[landmark][i] );
  }

  // de-allocate memory
  freevector( mn );

} // maxmin

double lincolnpetersen( const size_t n, double* insample, const double fcrit )
{
  const double sqrtfcrit = sqrt( fcrit );
  size_t m = n / 2;
  if ( m == 0 ) return( 0.0 );

  size_t nmarked = 0;
  double* marked = getvector( m, 0.0 );
  for ( size_t i = 1; i <= m; i++ ) {
    const double f = insample[i];
    bool same = false;
    for ( size_t k = 1; k <= nmarked; k++ ) if ( fabs( f - marked[k] ) < sqrtfcrit ) {
      same = true;
      break;
    }
    if ( same == false ) {
      nmarked++;
      marked[nmarked] = f;
    }
  }

  size_t ndrawn = 0;
  double* drawn = getvector( m, 0.0 );
  for ( size_t i = m + 1; i <= n; i++ ) {
    const double f = insample[i];
    bool same = false;
    for ( size_t k = 1; k <= ndrawn; k++ ) if ( fabs( f - drawn[k] ) < sqrtfcrit ) {
      same = true;
      break;
    }
    if ( same == false ) {
      ndrawn++;
      drawn[ndrawn] = f;
    }
  }

  size_t noldmarks = 0;
  for ( size_t i = 1; i <= ndrawn; i++ ) {
    const double f = drawn[i];
    for ( size_t k = 1; k <= nmarked; k++ ) if ( fabs( f - marked[k] ) < sqrtfcrit ) {
      noldmarks++;
      break;
    }
  }

  const double Nhat = ( noldmarks == 0 ? 0.0 : ( double)( nmarked ) * ( double )( ndrawn ) / ( double )( noldmarks ) );
  return( Nhat );
}

double schnabel( const size_t n, double* catch, double* recaps, double* newmarks )
// Schnabel method for estimation population size
// catch    = vector containing the number of animal caught in each mark-recapture experiment
// recaps   = vector containing the number of animal recaptured in each mark-recapture experiment
// newmarks = vector containing the newly marked animals in each mark-recapture experiment
{
  double* marks = getvector( n, 0.0 );
  for ( size_t i = 2; i <= n; i++ ) marks[i] = marks[i-1] + newmarks[i-1];
  const double sm = dsum( n, &recaps[1], 1 );
  if ( iszero( sm ) ) return 0.0;
  const double dt = ddot( n, &catch[1], 1, &marks[1], 1 );
  const double N = dt / sm;
  freevector( marks );
  return( N );
} // schnabel

double chapman( const size_t n, double* catch, double* recaps, double* newmarks )
// Chapman method for estimation population size
// catch    = vector containing the number of animal caught in each mark-recapture experiment
// recaps   = vector containing the number of animal recaptured in each mark-recapture experiment
// newmarks = vector containing the newly marked animals in each mark-recapture experiment
{
  double* marks = getvector( n, 0.0 );
  for ( size_t i = 2; i <= n; i++ ) marks[i] = marks[i-1] + newmarks[i-1];
  const double sm = dsum( n, &recaps[1], 1 );
  const double dt = ddot( n, &catch[1], 1, &marks[1], 1 );
  const double N = dt / ( sm + 1.0 );
  freevector( marks );
  return( N );
} // chapman

double schumachereschmeyer( const size_t n, double* catch, double* recaps, double* newmarks )
// Schumacher-Eschmeyer method for estimation population size
// catch    = vector containing the number of animal caught in each mark-recapture experiment
// recaps   = vector containing the number of animal recaptured in each mark-recapture experiment
// newmarks = vector containing the newly marked animals in each mark-recapture experiment
{
  double* marks = getvector( n, 0.0 );
  for ( size_t i = 2; i <= n; i++ ) marks[i] = marks[i-1] + newmarks[i-1];
  const double dt = ddot( n, &recaps[1], 1, &marks[1], 1 );
  if ( iszero( dt ) ) return 0.0;
  double dt2 = 0.0;
  for ( size_t i = 1; i <= n; i++ ) dt2 += catch[i] * marks[i] * marks[i];
  const double N = dt2 / dt;
  freevector( marks );
  return( N );
} // schumachereschmeyer

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// (pseudo) (nonparameteric) confidence intervals
//

void pseudoconfidenceintervals( const size_t n, double** delta )
{
}

void jackknifeconfidenceintervals( const size_t n, double** delta )
{
}

void bootstrapconfidenceintervals( const size_t n, double** delta )
{
}

void permutationconfidenceintervals( const size_t n, double** delta )
{
}

void subsampleconfidenceintervals( const size_t n, double** delta )
{
}

static int angle( const size_t p, double** v, double* angle )
// returns angle(s) in degrees from rotation matrix
// order in angles for 1-dim: 0 (no rotation possible)
//                     2-dim: 1-2 (theta)
//                     3-dim: 1-2, 1-3, 2-3 (theta, psi, phi)
//                     4-dim: 0 (no rotation computed)
// ref: Gregory G. Slabaugh. Computing Euler angles from a rotation matrix
{
  int retval = 0;

  const double pi = 3.14159265358979323846;
  const double r2d = 180.0 / pi;

  // one dimension
  if ( p == 1 ) {
    angle[1] = 0.0;
    return retval = 1;
  }

  // two dimensions
  else if ( p == 2 ) {
    angle[1] = r2d * atan2( v[2][1], v[1][1] );
    return retval = 0;
  }

  // three dimensions
  else if ( p == 3 ) {
    double theta = 0.0;
    double psi = 0.0;
    double phi = 0.0;
    if ( isnotequal( v[3][1], 1.0 ) && isnotequal( v[3][1], -1.0 ) ) {
      theta = -1.0 * asin( v[3][1] );                                    // or other theta = pi-theta; 
      psi = atan2( v[3][2] / cos( theta ), v[3][3] / cos( theta ) );     // or psi = atan2( v[3][2]/cos( other theta ), v[3][3]/cos( other theta ) );
      phi = atan2( v[2][1] / cos( theta ), v[1][1] / cos( theta ) );     // or phi = atan2( v[2][1]/cos( other theta ), v[1][1]/cos( other theta ) );
    }
    else {
      if ( isequal( v[3][1], -1.0 ) ) {
        theta = pi / 2.0;
        psi = phi + atan2( v[1][2], v[1][3] );
      }
      else {
        theta = -1.0 * pi / 2.0;
        psi = -1.0 * phi + atan2( -v[1][2], -v[1][3] );
      }
    }
    angle[1] = r2d * phi;
    angle[2] = r2d * theta;
    angle[3] = r2d * psi;
    return retval = 0;
  }

  // more than three dimensions
  else {
    for ( size_t k = 1; k <= p; k++ ) angle[k] = 0.0;
    return retval = 4;
  }
} // angle

double ciellipse( const size_t n, const size_t p, double** x, const double ci, double* c, double* r, double* a )
// Heiser and Meulman, 1983
// computes confidence elipsoid from a set (n) of points x in p dimensions
// returns centroids (c), radii (r), angles (a) (in degrees), and area (return value)
{
  // compute centroid
  for ( size_t k = 1; k <= p; k++ ) c[k] = dsum( n, &x[1][k], p ) / ( double )( n );

  // center points
  double** xc = getmatrix( n, p, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) xc[i][k] = x[i][k] - c[k];

  // singular value decomposition
  double** u = getmatrix( n, n, 0.0 );
  double* w = getvector( n, 0.0 );
  double** v = getmatrix( p, p, 0.0 );
  svdcmp( n, p, xc, u, w, v );
  freematrix( xc );

  // distances to centroid
  double* d = getvector( n, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) d[i] = sqrt( dssq( p, &u[i][1], 1 ) );
  freematrix( u );

  // sort distances
  dsort0( n, d );

  // compute radii
  size_t mi = max_t( 1, ( size_t )( floor( ci * ( double )( n ) + 0.5 ) ) );
  size_t ma = min_t( n, mi + 1 );
  double alpha = ci * ( double )( n ) + 0.5 - ( double )( mi );
  double radius = ( 1.0 - alpha ) * d[mi] + alpha * d[ma];
  for ( size_t k = 1; k <= p; k++ ) r[k] = radius * w[k];
  freevector( w );
  freevector( d );

  // determine angle
  angle( p, v, a );
  freematrix( v );

  // return area
  double area = 3.14159265358979323846;
  for ( size_t k = 1; k <= p; k++ ) area *= r[k];
  return area;
} // ciellipse

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// R print functions
//

void printerror( char* title )
{
  #ifdef R
    Rprintf( "error: %s\n", title );
  #else
    printf( "error: %s\n", title );
    exit( 0 );
  #endif
} // printerror

void printpass( const size_t nr )
{
  #ifdef R
    Rprintf( "pass %zu\n", nr );
  #else
    printf( "pass %lu\n", ( unsigned long ) nr );
  #endif
} // printpass

void printstring( char* title, char* s )
{
  #ifdef R
    Rprintf( "%s", title );
    Rprintf( "%s", s );
  #else
    printf( "%s", title );
    printf( "%s", s );
  #endif
} // printstring

void printscalar_t( char* title, const size_t a )
{
  #ifdef R
    Rprintf( "%s", title );
    Rprintf( " = %zu\n", a );
  #else
    printf( "%s", title );
    printf( " = %lu\n", ( unsigned long ) a );
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
    for ( size_t i = 1; i <= n; i++ ) printf( " %lu", ( unsigned long ) a[i] );
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
      printf( "iter = %6lu; fnew = %0.15f\n", ( unsigned long ) iter, fold );
    #endif
  }
  else {
    #ifdef R
      Rprintf( "iter = %6zu; fold = %0.15f; fhalf = %0.15f; fnew = %0.15f; diff = %0.15f\n", iter, fold, fhalf, fnew, fold - fnew);
    #else
      printf( "iter = %6lu; fold = %0.15f; fhalf = %0.15f; fnew = %0.15f; diff = %0.15f\n", ( unsigned long ) iter, fold, fhalf, fnew, fold - fnew);
    #endif
  }
} // echoprogress

void writematrix( char* name, const size_t n, const size_t m, double** a ) {
  #ifdef R
  #else
    #ifdef _WIN32
      FILE *fptr;
      fptr = fopen( name, "w");
      for ( size_t i = 1; i <= n; i++ ) {
        for ( size_t j = 1; j <= m; j++ ) fprintf( fptr, "%0.15f ", a[i][j] );
        fprintf( fptr, "\n" );
      }
      fclose(fptr);
    #endif
  #endif  
} // writematrix

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

char* getdatetime( void )
{
  #ifdef R
    return( 0 );
  #else
    time_t t = time( NULL );
    struct tm *tm = localtime( &t );
    return asctime( tm );
  #endif
}

size_t setstarttime( void )
// return current time in time_t format
{
  #ifdef R
    return( 0 );
  #else
    struct timespec current_time;
    if ( clock_gettime( CLOCK_REALTIME, &current_time ) != 0 ) return 0;
    return current_time.tv_sec * CLOCKS_PER_SEC + current_time.tv_nsec / ( 1000000000 / CLOCKS_PER_SEC ); 
  #endif
} // setstarttime

double getelapsedtime( const size_t starttime )
// get current time in time_t format
// return time difference in seconds in double format
{
  #ifdef R
    return( 0.0 );
  #else
    struct timespec current_time;
    if ( clock_gettime( CLOCK_REALTIME, &current_time ) != 0 ) return 0.0;
    size_t endtime = ( size_t )( current_time.tv_sec * CLOCKS_PER_SEC + current_time.tv_nsec / ( 1000000000 / CLOCKS_PER_SEC ) ); 
    return( ( double )( endtime - starttime ) / 1000.0 );
  #endif
} // getelapsedtime

void lowercase( char* str )
{
  int i = 0;
  char chr;
  while ( str[i] ) {
    chr = str[i];
    str[i] = tolower( chr );
    i++;
  }
}

// -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// hashmap functions
//

hashmap* gethashmap( const size_t mapsize )
{
  #ifdef R
    return( NULL );
  #else
    hashmap* map = malloc( sizeof( hashmap ) * mapsize );
    for ( size_t i = 0; i < mapsize; i++ ) {
      map[i].vecsize = 0;
      map[i].vec = NULL;
    }
    return( map );
  #endif
} // gethashmap

void freehashmap( const size_t mapsize, hashmap* map )
{
  #ifdef R

  #else
    for ( size_t i = 0; i < mapsize; i++ ) freevector( map[i].vec );
    free( map );
    map = NULL;
  #endif
} // freehashmap

hashmap* readhashmap( char* infilename, size_t* mapsize )
{
  #ifdef R
    return( NULL );
  #else
    int MAXCHARS = 1024;
    char buffer[MAXCHARS];
    double vec[MAXCHARS];
    FILE *infile;
    fopen_s( &infile, infilename, "r" ); 
    char* sep = " ,\n";
    char* token = NULL;
    char* nexttoken = NULL;  
    char* ptr = NULL;
    if ( infile == NULL ) printerror( "Error: infilename not found in loadhashmap()" );
    size_t nlines = 0; 
    while ( fgets( buffer, MAXCHARS, infile ) != NULL ) nlines++;
    hashmap* map = malloc( sizeof( hashmap ) * nlines );
    rewind( infile );
    int i = -1;
    while ( fgets( buffer, sizeof( buffer ), infile ) ) { 
      i++;
      token = strtok_s( buffer, sep, &nexttoken ); 
      strcpy_s( map[i].key, strlen( token ) + 1, token );
      lowercase( map[i].key );
      strcpy( map[i].value, nexttoken ); 
      map[i].value[strlen(nexttoken)-1] = '\0';
      lowercase( map[i].value );
      size_t vecsize = 0;
      while ( token != NULL ) {
        token = strtok_s( NULL, sep, &nexttoken );  
        if ( token == NULL ) break;
        vecsize++;
        vec[vecsize] = ( double )( strtod( token, &ptr ) );
      }
      map[i].vecsize = vecsize;
      map[i].vec = getvector( vecsize, 0.0 );
      for ( size_t k = 1; k <= vecsize; k++ ) map[i].vec[k] = vec[k];
      if ( i >= nlines ) printerror( "Error: overflow in loadhashmap()" );
    }
    fclose( infile );
    ( *mapsize ) = nlines;
    return( map );
  #endif
} // readhashmap

void writehashmap( char* outfilename, const size_t mapsize, hashmap* map )
{
  #ifdef R

  #else
    FILE *outfile;
    outfile = fopen( outfilename, "w" );
    for ( size_t i = 0; i < mapsize; i++ ) fprintf( outfile, "%s %s\n", map[i].key, map[i].value );
    fclose( outfile ); 
  #endif
} // writehashmap

void displayhashmap( const size_t mapsize, hashmap* map )
{
  #ifdef R

  #else
    printf_s( "\n" );
    for ( size_t i = 0; i < mapsize; i++ ) {
      double num = roundat( map[i].vec[1], 15 );
      char str[64]; 
      sprintf( str, "%0.15f", num );
      size_t len = strlen( str ); 
      for ( size_t j = len - 1; j > 0; j-- ) if ( str[j] == '0' ) str[j] = str[j+1]; else break;
      len = strlen( str );
      if ( str[len-1] == '.' ) str[len-1] = str[len];
      printf_s( "key[%s] = value[(string)(%s),vecsize(%zu),vec(%s", map[i].key, map[i].value, map[i].vecsize, str );
      for ( size_t j = 2; j <= map[i].vecsize; j++ ) {
        num = roundat( map[i].vec[j], 15 );
        sprintf( str, "%f", num );
        len = strlen( str ); 
        for ( size_t k = len - 1; k > 0; k-- ) if ( str[k] == '0' ) str[k] = str[k+1]; else break;
        len = strlen( str );
        if ( str[len-1] == '.' ) str[len-1] = str[len];
        printf_s( ",%s", str );
      }
      printf_s( ")]\n" );
    }
  #endif
} // displayhashmap

char* gethashmapstring( const size_t mapsize, hashmap* map, char32 key, char1024* defchar )
{
  #ifdef R
    return( 0 );
  #else
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      return( ( char* )( &map[i].value ) );
    }
    return( ( char* ) defchar );
  #endif
} // gethashmapstring

int gethashmapint( const size_t mapsize, hashmap* map, char* key, const int defint )
{
  #ifdef R
    return( 0 );
  #else
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      return( ( int ) lround( map[i].vec[1] ) );
    }
    return( defint );
  #endif
} // gethashmapint

size_t gethashmapsize_t( const size_t mapsize, hashmap* map, char* key, const size_t defsize_t )
{
  #ifdef R
    return( 0 );
  #else
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      return( ( size_t ) llround( map[i].vec[1] ) );
    }
    return( defsize_t );
  #endif
} // gethashmapsize_t

double gethashmapdouble( const size_t mapsize, hashmap* map, char32 key, const double defdouble )
{
  #ifdef R
    return( 0.0 );
  #else
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      return( map[i].vec[1] );
    }
    return( defdouble );
  #endif
} // gethashmapdouble

int* gethashmapvectorint( const size_t mapsize, hashmap* map, char32 key )
{
  #ifdef R
    return( NULL );
  #else
    int* vec = NULL;
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      size_t len = min_t( strlen( p1 ), strlen( p2 ) ) - 1;
      if ( strncmp( p1, p2, len ) == 0 ) {
        vec = getivector( map[i].vecsize, 0 );
        for ( size_t k = 1; k <= map[i].vecsize; k++ ) vec[k] = ( int ) lround( map[i].vec[k] );
        return( vec );
      }
    }
    return( NULL );
  #endif
} // gethashmapvectorint

size_t* gethashmapvectorsize_t( const size_t mapsize, hashmap* map, char32 key )
{
  #ifdef R
    return( NULL );
  #else
    size_t* vec = NULL;
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      size_t len = min_t( strlen( p1 ), strlen( p2 ) ) - 1;
      if ( strncmp( p1, p2, len ) == 0 ) {
        vec = getvector_t( map[i].vecsize, 0 );
        for ( size_t k = 1; k <= map[i].vecsize; k++ ) vec[k] = ( size_t ) llround( map[i].vec[k] );
        return( vec );
      }
    }
    return( NULL );
  #endif
} // gethashmapvectorsize_t

double* gethashmapvectordouble( const size_t mapsize, hashmap* map, char32 key )
{
  #ifdef R
    return( NULL );
  #else
    double* vec = NULL;
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      vec = getvector( map[i].vecsize, 0.0 );
      for ( size_t k = 1; k <= map[i].vecsize; k++ ) vec[k] = map[i].vec[k];
      return( vec );
    }
    return( NULL );
  #endif
} // gethashmapvectordouble

void sethashmapstring( const size_t mapsize, hashmap* map, char32 key, const char1024 value )
{
  #ifdef R
    return;
  #else
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      strcpy( map[i].value, value );
      map[i].vecsize = 1;
      map[i].vec[1] = 0.0;
      return;
    }
    printerror( "key not found in sethahshmapstring()" );
  #endif
} // sethashmapstring

void sethashmapint( const size_t mapsize, hashmap* map, char32 key, const int value )
{
  #ifdef R
    return;
  #else
    char str[64]; 
    sprintf( str, "%i", value );
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      strcpy( map[i].value, str );
      map[i].vecsize = 1;
      map[i].vec[1] = ( double )( value );
      return;
    }
    printerror( "key not found in sethahshmapint()" );
  #endif
} // sethashmapint

void sethashmapsize_t( const size_t mapsize, hashmap* map, char32 key, const size_t value )
{
  #ifdef R
    return;
  #else
    char str[64]; 
    sprintf( str, "%lu", ( unsigned long ) value );
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      strcpy( map[i].value, str );
      map[i].vecsize = 1;
      map[i].vec[1] = ( double )( value );
      return;
    }
    printerror( "key not found in sethahshmapsize_t()" );
  #endif
} // sethashmapsize_t

void sethashmapdouble( const size_t mapsize, hashmap* map, char32 key, const double value )
{
  #ifdef R
    return;
  #else
    char str[64]; 
    sprintf( str, "%0.15f", value );
    for ( size_t i = 0; i < mapsize; i++ ) {
      char* p1 = map[i].key;
      char* p2 = key;
      if ( strlen( p1 ) != strlen( p2 ) ) continue;
      size_t len = strlen( p1 ) - 1;
      if ( strncmp( p1, p2, len ) != 0 ) continue;
      strcpy( map[i].value, str );
      map[i].vecsize = 1;
      map[i].vec[1] = ( double )( value );
      return;
    }
    printerror( "key not found in sethahshmapdouble()" );
  #endif
} // sethashmapdouble

void flib_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test flib at ", dt );
  printstring( "", "==============================================================\n" );

  // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // speed check two random number generators
  {
    double rn = 0;
  
    size_t tmr = setstarttime();
    // size_t seed_t = 42;
    // pcg32_srandom( seed_t, 1 );
    // for ( size_t i = 1; i <= 100000; i++ ) rn = pcg32_next( );
    printscalar( "elapsed for pcg32_random", getelapsedtime( tmr ) );
    printscalar( "random number", rn );

    tmr = setstarttime();
    long lseed = 42;
    randomize( &lseed );
    for ( size_t i = 1; i <= 100000; i++ ) rn = nextdouble( );
    printscalar( "elapsed for nextdouble", getelapsedtime( tmr ) );
    printscalar( "random number", rn );
  }

  // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // test reading and writing hashmap
  {
    char* infilename = "food.in";
    size_t mapsize = 0;
    hashmap* map = readhashmap( infilename, &mapsize );
    bool echo = 0 != gethashmapint( mapsize, map, "echo", 0 );
    if ( echo == true ) displayhashmap( mapsize, map );
    size_t MAXITER = gethashmapsize_t( mapsize, map, "maxiter", 65536 );
    printscalar_t( "maximum number of iterations", MAXITER );
  }

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test flib at ", dt );
  printstring( "", "==============================================================\n" );
} // unittest
