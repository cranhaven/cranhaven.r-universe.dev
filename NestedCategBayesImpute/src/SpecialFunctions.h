#pragma once
#include <vector>
#include <climits>
#include <cstdio>
#include <ctime>
#include <cmath>
using namespace std;

class MTRand {
public:
  typedef unsigned long uint32;
  enum { N = 624 };
  enum { SAVE = N + 1 };

protected:
  enum { M = 397 };  // period parameter
  uint32 state[N];   // internal state
  uint32 *pNext;     // next value to get from state
  int left;          // number of values left before reload needed

  //Methods
public:
  MTRand( const uint32& oneSeed );
  MTRand( uint32 *const bigSeed, uint32 const seedLength = N );
  MTRand();

  // Access to 32-bit random numbers
  double rand();                          // real number in [0,1]
  double rand( const double& n );         // real number in [0,n]
  double randExc();                       // real number in [0,1)
  double randExc( const double& n );      // real number in [0,n)
  double randDblExc();                    // real number in (0,1)
  double randDblExc( const double& n );   // real number in (0,n)
  uint32 randInt();                       // integer in [0,2^32-1]
  uint32 randInt( const uint32& n );      // integer in [0,n] for n < 2^32
  double operator()() { return rand(); }  // same as rand()

  // Access to nonuniform random number distributions
  double randNorm( const double& mean = 0.0, const double& variance = 0.0 );

  // Re-seeding functions with same behavior as initializers
  void seed( const uint32 oneSeed );
  void seed( uint32 *const bigSeed, const uint32 seedLength = N );
  void seed();

protected:
  void initialize( const uint32 oneSeed );
  void reload();
  uint32 hiBit( const uint32& u ) const { return u & 0x80000000UL; }
  uint32 loBit( const uint32& u ) const { return u & 0x00000001UL; }
  uint32 loBits( const uint32& u ) const { return u & 0x7fffffffUL; }
  uint32 mixBits( const uint32& u, const uint32& v ) const
  { return hiBit(u) | loBits(v); }
  uint32 twist( const uint32& m, const uint32& s0, const uint32& s1 ) const
  { return m ^ (mixBits(s0,s1)>>1) ^ (-loBit(s1) & 0x9908b0dfUL); }
  static uint32 hash( time_t t, clock_t c );
};


inline MTRand::MTRand( const uint32& oneSeed )
{ seed(oneSeed); }

inline MTRand::MTRand( uint32 *const bigSeed, const uint32 seedLength )
{ seed(bigSeed,seedLength); }

inline MTRand::MTRand()
{ seed(); }

inline double MTRand::rand()
{ return double(randInt()) * (1.0/4294967295.0); }

inline double MTRand::rand( const double& n )
{ return rand() * n; }

inline double MTRand::randExc()
{ return double(randInt()) * (1.0/4294967296.0); }

inline double MTRand::randExc( const double& n )
{ return randExc() * n; }

inline double MTRand::randDblExc()
{ return ( double(randInt()) + 0.5 ) * (1.0/4294967296.0); }

inline double MTRand::randDblExc( const double& n )
{ return randDblExc() * n; }

inline double MTRand::randNorm( const double& mean, const double& variance )
{
  // Return a real number from a normal (Gaussian) distribution with given
  // mean and variance by Box-Muller method
  double r = sqrt( -2.0 * log( 1.0-randDblExc()) ) * variance;
  double phi = 2.0 * 3.14159265358979323846264338328 * randExc();
  return mean + r * cos(phi);
}

inline MTRand::uint32 MTRand::randInt()
{
  // Pull a 32-bit integer from the generator state
  // Every other access function simply transforms the numbers extracted here

  if( left == 0 ) reload();
  --left;

  uint32 s1;
  s1 = *pNext++;
  s1 ^= (s1 >> 11);
  s1 ^= (s1 <<  7) & 0x9d2c5680UL;
  s1 ^= (s1 << 15) & 0xefc60000UL;
  return ( s1 ^ (s1 >> 18) );
}

inline MTRand::uint32 MTRand::randInt( const uint32& n ) {
  uint32 used = n;
  used |= used >> 1;
  used |= used >> 2;
  used |= used >> 4;
  used |= used >> 8;
  used |= used >> 16;
  uint32 i;
  do
    i = randInt() & used;
  while( i > n );
  return i;
}

inline void MTRand::seed( const uint32 oneSeed )
{
  initialize(oneSeed);
  reload();
}


inline void MTRand::seed( uint32 *const bigSeed, const uint32 seedLength ) {
  initialize(19650218UL);
  int i = 1;
  uint32 j = 0;
  int k = ( N > seedLength ? N : seedLength );
  for( ; k; --k )
  {
    state[i] =
      state[i] ^ ( (state[i-1] ^ (state[i-1] >> 30)) * 1664525UL );
    state[i] += ( bigSeed[j] & 0xffffffffUL ) + j;
    state[i] &= 0xffffffffUL;
    ++i;  ++j;
    if( i >= N ) { state[0] = state[N-1];  i = 1; }
    if( j >= seedLength ) j = 0;
  }
  for( k = N - 1; k; --k )
  {
    state[i] =
      state[i] ^ ( (state[i-1] ^ (state[i-1] >> 30)) * 1566083941UL );
    state[i] -= i;
    state[i] &= 0xffffffffUL;
    ++i;
    if( i >= N ) { state[0] = state[N-1];  i = 1; }
  }
  state[0] = 0x80000000UL;
  reload();
}


inline void MTRand::seed() {
  seed( hash( time(NULL), clock() ) );
}

inline void MTRand::initialize( const uint32 seed ) {
  uint32 *s = state;
  uint32 *r = state;
  int i = 1;
  *s++ = seed & 0xffffffffUL;
  for( ; i < N; ++i )
  {
    *s++ = ( 1812433253UL * ( *r ^ (*r >> 30) ) + i ) & 0xffffffffUL;
    r++;
  }
}

inline void MTRand::reload() {
  uint32 *p = state;
  int i;
  for( i = N - M; i--; ++p )
    *p = twist( p[M], p[0], p[1] );
  for( i = M; --i; ++p )
    *p = twist( p[M-N], p[0], p[1] );
  *p = twist( p[M-N], p[0], state[0] );

  left = N, pNext = state;
}

inline MTRand::uint32 MTRand::hash( time_t t, clock_t c ) {
  static uint32 differ = 0;
  uint32 h1 = 0;
  unsigned char *p = (unsigned char *) &t;
  for( size_t i = 0; i < sizeof(t); ++i ) {
    h1 *= UCHAR_MAX + 2U;
    h1 += p[i];
  }
  uint32 h2 = 0;
  p = (unsigned char *) &c;
  for( size_t j = 0; j < sizeof(c); ++j ) {
    h2 *= UCHAR_MAX + 2U;
    h2 += p[j];
  }
  return ( h1 + differ++ ) ^ h2;
}

class SpecialFunctions
{
public:
	SpecialFunctions(void);
	~SpecialFunctions(void);

	static double gammaln(double x);
	static double betaln(double x, double y);

	static double norminv(double p);			//inverse normal cdf
	static double normcdf(double u);

	static double gammainc(double x, double a);
	static double gammacdf(double x,double a, double b);
	static double gammainv(double p,double a, double b);
	static double gammapdf(double x,double a, double b);

	static void cmpower2(int nSize, double *px, double* py, double* pResult);
	static void cmrand(int nSize, MTRand& mt, double* pResult);
	static bool gammarand(double a, double b, int nSize, MTRand& mt, vector<double>& result);
	static double gammarand(double a, double b, MTRand& mt);
	static double chi2rand(double a, MTRand& mt);
	static bool betarand(double a, double b, int nSize, MTRand& mt, vector<double>& result);
	static double betarand(double a, double b, MTRand& mt);
	double betapdf(double x, double a, double b,int logspace);
	static unsigned int binorand(int n, double p, MTRand& mt);

	static double gammarand_int(unsigned int a,MTRand& mt);
	static unsigned int poissonrand(double mu, MTRand& mt);
	static unsigned int negative_binomial_rand(double p, double n, MTRand& mt);


	static double log_sum(double a, double b);
	static double log_gamma_rand(double shape, MTRand& mt);
	static void multinomialrand (unsigned int K, unsigned int N, double *p, unsigned int *n, MTRand& mt);
	static int discreterand(int K, double *p,MTRand& mt);
	static int discreterand_norm(int K, double *p, double norm, MTRand& mt);
};
