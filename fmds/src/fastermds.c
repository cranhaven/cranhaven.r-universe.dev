//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

#define IJ2K( n, i, j ) ( j * n + i )
#define IJ2L( n, i, j ) ( i > j ? ( 2 * j * n - j * j + 2 * i - 3 * j - 2 ) / 2 : ( 2 * i * n - i * i + 2 * j - 3 * i - 2 ) / 2 )
#define U2L( n, k ) ( ( k % n ) * n + k / n )
#define L2U( n, k ) ( ( k % n ) * n + k / n )

void Cfasterstress( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnsamples, int* rsamplesize, int* rseed, double* stress, double* se )
{
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const size_t NSAMPLES = *rnsamples;
  const size_t SAMPLESIZE = *rsamplesize;
  double* __restrict px = &rx[0];
  double* __restrict pz = &rz[0];
  double* v = getvector( NSAMPLES, 0.0 );
  long xseed = ( long )( *rseed );
  randomize( &xseed );
  double meanstress = 0.0;
  for ( size_t sample = 1; sample <= NSAMPLES; sample++ ) {
    double upper = 0.0;
    double lower = 0.0;
    for ( size_t observation = 1; observation <= SAMPLESIZE; observation++ ) {
      const size_t idx1 = ( size_t )( n * nextdouble( ) );
      const size_t idx2 = ( size_t )( n * nextdouble( ) );
      const double delta = fdist1( m, &px[idx1*m], &px[idx2*m] );
      const double d = fdist1( p, &pz[idx1*p], &pz[idx2*p] );
      lower += delta * delta;
      const double work = delta - d;
      upper += work * work;
    }
    meanstress += v[sample] = upper / lower;
  }
  ( *stress ) = meanstress / ( double )( NSAMPLES );
  ( *se ) = stddev( NSAMPLES, &v[1], 1 );
  freevector( v );
} // Cfasterstress

void Cfastermds( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NSTEPS = *rnsteps;
  const double RCRIT = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSUBSETS = n / 3;
  const double ALPHA = pow( RCRIT / MAXRATE, 1.0 / ( double )( NSTEPS ) );

  // start main loop
  double mu = MAXRATE;
  for ( size_t iter = 1; iter <= NSTEPS; iter++ ) {
    const double cmu = 1.0 - mu;

    // start subsets loop
    for( size_t subs = 1; subs <= NSUBSETS; subs++ ) {

      const size_t idx1 = nextsize_t() % n;
      const size_t idx2 = nextsize_t() % n;
      const size_t idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx1, idx3 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = cmu * z1 + 0.5 * mu * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        pz[idx2p + k] = cmu * z2 + 0.5 * mu * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        pz[idx3p + k] = cmu * z3 + 0.5 * mu * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }

    // exponentially decrease mu by alpha
    mu *= ALPHA;
  }

} // Cfastermds

void Cfastermdsneg( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NSTEPS = *rnsteps;
  const double RCRIT = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double MAXRATE = 0.5;
  const size_t NSUBSETS = n / 3;
  const double ALPHA = pow( RCRIT / MAXRATE, 1.0 / ( double )( NSTEPS ) );

  // start main loop
  double mu = MAXRATE;
  for ( size_t iter = 1; iter <= NSTEPS; iter++ ) {
    const double cmu = 1.0 - mu;

    // start subsets loop
    for( size_t subs = 1; subs <= NSUBSETS; subs++ ) {

      const size_t idx1 = nextsize_t() % n;
      const size_t idx2 = nextsize_t() % n;
      const size_t idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx1, idx3 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double w12 = ( delta12 < 0.0 ? ( d12 < EPS ? ( EPS + delta12 * delta12 ) / EPS : ( d12 + fabs( delta12 ) ) / d12 ) : 1.0 );
      const double w13 = ( delta13 < 0.0 ? ( d13 < EPS ? ( EPS + delta13 * delta13 ) / EPS : ( d13 + fabs( delta13 ) ) / d13 ) : 1.0 );
      const double w23 = ( delta23 < 0.0 ? ( d23 < EPS ? ( EPS + delta23 * delta23 ) / EPS : ( d23 + fabs( delta23 ) ) / d23 ) : 1.0 );
      const double b12 = ( delta12 < 0.0 || d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( delta13 < 0.0 || d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( delta23 < 0.0 || d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = cmu * z1 + mu * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 ) / ( w13 + w23 );
        pz[idx2p + k] = cmu * z2 + mu * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 ) / ( w12 + w23 );
        pz[idx3p + k] = cmu * z3 + mu * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 ) / ( w12 + w13 );
      }
    }

    // exponentially decrease mu by alpha
    mu *= ALPHA;
  }

} // Cfastermdsneg

void Cfasterwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NSTEPS = *rnsteps;
  const double RCRIT = *rminrate;
  long xseed = ( long )( *rseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double MAXRATE = 0.5;
  const size_t NSUBSETS = n / 3;
  const double ALPHA = pow( RCRIT / MAXRATE, 1.0 / ( double )( NSTEPS ) );

  randomize( &xseed );

  // start main loop
  double mu = MAXRATE;
  for ( size_t iter = 1; iter <= NSTEPS; iter++ ) {
    const double cmu = 1.0 - mu;

    // start subsets loop
    for( size_t subs = 1; subs <= NSUBSETS; subs++ ) {

      const size_t idx1 = nextsize_t() % n;
      const size_t idx2 = nextsize_t() % n;
      const size_t idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx1, idx3 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx1, idx3 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = cmu * z1 + mu * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 ) / ( w13 + w23 );
        pz[idx2p + k] = cmu * z2 + mu * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 ) / ( w12 + w23 );
        pz[idx3p + k] = cmu * z3 + mu * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 ) / ( w12 + w13 );
      }
    }

    // exponentially decrease mu by alpha
    mu *= ALPHA;
  }

} // Cfasterwgtmds

void Cfasterfxdmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, int* rnsteps, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NSTEPS = *rnsteps;
  const double RCRIT = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double MAXRATE = 0.5;
  const size_t NSUBSETS = n / 3;
  const double ALPHA = pow( RCRIT / MAXRATE, 1.0 / ( double )( NSTEPS ) );

  // start main loop
  double mu = MAXRATE;
  for ( size_t iter = 1; iter <= NSTEPS; iter++ ) {
    const double cmu = 1.0 - mu;

    // start subsets loop
    for( size_t subs = 1; subs <= NSUBSETS; subs++ ) {

      const size_t idx1 = nextsize_t() % n;
      const size_t idx2 = nextsize_t() % n;
      const size_t idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx1, idx3 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = cmu * z1 + 0.5 * mu * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = cmu * z2 + 0.5 * mu * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = cmu * z3 + 0.5 * mu * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }

    // exponentially decrease mu by alpha
    mu *= ALPHA;
  }

} // Cfasterfxdmds

void Cfasterordmds( int* rn, double* rdelta, int* rp, double* rz, int* rnsteps, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NSTEPS = *rnsteps;
  const double RCRIT = *rminrate;
  long xseed = ( long )( *rseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const size_t NSUBSETS = n / 2;
  double MAXRATE = 0.5;
  const double ALPHA = pow( RCRIT / MAXRATE, 1.0 / ( double )( NSTEPS ) );

  // sorted index lower triangle
  const size_t nn = n * ( n - 1 ) / 2;
  size_t* __restrict pidx = ( size_t* ) calloc( nn, sizeof( size_t ) );
  double* __restrict pd = ( double* ) calloc( nn, sizeof( double ) );
  double* __restrict pw = ( double* ) calloc( nn, sizeof( double ) );
  for ( size_t j = 0, k = 0, t = 0; j < n; j++ ) {
    for ( size_t i = 0; i < n; i++, k++ ) if ( i > j ) {
      pidx[t] = k;
      pd[t] = pdelta[k];
      t++;
    }
  }
  dsort( nn, &pd[-1], &pidx[-1] );

  randomize( &xseed );
  
  // transformation loop
  for ( size_t outer = 1; outer <= ( NSTEPS ); outer++ ) {  // needed for proper stress
    dset( nn, 0.0, pd, 1 );
    dset( nn, 0.0, pw, 1 );

    // configuration loop
    double mu = MAXRATE;
    for ( size_t iter = 1; iter <= sqrt( NSTEPS ); iter++ ) {  // not needed but for speed
      const double cmu = 1.0 - mu;

      // start subsets loop
      for( size_t subs = 0; subs < NSUBSETS; subs++ ) {
  
        // first and second indices
        size_t idx1 = duniform( 0, n - 1 );
        size_t idx2 = duniform( 0, n - 1 );
        while ( idx1 == idx2 ) idx2 = duniform( 0, n - 1 );
        if ( idx1 < idx2 ) { const size_t itmp = idx2; idx2 = idx1; idx1 = itmp; }
        const size_t idx1p = idx1 * p;
        const size_t idx2p = idx2 * p;
  
        // update coordinates
        const double d = fdist1( p, &pz[idx1p], &pz[idx2p] );
        const double delta = pdelta[IJ2K( n, idx1, idx2 )];
        const double b = ( d < TINY ? 0.0 : delta / d );
        for ( size_t k = 0; k < p; k++ ) {
          const double z1 = pz[idx1p + k];
          const double z2 = pz[idx2p + k];
          const double t = b * ( z1 - z2 );
          pz[idx1p + k] = cmu * z1 + mu * ( t + z2 );
          pz[idx2p + k] = cmu * z2 + mu * ( z1 - t );
        }

        // vectors for monotone
        const size_t l = pidx[IJ2L( n, idx1, idx2 )];
        pd[l] = d;
        pw[l] = 1.0;
      }
  
      // exponentially decrease mu by ALPHA
      mu *= ALPHA;
    }

    monotone( nn, &pd[-1], &pw[-1]);  
    for ( size_t i = 0; i < nn; i++ ) {
      const size_t k = pidx[i];
      pdelta[k] = pdelta[L2U( n, k )] = pd[i];
    }

    // exponentially decrease MAXRATE by ALPHA
    MAXRATE *= ALPHA;
  }

  size_t wrong = 0;
  for ( size_t t = 1; t < n; t++ ) {
  const size_t k = pidx[t];
    const size_t km1 = pidx[t-1];
    if ( pdelta[k] < pdelta[km1] ) wrong++;       
  }
  printscalar_t( "wrong", wrong );

  // de-allocate memory
  free( pidx );
  free( pd );
  free( pw );

} // Cfasterordmds

void fastermds_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test fastermds at ", dt );
  printstring( "\n", "==============================================================\n" );

  randomize( &seed );
 
  int n = 20;
  int p = 2;
  int nsteps = 1024;
  double minrate = 0.0000001;
  int iseed = ( int )( seed );

  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 2.0 * nextdouble();
  double** w = getmatrix( n, n, 1.0 );
  for ( size_t i = 1; i <= n; i++ ) w[i][i] = 0.0;
  double** z = getmatrix( n, p, 0.0 );
  int** fz = getimatrix( n, p, 0 );
  double** d = getmatrix( n, n, 0.0 );

  pcoa( n, delta, p, 0.0, z );
  size_t tm = setstarttime();
  Cfastermds( &n, &delta[1][1], &p, &z[1][1], &nsteps, &minrate, &iseed );
  printscalar( "elapsed for Cfastermds", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  pcoa( n, delta, p, 0.0, z );
  tm = setstarttime();
  Cfastermdsneg( &n, &delta[1][1], &p, &z[1][1], &nsteps, &minrate, &iseed );
  printscalar( "elapsed for Cfastermdsneg", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  pcoa( n, delta, p, 0.0, z );
  tm = setstarttime();
  Cfasterfxdmds( &n, &delta[1][1], &p, &z[1][1], &fz[1][1], &nsteps, &minrate, &iseed );
  printscalar( "elapsed for Cfasterfxdmds", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  pcoa( n, delta, p, 0.0, z );
  tm = setstarttime();
  Cfasterwgtmds( &n, &delta[1][1], &w[1][1], &p, &z[1][1], &nsteps, &minrate, &iseed );
  printscalar( "elapsed for Cfasterwgtmds", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  pcoa( n, delta, p, 0.0, z );
  tm = setstarttime();
  //Cfasterordmds( &n, &delta[1][1], &p, &z[1][1], &nsteps, &minrate, &iseed );
  printscalar( "elapsed for Cfasterordmds", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );
 
  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test fastermds at ", dt );
  printstring( "\n", "==============================================================\n" );
} // unittest
