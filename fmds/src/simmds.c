//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

// get vector index from row-major organized (lower-triangular) matrices
#define IJ2K( n, i, j ) ( j * n + i )
#define IJ2L( n, i, j ) ( i > j ? j * ( n - 1 ) - j * ( j + 1 ) / 2 + i - 1 : i * ( n - 1 ) - i * ( i + 1 ) / 2 + j - 1 )
#define interval( d, dmin, dmax ) ( d < dmin ? dmin : d > dmax ? dmax : 0.5 * ( dmin + dmax ) )

void Csimmds1( int* rn, double* rdist, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
// basic multidimensional scaling on a lower-triangular matrix
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 0; epoch < NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds1

void Csimmds2( int* rn, double* rdelta, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1, t = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++, t++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds2

void Csimmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds3

void Csimfxdmds1( int* rn, double* rdist, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
// basic multidimensional scaling on a lower-triangular matrix
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimfxdmds1

void Csimfxdmds2( int* rn, double* rdelta, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds2

void Csimfxdmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds3

void Csimmds1local( int* rn, double* rdist, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds1local

void Csimmds2local( int* rn, double* rdelta, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds2local

void Csimmds3local( int* rn, int* rm, double* rx, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds3local

void Csimfxdmds1local( int* rn, double* rdist, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimfxdmds1local

void Csimfxdmds2local( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimfxdmds2local

void Csimfxdmds3local( int* rn, int* rm, double* rx, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimfxdmds3local

void Csimmds2interval( int* rn, double* rdelta, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds2interval

void Csimfxdmds2interval( int* rn, double* rdelta, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimfxdmds2interval

void Csimmds2localinterval( int* rn, double* rdelta, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimmds2localinterval

void Csimfxdmds2localinterval( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
      }
    }
  }
} // Csimfxdmds2localinterval

void Csimlinmds2( int* rn, double* rdelta, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // set up linear transformation
  double xmin = DBL_MAX;
  for ( size_t i = 1, k = 1; i < n; i++, k++ ) for ( size_t j = 0; j < n; j++, k++ ) if ( pdelta[k] < xmin ) xmin = pdelta[k];
  double cura = 0.0;
  double curb = 1.0;

  printscalar( "current a", cura );
  printscalar( "current b", curb );

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) {
      double wsumx = 0.0;
      double wsumy = 0.0;
      double wssqx = 0.0;
      double cross = 0.0;
      
      for ( size_t i = 0; i < n; i++ ) {

        const size_t idx1 = i;
        size_t idx2 = nextsize_t() % n;
        while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
        size_t idx3 = nextsize_t() % n;
        while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
        const size_t idx1p = idx1 * p;
        const size_t idx2p = idx2 * p;
        const size_t idx3p = idx3 * p;

        const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
        const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
        const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
        const double delta12 = cura + curb * pdelta[IJ2K( n, idx1, idx2 )];
        const double delta13 = cura + curb * pdelta[IJ2K( n, idx3, idx1 )];
        const double delta23 = cura + curb * pdelta[IJ2K( n, idx2, idx3 )];
      
        wsumx += ( delta12 - xmin ) + ( delta13 - xmin ) + ( delta23 - xmin );
        wsumy += d12 + d13 + d23;
        wssqx += ( delta12 - xmin ) * ( delta12 - xmin ) + ( delta13 - xmin ) * ( delta13 - xmin ) + ( delta23 - xmin ) * ( delta23 - xmin );
        cross += d12 * ( delta12 - xmin ) + d13 * ( delta13 - xmin ) + d23 * ( delta23 - xmin );

        const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
        const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
        const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );

        for ( size_t k = 0; k < p; k++ ) {
          const double z1 = pz[idx1p + k];
          const double z2 = pz[idx2p + k];
          const double z3 = pz[idx3p + k];
          pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
          pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
          pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
        }
      }

      const double sumw = ( double )( 3 * n );
      const double work = wssqx * sumw - wsumx * wsumx;
      double newb = ( isnotzero( work ) ? ( cross * sumw - wsumx * wsumy ) / work : 0.0 );
      if ( newb < 0.0 ) newb = 0.0;
      double newa = ( wsumy - newb * wsumx ) / sumw;
      if ( newa < 0.0 ) { 
        newa = 0.0;
        newb = cross / wssqx;
        if ( newb < 0.0 ) newb = 0.0;
      }
      newa -= newb * xmin;

      cura = ceta * cura + eta * newa;
      curb = ceta * curb + eta * newb;

      printscalar( "current a", cura );
      printscalar( "current b", curb );
    
    }
  }
} // Csimlinmds2

void Csimwgtmds1( int* rn, double* rdist, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx3, idx1 )] ) && iszero( pw[IJ2L( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;

      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double w12 = pw[IJ2L( n, idx1, idx2 )];
      const double w13 = pw[IJ2L( n, idx1, idx3 )];
      const double w23 = pw[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtdmds1

void Csimwgtmds2( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;

      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtmds2

void Csimwgtmds3( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double w12 = pw[idx1] * pw[idx2];
      const double w13 = pw[idx1] * pw[idx3];
      const double w23 = pw[idx2] * pw[idx3];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( isnotzero( w12) && isnotzero( w13 ) ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( isnotzero( w12) && isnotzero( w23 ) ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( isnotzero( w13) && isnotzero( w23 ) ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtmds3

void Csimfxdwgtmds1( int* rn, double* rdist, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx3, idx1 )] ) && iszero( pw[IJ2L( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;

      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double w12 = pw[IJ2L( n, idx1, idx2 )];
      const double w13 = pw[IJ2L( n, idx1, idx3 )];
      const double w23 = pw[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtdmds1

void Csimfxdwgtmds2( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;
      
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtmds2

void Csimfxdwgtmds3( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double w12 = pw[idx1] * pw[idx2];
      const double w13 = pw[idx1] * pw[idx3];
      const double w23 = pw[idx2] * pw[idx3];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( isnotzero( w12) && isnotzero( w13 ) ) if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( isnotzero( w12) && isnotzero( w23 ) ) if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( isnotzero( w13) && isnotzero( w23 ) ) if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtmds3

void Csimwgtmds1local( int* rn, double* rdist, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx3, idx1 )] ) && iszero( pw[IJ2L( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;

      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double w12 = pw[IJ2L( n, idx1, idx2 )];
      const double w13 = pw[IJ2L( n, idx1, idx3 )];
      const double w23 = pw[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtmds1local

void Csimwgtmds2local( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;

      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtmds2local

void Csimwgtmds3local( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double w12 = pw[idx1] * pw[idx2];
      const double w13 = pw[idx1] * pw[idx3];
      const double w23 = pw[idx2] * pw[idx3];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( isnotzero( w12) && isnotzero( w13 ) ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( isnotzero( w12) && isnotzero( w23 ) ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( isnotzero( w13) && isnotzero( w23 ) ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtmds3local

void Csimfxdwgtmds1local( int* rn, double* rdist, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdist = &rdist[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2L( n, idx3, idx1 )] ) && iszero( pw[IJ2L( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;

      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdist[IJ2L( n, idx1, idx2 )];
      const double delta13 = pdist[IJ2L( n, idx1, idx3 )];
      const double delta23 = pdist[IJ2L( n, idx2, idx3 )];
      const double w12 = pw[IJ2L( n, idx1, idx2 )];
      const double w13 = pw[IJ2L( n, idx1, idx3 )];
      const double w23 = pw[IJ2L( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtmds1local

void Csimfxdwgtmds2local( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;
      
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = pdelta[IJ2K( n, idx1, idx2 )];
      const double delta13 = pdelta[IJ2K( n, idx3, idx1 )];
      const double delta23 = pdelta[IJ2K( n, idx2, idx3 )];
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtmds2local

void Csimfxdwgtmds3local( int* rn, int* rm, double* rx, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( idx2 == idx1 ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = nextsize_t() % n;
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double w12 = pw[idx1] * pw[idx2];
      const double w13 = pw[idx1] * pw[idx3];
      const double w23 = pw[idx2] * pw[idx3];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( isnotzero( w12) && isnotzero( w13 ) ) if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( isnotzero( w12) && isnotzero( w23 ) ) if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( isnotzero( w13) && isnotzero( w23 ) ) if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtmds3local

void Csimwgtmds2interval( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;
      
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtmds2interval

void Csimfxdwgtmds2interval( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;

      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( pfz[idx2p + k] == 0 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( pfz[idx3p + k] == 0 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtmds2interval

void Csimwgtmds2localinterval( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;
      
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimwgtmds2localinterval

void Csimfxdwgtmds2localinterval( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rboundary, int* rnepochs, double* rminrate, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const double boundary = *rboundary;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];
  double* __restrict pz = &rz[0];
  int* __restrict pfz = &rfz[0];

  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;

  // start main loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {

    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;

    // start steps loop
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < n; i++ ) {

      const size_t idx1 = i;
      size_t idx2 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx1, idx2 )] ) ) idx2 = nextsize_t() % n;
      size_t idx3 = nextsize_t() % n;
      while ( iszero( pw[IJ2K( n, idx3, idx1 )] ) && iszero( pw[IJ2K( n, idx2, idx3 )] ) ) idx3 = nextsize_t() % n;
      
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;

      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = interval( d12, pdelta[IJ2K( n, idx2, idx1 )], pdelta[IJ2K( n, idx1, idx2 )] );
      const double delta13 = interval( d13, pdelta[IJ2K( n, idx3, idx1 )], pdelta[IJ2K( n, idx3, idx1 )] );
      const double delta23 = interval( d23, pdelta[IJ2K( n, idx3, idx2 )], pdelta[IJ2K( n, idx2, idx3 )] );
      const double w12 = pw[IJ2K( n, idx1, idx2 )];
      const double w13 = pw[IJ2K( n, idx3, idx1 )];
      const double w23 = pw[IJ2K( n, idx2, idx3 )];
      const double b12 = ( d12 < EPS ? 0.0 : w12 * delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : w13 * delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : w23 * delta23 / d23 );

      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        if ( pfz[idx1p + k] == 0 ) if ( delta12 <= boundary || d12 < delta12 ) pz[idx1p + k] = ceta * z1 + eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + w12 * z2 + w13 * z3 ) / ( w12 + w13 );
        if ( pfz[idx2p + k] == 0 ) if ( delta13 <= boundary || d13 < delta13 ) pz[idx2p + k] = ceta * z2 + eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + w12 * z1 + w23 * z3 ) / ( w12 + w23 );
        if ( pfz[idx3p + k] == 0 ) if ( delta23 <= boundary || d23 < delta23 ) pz[idx3p + k] = ceta * z3 + eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + w13 * z1 + w23 * z2 ) / ( w13 + w23 );
      }
    }
  }
} // Csimfxdwgtmds2localinterval

void Csimlmkmds3( int* rn, int* rm, double* rx, int* rp, double* rz, int* rnepochs, double* rminrate, int* rnlandmarks, int* rseed )
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  const size_t p = *rp;
  const size_t NEPOCHS = *rnepochs;
  const double MINRATE = *rminrate;
  const size_t nlm = *rnlandmarks;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pz = &rz[0];
  int* __restrict ilm = ( int* ) calloc( n, sizeof( int ) );
  for ( size_t i = 0; i < n; i++ ) ilm[i] = i;
  size_t* __restrict lm = ( size_t* ) calloc( nlm, sizeof( size_t ) );
  for ( size_t i = 0; i < nlm; i++ ) {
    size_t k = i + nextsize_t() % ( n - i );
    size_t j = ilm[i];
    lm[i] = ilm[k];
    ilm[k] = j;
  }
  for ( size_t i = 0; i < n; i++ ) ilm[i] = 0;
  for ( size_t i = 0; i < nlm; i++ ) ilm[lm[i]] = 1;
  
  // set constants
  const double EPS = DBL_EPSILON;
  const double MAXRATE = 0.5;
  const size_t NSTEPS = 16;
  double SCALE = 1.0 / ( MINRATE * ( double )( 3 * nlm * p * NSTEPS ) );

  // start landmarks loop
  for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {
    const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
    const double ceta = 1.0 - eta;
    double absdif = 0.0;
    for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t i = 0; i < nlm; i++ ) {
      const size_t idx1 = lm[i];
      size_t idx2 = lm[nextsize_t() % nlm];
      while ( idx2 == idx1 ) idx2 = lm[nextsize_t() % nlm];
      size_t idx3 = lm[nextsize_t() % nlm];
      while ( idx3 == idx1 || idx3 == idx2 ) idx3 = lm[nextsize_t() % nlm];
      const size_t idx1p = idx1 * p;
      const size_t idx2p = idx2 * p;
      const size_t idx3p = idx3 * p;
      const size_t idx1m = idx1 * m;
      const size_t idx2m = idx2 * m;
      const size_t idx3m = idx3 * m;
      const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
      const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
      const double d23 = fdist1( p, &pz[idx2p], &pz[idx3p] );
      const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
      const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
      const double delta23 = fdist1( m, &px[idx2m], &px[idx3m] );
      const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
      const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
      const double b23 = ( d23 < EPS ? 0.0 : delta23 / d23 );
      for ( size_t k = 0; k < p; k++ ) {
        const double z1 = pz[idx1p + k];
        const double z2 = pz[idx2p + k];
        const double z3 = pz[idx3p + k];
        pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
        pz[idx2p + k] = ceta * z2 + 0.5 * eta * ( b12 * ( z2 - z1 ) + b23 * ( z2 - z3 ) + z1 + z3 );
        pz[idx3p + k] = ceta * z3 + 0.5 * eta * ( b13 * ( z3 - z1 ) + b23 * ( z3 - z2 ) + z1 + z2 );
        absdif += fabs( z1 - pz[idx1p + k] ) + fabs( z2 - pz[idx2p + k] ) + fabs( z3 - pz[idx3p + k] );
      }
    }
    absdif *= SCALE;
    //printscalar( "", absdif );
    if ( absdif < MINRATE ) break;
  }

  // loop over out-of-samples
  SCALE = 1.0 / ( MINRATE * ( double )( nlm * p * NSTEPS ) );
  for ( size_t i = 0; i < n; i++ ) if ( ilm[i] == 0 ) {
    for ( size_t epoch = 1; epoch <= NEPOCHS; epoch++ ) {
      const double eta = 0.5 * ( MINRATE + MAXRATE ) + 0.5 * ( MAXRATE - MINRATE ) * cos( M_PI * epoch / NEPOCHS );
      const double ceta = 1.0 - eta;
        double absdif = 0.0;
      for( size_t step = 1; step <= NSTEPS; step++ ) for ( size_t j = 0; j < nlm; j++ ) {
        const size_t idx1 = i;
        const size_t idx2 = lm[j];
        size_t idx3 = lm[nextsize_t() % nlm];
        while ( idx3 == idx2 ) idx3 = lm[nextsize_t() % nlm];
        const size_t idx1p = idx1 * p;
        const size_t idx2p = idx2 * p;
        const size_t idx3p = idx3 * p;
        const size_t idx1m = idx1 * m;
        const size_t idx2m = idx2 * m;
        const size_t idx3m = idx3 * m;
        const double d12 = fdist1( p, &pz[idx1p], &pz[idx2p] );
        const double d13 = fdist1( p, &pz[idx1p], &pz[idx3p] );
        const double delta12 = fdist1( m, &px[idx1m], &px[idx2m] );
        const double delta13 = fdist1( m, &px[idx1m], &px[idx3m] );
        const double b12 = ( d12 < EPS ? 0.0 : delta12 / d12 );
        const double b13 = ( d13 < EPS ? 0.0 : delta13 / d13 );
        for ( size_t k = 0; k < p; k++ ) {
          const double z1 = pz[idx1p + k];
          const double z2 = pz[idx2p + k];
          const double z3 = pz[idx3p + k];
          pz[idx1p + k] = ceta * z1 + 0.5 * eta * ( b12 * ( z1 - z2 ) + b13 * ( z1 - z3 ) + z2 + z3 );
          absdif += fabs( z1 - pz[idx1p + k] );
        }
      }
      absdif *= SCALE;
      //printscalar( "", absdif );
      if ( absdif < MINRATE ) break;
    }
  }

  free( ilm );
  free( lm );

} // Csimlmkmds3

void simmds_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test simmds at ", dt );
  printstring( "", "==============================================================\n" );

  randomize( &seed );
 
  size_t n = 0;
  size_t m = 0; 
  size_t p = 2;
  size_t tm = setstarttime();

  // example with weights and fixed coordinates
  {
    size_t n = 100;
    size_t m = 2; 
    size_t p = 2;
    size_t tm = setstarttime();

    double** data = getmatrix( n, m, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= m; j++ ) data[i][j] = 10.0 * nextdouble();
    double** delta = getmatrix( n, n, 0.0 );
    euclidean1( n, m, data, delta );
    double** w = getmatrix( n, n, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) {
      const double mn = dsum( n, &delta[i][1], 1 ) / ( double )( n - 1 );
      for ( size_t j = 1; j <= n; j++ ) w[i][j] = ( data[i][j] < mn ? 1.0 : 0.0 );
    }
    double** z = getmatrix( n, p, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= p; j++ ) z[i][j] = 10.0 * nextdouble();
    double** d = getmatrix( n, n, 0.0 );
    int** fz = getimatrix( n, p, 0 );
    for ( size_t i = 1; i <= 10; i++ ) for ( size_t j = 1; j <= p; j++ ) fz[i][j] = 1;

    int intn = ( int )( n );
    int intp = ( int )( p );
    int intseed = ( int )( seed );
    int nepochs = 64;
    double minrate = 0.01;

    tm = setstarttime();
    Csimfxdwgtmds2( &intn, &delta[1][1], &w[1][1], &intp, &z[1][1], &fz[1][1], &nepochs, &minrate, &intseed );
    printscalar( "elapsed for Csimmds3", getelapsedtime( tm ) );

    freematrix( data );
    freematrix( delta );
    freematrix( w );
    freematrix( z );
    freematrix( d );
    freeimatrix( fz );
    //return;
  }

  // small example on tortula data to check everything out
  {
    double** data = readmatrix( "tortula.dat", &n, &m );
    double** w = getmatrix( n, n, 1.0 );
    dset( n, 0.0, &w[1][1], n + 1 );
    double** delta = getmatrix( n, n, 0.0 );
    euclidean1( n, m, data, delta );
    double** z = getmatrix( n, p, 0.0 );
    for ( size_t i = 1, k = 1; i <= n; i++ ) for ( size_t j = 1; j <= p; j++, k++ ) z[i][j] = nextdouble();
    double** d = getmatrix( n, n, 0.0 );

    int intn = ( int )( n );
    int intm = ( int )( m );
    int intp = ( int )( p );
    int intseed = ( int )( seed );
    int nepochs = 512;;
    int nlandmarks = 4;
    double minrate = 0.001;

    tm = setstarttime();
    Csimlmkmds3( &intn, &intm, &data[1][1], &intp, &z[1][1], &nepochs, &minrate, &nlandmarks, &intseed );
    printscalar( "elapsed for Csimlmkmds3", getelapsedtime( tm ) );
    euclidean1( n, p, z, d );
    printscalar( "stress = ", dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / dssq( n * n, &delta[1][1], 1 ) );  

    freematrix( data );
    freematrix( w );
    freematrix( delta );
    freematrix( d );
    freematrix( z );
    //return;
  }

  // large example of perfect data
  {
    size_t n = 1000;
    size_t m = 10; 
    size_t p = 2;
    size_t tm = setstarttime();

    double** data = getmatrix( n, m, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= m; j++ ) data[i][j] = 10.0 * nextdouble();
    double** delta = getmatrix( n, n, 0.0 );
    euclidean1( n, m, data, delta );
    double** z = getmatrix( n, p, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= p; j++ ) z[i][j] = 10.0 * nextdouble();
    double** d = getmatrix( n, n, 0.0 );

    int intn = ( int )( n );
    int intm = ( int )( m );
    int intp = ( int )( p );
    int intseed = ( int )( seed );
    int nepochs = 1024;
    int nlandmarks = 10;
    double minrate = 0.001;

    tm = setstarttime();
    Csimmds3( &intn, &intm, &data[1][1], &intp, &z[1][1], &nepochs, &minrate, &intseed );
    printscalar( "elapsed for Csimmds3", getelapsedtime( tm ) );
    euclidean1( n, p, z, d );
    printscalar( "stress", dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / dssq( n * n, &delta[1][1], 1 ) );  

    tm = setstarttime();
    Csimlmkmds3( &intn, &intm, &data[1][1], &intp, &z[1][1], &nepochs, &minrate, &nlandmarks, &intseed );
    printscalar( "elapsed for Csimlmkmds3", getelapsedtime( tm ) );
    euclidean1( n, p, z, d );
    printscalar( "stress = ", dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / dssq( n * n, &delta[1][1], 1 ) );  

    freematrix( data );
    freematrix( delta );
    freematrix( d );
    freematrix( z );
    //return;
  }

  // example with linear transformation
  {
    size_t n = 1000;
    size_t m = 10; 
    size_t p = 2;
    size_t tm = setstarttime();

    double** data = getmatrix( n, m, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= m; j++ ) data[i][j] = 10.0 * nextdouble();
    double** delta = getmatrix( n, n, 0.0 );
    euclidean1( n, m, data, delta );
    double** z = getmatrix( n, p, 0.0 );
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= p; j++ ) z[i][j] = 10.0 * nextdouble();
    double** d = getmatrix( n, n, 0.0 );

    int intn = ( int )( n );
    int intm = ( int )( m );
    int intp = ( int )( p );
    int intseed = ( int )( seed );
    int nepochs = 1024;
    double minrate = 0.001;

    tm = setstarttime();
    Csimmds3( &intn, &intm, &data[1][1], &intp, &z[1][1], &nepochs, &minrate, &intseed );
    printscalar( "elapsed for Csimmds3", getelapsedtime( tm ) );
    euclidean1( n, p, z, d );
    printscalar( "stress", dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / dssq( n * n, &delta[1][1], 1 ) );  

    freematrix( data );
    freematrix( delta );
    freematrix( d );
    freematrix( z );
    //return;
  }

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test simmds at ", dt );
  printstring( "", "==============================================================\n" );
}
