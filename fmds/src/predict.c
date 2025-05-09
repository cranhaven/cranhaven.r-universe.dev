//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

double predict( const size_t n, const size_t p, double** z, double* delta, double* w, double* x, double* d, const int level, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo )
// function predict performs out-of-sample multidimensional scaling, i.e., external unfolding
{
  // set constants
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* b = getvector( n, 0.0 );
  size_t* index = getvector_t( n, ( size_t )( 0 ) );
  size_t* tbl = getvector_t( n, ( size_t )( 0 ) );
  double* vd = getvector( n, 0.0 );
  double* vw = getvector( n, 1.0 );

  // set variables
  const double sumw = dsum( n, &w[1], 1 );

  // initial y: probability weighted average
  if ( iszero( dssq( p, &x[1], 1 ) ) ) {
    double* probs = getvector( n, 0.0 );
    pdist( n, delta, w, probs );
    for ( size_t k = 1; k <= p; k++ ) x[k] = ddot( n, &probs[1], 1, &z[1][k], p );
    freevector( probs );
  }

  // prepare ordinal transform
  size_t count = 0;
  size_t ntb = 1;
  if ( level == 3 ) {
    size_t last = n;
    for ( size_t i = 1; i <= n; i++ ) {
      if ( isnotzero( w[i] ) ) {
        count++;
        index[count] = i;
        vd[count] = delta[i];
      }
      else {
        index[last] = i;
        last--;
      }
    }
    dsort( count, vd, index );
    tbl[ntb] = 1;
    for ( size_t i = 2; i <= count; i++ ) {
      if ( isequal( vd[i], vd[i - 1] ) ) tbl[ntb]++;
      else {
        ntb++;
        tbl[ntb] = 1;
      }
    }
  }

  // administration
  for ( size_t i = 1; i <= n; i++ ) d[i] = fdist1( p, &z[i][1], &x[1] );
  double upper = dwsse( n, &delta[1], 1, &d[1], 1, &w[1], 1 );
  double lower = dwssq( n, &delta[1], 1, &w[1], 1 );
  double fold = ( lower < DBL_EPSILON ? 1.0 : upper / lower );
  double fhalf = 0.0;
  double fnew = 0.0;

  if ( echo == true ) echoprogress( 0, 1.0, 1.0, fold );

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // apply majorization constant to pd
    const double mconst = 1.0 / ( 1.0 - fold );
    dscal( n, mconst, &d[1], 1 );

    // optimal scale factor: none (0)
    if ( level == TRANSFORMATIONLEVEL.ABSOLUTE ) {

    }

    // optimal scale factor: ratio (1)
    else if ( level == TRANSFORMATIONLEVEL.RATIO ) {
      double wssq = dwssq( n, &delta[1], 1, &w[1], 1 );
      const double cross = dwdot( n, &delta[1], 1, &d[1], 1, &w[1], 1 );
      if ( iszero( wssq ) ) wssq = DBL_EPSILON;
      const double b = ( cross < 0.0 ? 0.0 : cross / wssq );
      if ( isnotzero( b ) ) dscal( n, b, &delta[1], 1 );
    }

    // optimal intercept: linear (2)
    else if ( level == TRANSFORMATIONLEVEL.LINEAR ) {
      double xmin = DBL_MAX;
      for ( size_t i = 1; i <= n; i++ ) if ( isnotzero( w[i] ) && delta[i] < xmin ) xmin = delta[i];
      for ( size_t i = 1; i <= n; i++ ) delta[i] -= xmin;
      const double wsumx = dwsum( n, &delta[1], 1, &w[1], 1 );
      const double wsumy = dwsum( n, &d[1], 1, &w[1], 1 );
      const double wssqx = dwssq( n, &delta[1], 1, &w[1], 1 );
      const double cross = dwdot( n, &d[1], 1, &delta[1], 1, &w[1], 1 );
      const double work = wssqx * sumw - wsumx * wsumx;
      double b = ( isnotzero( work ) ? ( cross * sumw - wsumx * wsumy ) / work : 0.0 );
      if ( b < 0.0 ) b = 0.0;
      double a = ( wsumy - b * wsumx ) / sumw;
      if ( a < 0.0 ) {
        a = 0.0;
        b = cross / wssqx;
        if ( b < 0.0 ) b = 0.0;
      }
      for ( size_t i = 1; i <= n; i++ ) delta[i] += xmin;
      a -= b * xmin;
      if ( isnotzero( b ) ) for ( size_t i = 1; i <= n; i++ ) delta[i] = a + b * delta[i];
    }

    // optimal steps: ordinal (3)
    else if ( level == TRANSFORMATIONLEVEL.ORDINAL ) {
      for ( size_t k = 1; k <= count; k++ ) {
        vd[k] = d[index[k]];
        vw[k] = w[index[k]];
      }
      for ( size_t b = 1, k = 0; b <= ntb; b++ ) {
        if ( tbl[b] > 1 ) dsort( tbl[b], &vd[k], &index[k] );
        for ( size_t j = 1; j <= tbl[b]; j++ ) vw[k+j] = w[index[k+j]];
        k += tbl[b];
      }
      monotone( count, vd, vw );
      for ( size_t k = 1; k <= count; k++ ) delta[index[k]] = vd[k];
    }

    // intermediate results
    if ( echo == true ) {
      upper = dwsse( n, &delta[1], 1, &d[1], 1, &w[1], 1 );
      lower = dwssq( n, &delta[1], 1, &w[1], 1 );
      fhalf = ( lower < DBL_EPSILON ? 1.0 : upper / lower );
    }

    // update y
    for ( size_t i = 1; i <= n; i++ ) b[i] = ( d[i] > TINY ? w[i] * delta[i] / d[i] : 0.0 );
    const double pi = dsum( n, &b[1], 1 );
    for ( size_t k = 1; k <= p; k++ ) {
      const double bz = ddot( n, &b[1], 1, &z[1][k], p );
      const double ztilde = pi * x[k] - bz;
      const double ztildeplus = ztilde + ddot( n, &w[1], 1, &z[1][k], p );
      x[k] = ztildeplus / sumw;
    }

    // administration
    for ( size_t i = 1; i <= n; i++ ) d[i] = fdist1( p, &z[i][1], &x[1] );
    upper = dwsse( n, &delta[1], 1, &d[1], 1, &w[1], 1 );
    lower = dwssq( n, &delta[1], 1, &w[1], 1 );
    fnew = ( lower < DBL_EPSILON ? 1.0 : upper / lower );

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fhalf, fnew );

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    if ( fnew < FCRIT ) break;
    if ( 2.0 * ( *lastdif ) <= FCRIT * ( fold + fnew + EPS ) ) break;
    fold = fnew;
  }
  ( *lastiter ) += iter;

  // de-allocate memory
  freevector( b );
  freevector_t( index );
  freevector_t( tbl );
  freevector( vw );
  freevector( vd );

  return( fnew );
} // predict

void CRpredict( int* rn, int* rp, double* rz, double* rdelta, double* rw, double* rx, double* rd, int* rlevel, int* rmaxiter, double* rfdif, double* rfvalue, int* recho )
// function CRpredictdistances performs out-of-sample multidimensional scaling, i.e., external unfolding
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const int level = *rlevel;
  const size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const bool echo = ( *recho ) != 0;

  // allocate one-based memory
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  double* delta = getvector( n, 0.0 );
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) delta[i] = rdelta[k];
  double* w = getvector( n, 0.0 );
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) w[i] = rw[k];
  double* x = getvector( p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++, k++ ) x[j] = rx[k];
  double* d = getvector( n, 0.0 );
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) d[i] = rd[k];

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  const double fvalue = predict( n, p, z, delta, w, x, d, level, MAXITER, FCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) rdelta[k] = delta[i];
  for ( size_t j = 1, k = 0; j <= p; j++, k++ ) rx[k] = x[j];
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) rd[k] = d[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate one-based memory
  freematrix( z );
  freevector( delta );
  freevector( w );
  freevector( x );
  freevector( d );

} // CRpredict
