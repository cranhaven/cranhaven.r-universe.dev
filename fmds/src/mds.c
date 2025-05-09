//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

double mds( const size_t n, double** delta, const size_t p, double** z, double** d, const bool anchor, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function mds() performs multidimensional scaling.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* s = getvector( p, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );
  double** gamma = getmatrix( n, n, 0.0 );

  // determine symmetry
  bool symmetric = true;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( isnotequal( delta[i][j], delta[j][i] ) ) {
    symmetric = false;
    break;
  }
  
  // initialization
  center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  double h = ( double )( n - 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  dcopy( n * n, &delta[1][1], 1, &gamma[1][1], 1 );
  double scale = dssq( n * n, &gamma[1][1], 1 );
  double fold = dsse( n * n, &gamma[1][1], 1, &d[1][1], 1 ) / scale;
  double fhalf = 0.0;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  if ( fold > TINY ) for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute transformation update
    if ( anchor == true ) nnintercept( symmetric, n, delta, d, 1.0 / ( 1.0 - fold ), NULL, gamma );

    // intermediate results
    if ( echo == true ) {
      scale = dssq( n * n, &gamma[1][1], 1 );
      fhalf = dsse( n * n, &gamma[1][1], 1, &d[1][1], 1 ) / scale;
    }

    // compute update for z
    for ( size_t k = 1; k <= p; k++ ) s[k] = dsum( n, &z[1][k], p );
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) {
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        z[i][k] = ( t + s[k] - zold[i][k] ) / h;
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    scale = dssq( n * n, &gamma[1][1], 1 );
    fnew = dsse( n * n, &gamma[1][1], 1, &d[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fhalf, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;

  // return gamma (transformed delta) in delta
  dcopy( n * n, &gamma[1][1], 1, &delta[1][1], 1 );

  // de-allocate memory
  freevector( imb );
  freevector( s );
  freematrix( zold );
  freematrix( gamma );

  return( fnew );
} // mds

void Cmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* ranchor, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cmds() performs multidimensional scaling.
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  double** d = getmatrix( n, n, 0.0 );
  const bool anchor = ( *ranchor ) != 0;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  const bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = mds( n, delta, p, z, d, anchor, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rdelta[k] = delta[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( z );
  freematrix( d );

} // Cmds

double mdsneg( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function mdsneg() performs multidimensional scaling allowing negative dissimilarities.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* imw = getvector( n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
  }

  // initialization
  center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  const double scale = dssq( n * n, &delta[1][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( delta[i][j] < 0.0 || d[i][j] < DISCRIT ? 0.0 : -delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      double h = 0.0;
      for ( size_t j = 1; j <= n; j++ ) {
        if ( delta[i][j] < 0.0 ) {
          const double work = fabs( delta[i][j] );
          if ( d[i][j] < DISCRIT ) h += imw[j] = ( EPSCRIT + work * work ) / EPSCRIT;
          else h += imw[j] = ( d[i][j] + work ) / d[i][j];
        }
        else h += imw[j] = 1.0;
      }
      for ( size_t k = 1; k <= p; k++ ) {
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        const double s = ddot( n, &imw[1], 1, &zold[1][k], p );
        z[i][k] = ( t + s ) / h;
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;

  // de-allocate memory
  freevector( imb );
  freevector( imw );
  freematrix( zold );

  return( fnew );
} // mdsneg

void Cmdsneg( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cmdsneg() performs multidimensional scaling allowing negative dissimilarities.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  double** d = getmatrix( n, n, 0.0 );
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = mdsneg( n, delta, p, z, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( z );
  freematrix( d );

} // Cmdsneg

double wgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function wgtmds() performs weighted multidimensional scaling.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* h = getvector( n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta and w
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) {
      if ( iszero( w[i][j] ) ) {
        if ( iszero( w[j][i] ) ) delta[i][j] = delta[j][i] = w[i][j] = w[j][i] = 0.0;
        else {
          delta[i][j] = delta[j][i];
          w[i][j] = w[j][i] *= 0.5;
        }
      }
      else {
        if ( iszero( w[j][i] ) ) {
          delta[j][i] = delta[i][j];
          w[j][i] = w[i][j] *= 0.5;
        }
        else {
          delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
          w[i][j] = w[j][i] = 0.5 * ( w[i][j] + w[j][i] );
        }
      }
    }
  }

  // initialization
  center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  const double scale = dwssq( n * n, &delta[1][1], 1, &w[1][1], 1 );
  for ( size_t i = 1; i <= n; i++ ) h[i] = dsum( n, &w[i][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -w[i][j] * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) {
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        const double s = ddot( n, &w[i][1], 1, &zold[1][k], p );
        z[i][k] = ( t + s ) / h[i];
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // rotate to principal axes of z
//  weightedrotate( n, p, z, h );
 
  // de-allocate memory
  freevector( imb );
  freevector( h );
  freematrix( zold );

  return( fnew );
} // wgtmds

void Cwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cwgtmds() performs weighted multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = wgtmds( n, delta, w, p, z, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freematrix( d );

} // Cwgtmds

double wgtmdsneg( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function wgtmdsneg() performs weighted multidimensional scaling allowing negative dissimilarities.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* imw = getvector( n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta and w
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) {
      if ( iszero( w[i][j] ) ) {
        if ( iszero( w[j][i] ) ) delta[i][j] = delta[j][i] = w[i][j] = w[j][i] = 0.0;
        else {
          delta[i][j] = delta[j][i];
          w[i][j] = w[j][i] *= 0.5;
        }
      }
      else {
        if ( iszero( w[j][i] ) ) {
          delta[j][i] = delta[i][j];
          w[j][i] = w[i][j] *= 0.5;
        }
        else {
          delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
          w[i][j] = w[j][i] = 0.5 * ( w[i][j] + w[j][i] );
        }
      }
    }
  }

  // initialization
  center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  const double scale = dwssq( n * n, &delta[1][1], 1, &w[1][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( delta[i][j] < 0.0 || d[i][j] < DISCRIT ? 0.0 : -w[i][j] * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      double h = 0.0;
      for ( size_t j = 1; j <= n; j++ ) {
        if ( delta[i][j] < 0.0 ) {
          const double work = fabs( delta[i][j] );
          if ( d[i][j] < DISCRIT ) h += imw[j] = w[i][j] * ( EPSCRIT + work * work ) / EPSCRIT;
          else h += imw[j] = w[i][j] * ( d[i][j] + work ) / d[i][j];
        }
        else h += imw[j] = w[i][j];
      }
      for ( size_t k = 1; k <= p; k++ ) {
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        const double s =ddot( n, &imw[1], 1, &zold[1][k], p );
        z[i][k] = ( t + s ) / h;
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // rotate to principal axes of z
//  for ( size_t i = 1; i <= n; i++ ) imw[i] = dsum( n, &w[i][1], 1);
//  weightedrotate( n, p, z, imw );
 
  // de-allocate memory
  freevector( imb );
  freevector( imw );
  freematrix( zold );

  return( fnew );
} // wgtmdsneg

void Cwgtmdsneg( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cwgtmdsneg() performs weighted multidimensional scaling allowing negative dissimilarities.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = wgtmdsneg( n, delta, w, p, z, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freematrix( d );

} // Cwgtmdsneg

double fxdmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function fxdmds() performs multidimensional scaling with fixed coordinates.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* s = getvector( p, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
  }

  // initialization
  int nfz = 0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) nfz += fz[i][k];
  if ( nfz == 0 ) center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  const double scale = dssq( n * n, &delta[1][1], 1 );
  double h = ( double )( n - 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  if ( fold > TINY ) for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t k = 1; k <= p; k++ ) s[k] = dsum( n, &z[1][k], 1 );
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) if ( fz[i][k] == 0 ) {  // update free coordinate
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        z[i][k] = ( t + s[k] - zold[i][k] ) / h;
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;

  // de-allocate memory
  freevector( imb );
  freevector( s );
  freematrix( zold );

  return( fnew );
} // fxdmds

void Cfxdmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cfxdmds() performs multidimensional scaling with fixed coordinates.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  int** fz = getimatrix( n, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) fz[i][j] = rfz[k];
  double** d = getmatrix( n, n, 0.0 );
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = fxdmds( n, delta, p, z, fz, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );

} // Cfxdmds

double fxdmdsneg( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function fxdmdsneg() performs multidimensional scaling allowing negative dissimilarities.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* imw = getvector( n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
  }

  // initialization
  int nfz = 0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) nfz += fz[i][k];
  if ( nfz == 0 ) center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  const double scale = dssq( n * n, &delta[1][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( delta[i][j] < 0.0 || d[i][j] < DISCRIT ? 0.0 : -delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      double h = 0.0;
      for ( size_t j = 1; j <= n; j++ ) {
        if ( delta[i][j] < 0.0 ) {
          const double work = fabs( delta[i][j] );
          if ( d[i][j] < DISCRIT ) h += imw[j] = ( EPSCRIT + work * work ) / EPSCRIT;
          else h += imw[j] = ( d[i][j] + work ) / d[i][j];
        }
        else h += imw[j] = 1.0;
      }
      for ( size_t k = 1; k <= p; k++ ) if ( fz[i][k] == 0 ) {
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        const double s = ddot( n, &imw[1], 1, &zold[1][k], p );
        z[i][k] = ( t + s ) / h;
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;

  // de-allocate memory
  freevector( imb );
  freevector( imw );
  freematrix( zold );

  return( fnew );
} // fxdmdsneg

void Cfxdmdsneg( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cmdsneg() performs multidimensional scaling allowing negative dissimilarities and fixed coordinates.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  int** fz = getimatrix( n, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) fz[i][j] = rfz[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = fxdmdsneg( n, delta, p, z, fz, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );

} // Cfxdmdsneg

double fxdwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function fxdwgtmds() performs weighted multidimensional scaling with fixed coordinates.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* h = getvector( n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta and w
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) {
      if ( iszero( w[i][j] ) ) {
        if ( iszero( w[j][i] ) ) delta[i][j] = delta[j][i] = w[i][j] = w[j][i] = 0.0;
        else {
          delta[i][j] = delta[j][i];
          w[i][j] = w[j][i] *= 0.5;
        }
      }
      else {
        if ( iszero( w[j][i] ) ) {
          delta[j][i] = delta[i][j];
          w[j][i] = w[i][j] *= 0.5;
        }
        else {
          delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
          w[i][j] = w[j][i] = 0.5 * ( w[i][j] + w[j][i] );
        }
      }
    }
  }

  // initialization
  int nfz = 0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) nfz += fz[i][k];
  if ( nfz == 0 ) center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  const double scale = dwssq( n * n, &delta[1][1], 1, &w[1][1], 1 );
  for ( size_t i = 1; i <= n; i++ ) h[i] = dsum( n, &w[i][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -w[i][j] * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) if ( fz[i][k] == 0 ) {
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        const double s = ddot( n, &w[i][1], 1, &zold[1][k], p );
        z[i][k] = ( t + s ) / h[i];
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // de-allocate memory
  freevector( imb );
  freevector( h );
  freematrix( zold );

  return( fnew );
} // fxdwgtmds

void Cfxdwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cfxdwgtmds() performs weighted multidimensional scaling with fixed coordinates.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  int** fz = getimatrix( n, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) fz[i][j] = rfz[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = fxdwgtmds( n, delta, w, p, z, fz, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );

} // Cfxdwgtmds

double fxdwgtmdsneg( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function wgtmdsneg() performs weighted multidimensional scaling allowing negative dissimilarities and fixed coordinates.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* imw = getvector( n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta and w
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) {
      if ( iszero( w[i][j] ) ) {
        if ( iszero( w[j][i] ) ) delta[i][j] = delta[j][i] = w[i][j] = w[j][i] = 0.0;
        else {
          delta[i][j] = delta[j][i];
          w[i][j] = w[j][i] *= 0.5;
        }
      }
      else {
        if ( iszero( w[j][i] ) ) {
          delta[j][i] = delta[i][j];
          w[j][i] = w[i][j] *= 0.5;
        }
        else {
          delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
          w[i][j] = w[j][i] = 0.5 * ( w[i][j] + w[j][i] );
        }
      }
    }
  }

  // initialization
  int nfz = 0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) nfz += fz[i][k];
  if ( nfz == 0 ) center( n, p, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  const double scale = dwssq( n * n, &delta[1][1], 1, &w[1][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( delta[i][j] < 0.0 || d[i][j] < DISCRIT ? 0.0 : -w[i][j] * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      double h = 0.0;
      for ( size_t j = 1; j <= n; j++ ) {
        if ( delta[i][j] < 0.0 ) {
          const double work = fabs( delta[i][j] );
          if ( d[i][j] < DISCRIT ) h += imw[j] = w[i][j] * ( EPSCRIT + work * work ) / EPSCRIT;
          else h += imw[j] = w[i][j] * ( d[i][j] + work ) / d[i][j];
        }
        else h += imw[j] = w[i][j];
      }
      for ( size_t k = 1; k <= p; k++ ) if ( fz[i][k] == 0 ) {
        const double t = ddot( n, &imb[1], 1, &zold[1][k], p );
        const double s = ddot( n, &imw[1], 1, &zold[1][k], p );
        z[i][k] = ( t + s ) / h;
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // de-allocate memory
  freevector( imb );
  freevector( imw );
  freematrix( zold );

  return( fnew );
} // fxdwgtmdsneg

void Cfxdwgtmdsneg( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cfxdwgtmdsneg() performs weighted multidimensional scaling allowing negative dissimilarities and fixed coordinates.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  int** fz = getimatrix( n, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) fz[i][j] = rfz[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = fxdwgtmdsneg( n, delta, w, p, z, fz, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );

} // Cfxdwgtmdsneg

double varmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function varmds() performs restricted multidimensional scaling.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double** qtvq = getmatrix( h, h, 0.0 );
  double* imb = getvector( n, 0.0 );
  double** bz = getmatrix( n, p, 0.0 );
  double** z = getmatrix( n, p, 0.0 );
  double** qtbz = getmatrix( h, p, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
  }

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) q[i][j] -= mn;
  }

  // initialization
  const double scale = dssq( n * n, &delta[1][1], 1 );
  for ( size_t i = 1; i <= h; i++ ) {
    double work = 0.0;
    for ( size_t k = 1; k <= n; k++ ) work -= q[k][i];
    for ( size_t j = 1; j <= h; j++ ) {
      for ( size_t k = 1; k <= n; k++ ) qtvq[i][j] += ( work + ( double )( n - 1 ) * q[k][i] ) * q[k][j];
    }
  }
  inverse( h, qtvq );
  double** v = getmatrix( h, h, 0.0 );
  double* phi = getvector( n, 0.0 );
  dgemm( true, false, h, h, n, 1.0, q, q, 0.0, v );
  evdcmp( h, v, phi );
  freevector( phi );
  dgemm( false, false, n, p, h, 1.0, q, v, 0.0, z );
  freematrix( v );
  dgemm( true, false, h, p, n, 1.0, q, z, 0.0, qtbz );
  dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
  dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) bz[i][k] = ddot( n, &imb[1], 1, &z[1][k], p );
    }
    dgemm( true, false, h, p, n, 1.0, q, bz, 0.0, qtbz );
    dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
    dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // de-allocate memory
  freematrix( qtvq );
  freevector( imb );
  freematrix( z );
  freematrix( bz );
  freematrix( qtbz );
  freematrix( zold );

  return( fnew );
} // varmds

void Cvarmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cvarmds() performs restricted multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t h = *rh;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = varmds( n, delta, p, h, q, b, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );
  
  // transfer to R
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rq[k] = q[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( q );
  freematrix( b );
  freematrix( d );

} // Cvarmds

double varmdsneg( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function varmdsneg() performs restricted multidimensional scaling allowing negative dissimilarities.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double** qtv = getmatrix( h, n, 0.0 );
  double** qtvq = getmatrix( h, h, 0.0 );
  double* imb = getvector( n, 0.0 );
  double** imw = getmatrix( n, n, 0.0 );
  double** bz = getmatrix( n, p, 0.0 );
  double** z = getmatrix( n, p, 0.0 );
  double** qtbz = getmatrix( h, p, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
  }

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) q[i][j] -= mn;
  }

  // initialization
  const double scale = dssq( n * n, &delta[1][1], 1 );
  for ( size_t i = 1; i <= h; i++ ) {
    double work = 0.0;
    for ( size_t k = 1; k <= n; k++ ) work -= q[k][i];
    for ( size_t j = 1; j <= h; j++ ) {
      for ( size_t k = 1; k <= n; k++ ) qtvq[i][j] += ( work + ( double )( n - 1 ) * q[k][i] ) * q[k][j];
    }
  }
  inverse( h, qtvq );
  double** v = getmatrix( h, h, 0.0 );
  double* phi = getvector( n, 0.0 );
  dgemm( true, false, h, h, n, 1.0, q, q, 0.0, v );
  evdcmp( h, v, phi );
  freevector( phi );
  dgemm( false, false, n, p, h, 1.0, q, v, 0.0, z );
  freematrix( v );
  dgemm( true, false, h, p, n, 1.0, q, z, 0.0, qtbz );
  dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
  dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( delta[i][j] < 0.0 || d[i][j] < DISCRIT ? 0.0 : -delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) bz[i][k] = ddot( n, &imb[1], 1, &z[1][k], p );
    }
    dgemm( true, false, h, p, n, 1.0, q, bz, 0.0, qtbz );
    for ( size_t i = 1; i <= n; i++ ) {
      double sm = 0.0;
      for ( size_t j = 1; j <= n; j++ ) {
        if ( delta[i][j] < 0.0 ) {
          const double work = fabs( delta[i][j] );
          if ( d[i][j] < DISCRIT ) sm += imw[i][j] = ( EPSCRIT + work * work ) / EPSCRIT;
          else sm += imw[i][j] = ( d[i][j] + work ) / d[i][j];
        }
        else sm += imw[i][j] = 1.0;
      }
      imw[i][i] = -1.0 * sm;
    }
    dgemm( true, false, h, n, n, 1.0, q, imw, 0.0, qtv );
    dgemm( false, false, h, h, n, 1.0, qtv, q, 0.0, qtvq );
    inverse( h, qtvq );
    dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
    dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // de-allocate memory
  freematrix( qtv );
  freematrix( qtvq );
  freevector( imb );
  freematrix( imw );
  freematrix( z );
  freematrix( bz );
  freematrix( qtbz );
  freematrix( zold );

  return( fnew );
} // varmdsneg

void Cvarmdsneg( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cvarmdsneg() performs restricted multidimensional scaling allowing negative dissimilarities.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t h = *rh;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = varmdsneg( n, delta, p, h, q, b, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rq[k] = q[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( q );
  freematrix( b );
  freematrix( d );

} // Cvarmdsneg

double varwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function varwgtmds() performs restricted weighted multidimensional scaling.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double** qtv = getmatrix( h, n, 0.0 );
  double** qtvq = getmatrix( h, h, 0.0 );
  double* imb = getvector( n, 0.0 );
  double** bz = getmatrix( n, p, 0.0 );
  double** z = getmatrix( n, p, 0.0 );
  double** qtbz = getmatrix( h, p, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta and w
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) {
      if ( iszero( w[i][j] ) ) {
        if ( iszero( w[j][i] ) ) delta[i][j] = delta[j][i] = w[i][j] = w[j][i] = 0.0;
        else {
          delta[i][j] = delta[j][i];
          w[i][j] = w[j][i] *= 0.5;
        }
      }
      else {
        if ( iszero( w[j][i] ) ) {
          delta[j][i] = delta[i][j];
          w[j][i] = w[i][j] *= 0.5;
        }
        else {
          delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
          w[i][j] = w[j][i] = 0.5 * ( w[i][j] + w[j][i] );
        }
      }
    }
  }

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) q[i][j] -= mn;
  }

  // initialization
  const double scale = dwssq( n * n, &delta[1][1], 1, &w[1][1], 1 );
  for ( size_t i = 1; i <= n; i++ ) {
    double work = 0.0;
    for ( size_t j = 1; j <= n; j++ ) work -= w[i][j] *= -1.0;
    w[i][i] = work;  
  }
  dgemm( true, false, h, n, n, 1.0, q, w, 0.0, qtv );
  dgemm( false, false, h, h, n, 1.0, qtv, q, 0.0, qtvq );
  inverse( h, qtvq );
  double** v = getmatrix( h, h, 0.0 );
  double* phi = getvector( n, 0.0 );
  dgemm( true, false, h, h, n, 1.0, q, q, 0.0, v );
  evdcmp( h, v, phi );
  freevector( phi );
  dgemm( false, false, n, p, h, 1.0, q, v, 0.0, z );
  freematrix( v );
  dgemm( true, false, h, p, n, 1.0, q, z, 0.0, qtbz );
  dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
  dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) w[i][j] *= -1.0;
    w[i][i] = 0.0;  
  }

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -w[i][j] * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) bz[i][k] = ddot( n, &imb[1], 1, &z[1][k], p );
    }
    dgemm( true, false, h, p, n, 1.0, q, bz, 0.0, qtbz );
    dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
    dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // de-allocate memory
  freematrix( qtv );
  freematrix( qtvq );
  freevector( imb );
  freematrix( z );
  freematrix( bz );
  freematrix( qtbz );
  freematrix( zold );

  return( fnew );
} // varwgtmds

void Cvarwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cvarwgtmds() performs restricted weighted multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t h = *rh;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = varwgtmds( n, delta, w, p, h, q, b, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rq[k] = q[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( q );
  freematrix( b );
  freematrix( d );

} // Cvarwgtmds

double varwgtmdsneg( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function varwgtmdsneg() performs restricted weighted multidimensional scaling allowing negative dissimilarities.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double** qtv = getmatrix( h, n, 0.0 );
  double** qtvq = getmatrix( h, h, 0.0 );
  double* imb = getvector( n, 0.0 );
  double** imw = getmatrix( n, n, 0.0 );
  double** bz = getmatrix( n, p, 0.0 );
  double** z = getmatrix( n, p, 0.0 );
  double** qtbz = getmatrix( h, p, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta and w
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) {
      if ( iszero( w[i][j] ) ) {
        if ( iszero( w[j][i] ) ) delta[i][j] = delta[j][i] = w[i][j] = w[j][i] = 0.0;
        else {
          delta[i][j] = delta[j][i];
          w[i][j] = w[j][i] *= 0.5;
        }
      }
      else {
        if ( iszero( w[j][i] ) ) {
          delta[j][i] = delta[i][j];
          w[j][i] = w[i][j] *= 0.5;
        }
        else {
          delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
          w[i][j] = w[j][i] = 0.5 * ( w[i][j] + w[j][i] );
        }
      }
    }
  }

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) q[i][j] -= mn;
  }

  // initialization
  const double scale = dwssq( n * n, &delta[1][1], 1, &w[1][1], 1 );
  for ( size_t i = 1; i <= n; i++ ) {
    double work = 0.0;
    for ( size_t j = 1; j <= n; j++ ) work -= w[i][j] *= -1.0;
    w[i][i] = work;  
  }
  dgemm( true, false, h, n, n, 1.0, q, w, 0.0, qtv );
  dgemm( false, false, h, h, n, 1.0, qtv, q, 0.0, qtvq );
  inverse( h, qtvq );
  double** v = getmatrix( h, h, 0.0 );
  double* phi = getvector( n, 0.0 );
  dgemm( true, false, h, h, n, 1.0, q, q, 0.0, v );
  evdcmp( h, v, phi );
  freevector( phi );
  dgemm( false, false, n, p, h, 1.0, q, v, 0.0, z );
  freematrix( v );
  dgemm( true, false, h, p, n, 1.0, q, z, 0.0, qtbz );
  dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
  dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) w[i][j] *= -1.0;
    w[i][i] = 0.0;  
  }

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( delta[i][j] < 0.0 || d[i][j] < DISCRIT ? 0.0 : -1.0 * w[i][j] * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) bz[i][k] = ddot( n, &imb[1], 1, &z[1][k], p );
    }
    dgemm( true, false, h, p, n, 1.0, q, bz, 0.0, qtbz );
    for ( size_t i = 1; i <= n; i++ ) {
      double sm = 0.0;
      for ( size_t j = 1; j <= n; j++ ) {
        if ( delta[i][j] < 0.0 ) {
          const double work = fabs( delta[i][j] );
          if ( d[i][j] < DISCRIT ) sm += imw[i][j] = w[i][j] * ( EPSCRIT + work * work ) / EPSCRIT;
          else sm += imw[i][j] = w[i][j] * ( d[i][j] + work ) / d[i][j];
        }
        else sm += imw[i][j] = w[i][j];
      }
      imw[i][i] = -1.0 * sm;
    }
    dgemm( true, false, h, n, n, 1.0, q, imw, 0.0, qtv );
    dgemm( false, false, h, h, n, 1.0, qtv, q, 0.0, qtvq );
    inverse( h, qtvq );
    dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
    dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    fnew = dwsse( n * n, &delta[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // rotate to principal axes of z
//  rotateplus( n, p, z, h, b );
 
  // de-allocate memory
  freematrix( qtv );
  freematrix( qtvq );
  freevector( imb );
  freematrix( imw );
  freematrix( z );
  freematrix( bz );
  freematrix( qtbz );
  freematrix( zold );

  return( fnew );
} // varwgtmdsneg

void Cvarwgtmdsneg( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cvarwgtmdsneg() performs restricted weighted multidimensional scaling allowing negative dissimilarities.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t h = *rh;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  double** d = getmatrix( n, n, 0.0 );
  double FCRIT = *rfdif;
  double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = varwgtmdsneg( n, delta, w, p, h, q, b, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rq[k] = q[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( q );
  freematrix( b );
  freematrix( d );

} // Cvarwgtmdsneg

double penvarmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, const double lambda, const double alpha, const bool grouped, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function penvarmds() performs penalized restricted multidimensional scaling.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double INVTINY = 1.0 / TINY;

  // allocate memory
  double** qtvq = getmatrix( h, h, 0.0 );
  double* imb = getvector( n, 0.0 );
  double** bz = getmatrix( n, p, 0.0 );
  double** z = getmatrix( n, p, 0.0 );
  double** qtbz = getmatrix( h, p, 0.0 );
  double** hh = getmatrix( h, h, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // symmetrize delta
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 0.5 * ( delta[i][j] + delta[j][i] );
  }

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) q[i][j] -= mn;
  }

  // initialization
  const double rlambda = ( 1.0 - alpha ) * lambda;
  const double llambda = ( grouped == true ? 0.0 : alpha * lambda );
  const double glambda = ( grouped == true ? alpha * lambda : 0.0 );
  for ( size_t i = 1; i <= h; i++ ) {
    double work = 0.0;
    for ( size_t k = 1; k <= n; k++ ) work -= q[k][i];
    for ( size_t j = 1; j <= h; j++ ) {
      for ( size_t k = 1; k <= n; k++ ) hh[i][j] = qtvq[i][j] += ( work + ( double )( n - 1 ) * q[k][i] ) * q[k][j];
    }
    hh[i][i] = qtvq[i][i] += rlambda;
  }
  inverse( h, hh );
  double** v = getmatrix( h, h, 0.0 );
  double* phi = getvector( n, 0.0 );
  dgemm( true, false, h, h, n, 1.0, q, q, 0.0, v );
  evdcmp( h, v, phi );
  freevector( phi );
  dgemm( false, false, n, p, h, 1.0, q, v, 0.0, z );
  freematrix( v );
  dgemm( true, false, h, p, n, 1.0, q, z, 0.0, qtbz );
  dgemm( false, false, h, p, h, 1.0, hh, qtbz, 0.0, b );
  dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  // update distances and calculate stress
  euclidean1( n, p, z, d );
  double fridge = dssq( h * p, &b[1][1], 1 );
  double flasso = 0.0;
  double fgroup = 0.0;
  for ( size_t i = 1; i <= h; i++ ) {
    for ( size_t j = 1; j <= p; j++ ) flasso += fabs( b[i][j] );
    fgroup += sqrt( dssq( p, &b[i][1], 1 ) );
  }
  double fold = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) + rlambda * fridge + llambda * flasso + glambda * fgroup;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= n; j++ ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * delta[i][j] / d[i][j] );
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) bz[i][k] = ddot( n, &imb[1], 1, &z[1][k], p );
    }
    dgemm( true, false, h, p, n, 1.0, q, bz, 0.0, qtbz );
    for ( size_t k = 1; k <= p; k++ ) {
      dcopy( h * h, &qtvq[1][1], 1, &hh[1][1], 1 );
      for ( size_t i = 1; i <= h; i++ ) {
        hh[i][i] += 0.5 * llambda * ( fabs( b[i][k] ) < TINY ? INVTINY : 1.0 / fabs( b[i][k] ) );
        const double work = sqrt( dssq( p, &b[i][1], 1 ) );
        hh[i][i] += 0.5 * glambda * ( work < TINY ? INVTINY : 1.0 / work );
      }
      inverse( h, hh );
      for ( size_t i = 1; i <= h; i++ ) b[i][k] = ddot( h, &hh[i][1], 1, &qtbz[1][k], p );
    }
    dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );

    // update distances and calculate stress
    euclidean1( n, p, z, d );
    fridge = dssq( h * p, &b[1][1], 1 );
    flasso = fgroup = 0.0;
    for ( size_t i = 1; i <= h; i++ ) {
      for ( size_t j = 1; j <= p; j++ ) flasso += fabs( b[i][j] );
      fgroup += sqrt( dssq( p, &b[i][1], 1 ) );
    }
    fnew = dsse( n * n, &delta[1][1], 1, &d[1][1], 1 ) + rlambda * fridge + llambda * flasso + glambda * fgroup;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew ); 

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif < FCRIT ) break;
    const double zdif = dsse( n * p, &zold[1][1], 1, &z[1][1], 1 );
    if ( zdif < ZCRIT ) break;

    fold = fnew;
    dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );
  }
  ( *lastiter ) = iter;
 
  // de-allocate memory
  freematrix( qtvq );
  freevector( imb );
  freematrix( z );
  freematrix( bz );
  freematrix( qtbz );
  freematrix( hh );
  freematrix( zold );

  return( fnew );
} // penvarmds

void Cpenvarmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rlambda, double* ralpha, int* rgrouped, double* rd, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cpenvarmds() performs penalized restricted multidimensional scaling
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  size_t h = *rh;
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  const double lambda = *rlambda;
  const double alpha = *ralpha;
  const bool grouped = ( *rgrouped ) != 0;
  double** d = getmatrix( n, n, 0.0 );
  size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  const bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = penvarmds( n, delta, p, h, q, b, lambda, alpha, grouped, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rq[k] = q[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( q );
  freematrix( b );
  freematrix( d );

} // Cpenvarmds

void mds_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test mds at ", dt );
  printstring( "\n", "==============================================================\n" );

  randomize( &seed );
 
  size_t n = 20;
  size_t p = 2;
  bool anchor = false;
  size_t MAXITER = 1024;
  double FCRIT = 0.0000001;
  double ZCRIT = 0.00001;
  size_t lastiter = 0;
  double lastdif = 0.0;

  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j < i; j++ ) delta[i][j] = delta[j][i] = 2.0 * nextdouble();
  double** w = getmatrix( n, n, 2.0 );
  for ( size_t i = 1; i <= n; i++ ) w[i][i] = 0.0;
  double** z = getmatrix( n, p, 0.0 );
  int** fz = getimatrix( n, p, 0 );
  double** d = getmatrix( n, n, 0.0 );

  pcoa( n, delta, p, 0.0, z );
  size_t tm = setstarttime();
  mds( n, delta, p, z, d, anchor, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, true );
  printscalar( "elapsed for mds", getelapsedtime( tm ) );

  FCRIT = 0.0;
  ZCRIT = 0.00000001;
  for ( size_t test = 4; test <= 20; test++ ) {

    double** data = getmatrix( test, test, 0.0 );
    double** ddata = getmatrix( test, test, 0.0 );
    double** x = getmatrix( test, p, 0.0 );

    for ( size_t i = 1; i <= test; i++ ) for ( size_t k = 1; k <= p; k++ ) x[i][k] = 100.0 * nextdouble();
    euclidean1( test, p, x, data );
    double mind = 100.0;
    for ( size_t i = 1; i <= test; i++ ) for ( size_t j = 1; j <= test; j++ ) if ( i != j ) mind = fmin( mind, data[i][j] );
    for ( size_t i = 1; i <= test; i++ ) for ( size_t j = 1; j <= test; j++ ) if ( i != j ) data[i][j] = data[i][j] - mind;
    for ( size_t i = 1; i <= test; i++ ) for ( size_t k = 1; k <= p; k++ ) x[i][k] = 100.0 * nextdouble();

    tm = setstarttime();
    pcoa( test, data, p, 0.0, x );
    double f1 = mds( test, data, p, x, ddata, false, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, false );
    double ac = addconst( test, data );
    pcoa( test, data, p, ac, x );
    double f2 = mds( test, data, p, x, ddata, true, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, false );
    double d1 = getelapsedtime( tm );

    #ifdef R
      Rprintf( "%4zu; timing %8.6f; constant %8.6f; value %8.6f vs. %8.6f\n", test, d1, mind, f1, f2 );
    #else
      printf( "%4llu; timing %8.6f; constant %8.6f; value %8.6f vs. %8.6f\n", test, d1, mind, f1, f2 );
    #endif

    freematrix( data );
    freematrix( ddata );
    freematrix( x );
  }

  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );

//  double** vars = readmatrix( "facial.properties", &n, &h );
//  p = 2;
//  double** z = getmatrix( n, p, 0.0 );
//  double** b = getmatrix( h, p, 0.0 );
//  double* w = getvector( n, 1.0 );
//  double* a = getvector( p, 0.0 );
//  //double** d = getmatrix( n, n, 0.0 );
//  double** d2 = getmatrix( n, n, 0.0 );
//  double* d1 = getvector( n, 0.0 );
//  int* level = getivector( p, 3 );
//  const size_t nlambda = 8;
//  double* lambda = getvector( nlambda, 0.0 );
//  lambda[1] = 1.0;
//  lambda[2] = 2.0;
//  lambda[3] = 5.0;
//  lambda[4] = 10.0;
//  lambda[5] = 20.0;
//  lambda[6] = 50.0;
//  lambda[7] = 100.0;
//  lambda[8] = 200.0;
//  double* mserror = getvector( nlambda, 0.0 );
//  double* stderror = getvector( nlambda, 0.0 );
//  pcoa( n, delta, p, 0.0, z );
//  respcoa(n, delta, h, vars, p, 0.0, b, z );
//  varmds( n, delta, p, h, vars, b, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );
//  penvarmds( n, delta, p, h, vars, b, lambda[1], alpha, grouped, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );
//  rcvpenvarmds( 100, 4, nlambda, lambda, alpha, grouped, n, delta, p, h, vars, b, d, MAXITER, FCRIT, ZCRIT, echo, mserror, stderror );
//  printf( "\n" );
//  printvector( "lambda", nlambda, lambda );
//  printvector( "mse", nlambda, mserror );
//  printvector( "se", nlambda, stderror );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test mds at ", dt );
  printstring( "\n", "==============================================================\n" );
} // unittest
