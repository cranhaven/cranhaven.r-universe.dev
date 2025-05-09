//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

double splmds( const size_t n, double** delta, const size_t p, double** z, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function splmds() performs polynomial multidimensional scaling.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* s = getvector( p, 0.0 );
  double** gamma = getmatrix( n, n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  // determine symmetry
  bool symmetric = true;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( isnotequal( delta[i][j], delta[j][i] ) ) {
    symmetric = false;
    break;
  }

  // initialization
  double** basis = polynomialbasis( symmetric, n, delta, NULL, ninner, iknots, degree, anchor, knotstype, true );
  size_t ncoefs = anchor + ninner + degree;
  double* b = getvector( ncoefs, 1.0 );
  b[1] = 0.0;
  double** bstbs = getmatrix( ncoefs, ncoefs, 0.0 );
  const size_t nn = ( symmetric == true ? n * ( n - 1 ) / 2 : n * n - n );
  dgemm( true, false, ncoefs, ncoefs, nn, 1.0, basis, basis, 0.0, bstbs );
  polynomialcoefficients( symmetric, n, delta, NULL, ncoefs, basis, bstbs, b, 1.0, gamma );
  double h = ( double )( n - 1 );

  // calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = 0.0;
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double gij = gamma[i][j];
      const double work = gij - d[i][j];
      fold += work * work;
      scale += gij * gij;
    }
  }
  fold /= scale;
  double fhalf = 0.0;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute transformation update
    polynomialcoefficients( symmetric, n, d, NULL, ncoefs, basis, bstbs, b, 1.0 / ( 1.0 - fold ), gamma );

    // intermediate results
    if ( echo == true ) {
      scale = dssq( n * n, &gamma[1][1], 1 );
      fhalf = dsse( n * n, &gamma[1][1], 1, &d[1][1], 1 ) / scale;
    }

    // compute update for z
    for ( size_t k = 1; k <= p; k++ ) {
      double work = 0.0;
      for ( size_t i = 1; i <= n; i++ ) work += z[i][k];
      s[k] = work;
    }
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      if ( symmetric == true ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * gamma[i][j] / d[i][j] );
      }
      else {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -0.5 * ( gamma[i][j] + gamma[j][i] ) / d[i][j] );
      }
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) {
        double t = 0.0;
        for ( size_t j = 1; j <= n; j++ ) t += imb[j] * zold[j][k];
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
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) delta[i][j] = gamma[i][j];

  // de-allocate memory
  freevector( imb );
  freevector( s );
  freematrix( zold );
  freematrix( gamma );
  freematrix( basis );
  freematrix( bstbs );
  freevector( b );

  return( fnew );
} // splmds

void Csplmds( int* rn, double* rdelta, int* rp, double* rz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Csplmds() performs polynomial multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  size_t ninner = *rninner;
  double* iknots = getvector( max_t( 1, ninner ), 0.0 );
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) iknots[i] = riknots[k];
  double** d = getmatrix( n, n, 0.0 );
  size_t degree = *rdegree;
  bool anchor = ( *ranchor ) != 0;
  int knotstype = *rknotstype;
  size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = splmds( n, delta, p, z, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rdelta[k] = delta[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) riknots[k] = iknots[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( z );
  freematrix( d );
  freevector( iknots );

} // Csplmds

double splwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function splwgtmds() performs polynomial weighted multidimensional scaling.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* h = getvector( n, 0.0 );
  double** gamma = getmatrix( n, n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  // determine symmetry
  bool symmetric = true;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( isnotequal( w[i][j], w[j][i] ) || isnotequal( delta[i][j], delta[j][i] ) ) {
    symmetric = false;
    break;
  }

  // initialization
  double** basis = polynomialbasis( symmetric, n, delta, w, ninner, iknots, degree, anchor, knotstype, true );
  size_t ncoefs = anchor + ninner + degree;
  double* b = getvector( ncoefs, 1.0 );
  b[1] = 0.0;
  polynomialcoefficients( symmetric, n, delta, w, ncoefs, basis, NULL, b, 1.0, gamma );
  for ( size_t i = 1; i <= n; i++ ) h[i] = dsum( n, &w[i][1], 1 );

  // calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = 0.0;
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double wij = w[i][j];
      if ( isnotzero( wij ) ) {
        const double gij = gamma[i][j];
        const double work = gij - d[i][j];
        fold += wij * work * work;
        scale += wij * gij * gij;
      }
    }
  }
  fold /= scale;
  double fhalf = 0.0;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute transformation update
    polynomialcoefficients( symmetric, n, d, w, ncoefs, basis, NULL, b, 1.0 / ( 1.0 - fold ), gamma );

    // intermediate results
    if ( echo == true ) {
      scale = dwssq( n * n, &gamma[1][1], 1, &w[1][1], 1 );
      fhalf = dwsse( n * n, &gamma[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
    }

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      if ( symmetric == true ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -w[i][j] * gamma[i][j] / d[i][j] );
      }
      else {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -0.5 * ( w[i][j] * gamma[i][j] + w[j][i] * gamma[j][i] ) / d[i][j] );
      }
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) {
        double t = 0.0;
        for ( size_t j = 1; j <= n; j++ ) t += imb[j] * zold[j][k];
        double s = 0.0;
        for ( size_t j = 1; j <= n; j++ ) s += w[i][j] * zold[j][k];
        z[i][k] = ( t + s ) / h[i];
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    scale = dwssq( n * n, &gamma[1][1], 1, &w[1][1], 1 );
    fnew = dwsse( n * n, &gamma[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

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
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) delta[i][j] = gamma[i][j];

  // de-allocate memory
  freevector( imb );
  freevector( h );
  freematrix( zold );
  freematrix( gamma );
  freematrix( basis );
  freevector( b );

  return( fnew );
} // splwgtmds

void Csplwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Csplwgtmds() performs polynomial weighted multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  size_t ninner = *rninner;
  double* iknots = getvector( ninner, 0.0 );
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) iknots[i] = riknots[k];
  double** d = getmatrix( n, n, 0.0 );
  size_t degree = *rdegree;
  bool anchor = ( *ranchor ) != 0;
  int knotstype = *rknotstype;
  size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = splwgtmds( n, delta, w, p, z, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rdelta[k] = delta[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) riknots[k] = iknots[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freematrix( d );
  freevector( iknots );

} // Csplwgtmds

double fxdsplmds( const size_t n, double** delta, const size_t p, double** z, int** fz, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function fxdsplmds() performs polynomial multidimensional scaling with fixed coordinates.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* s = getvector( p, 0.0 );
  double** gamma = getmatrix( n, n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  // determine symmetry
  bool symmetric = true;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( isnotequal( delta[i][j], delta[j][i] ) ) {
    symmetric = false;
    break;
  }

  // initialization
  double** basis = polynomialbasis( symmetric, n, delta, NULL, ninner, iknots, degree, anchor, knotstype, true );
  size_t ncoefs = anchor + ninner + degree;
  double* b = getvector( ncoefs, 1.0 );
  b[1] = 0.0;
  double** bstbs = getmatrix( ncoefs, ncoefs, 0.0 );
  const size_t nn = ( symmetric == true ? n * ( n - 1 ) / 2 : n * n - n );
  dgemm( true, false, ncoefs, ncoefs, nn, 1.0, basis, basis, 0.0, bstbs );
  polynomialcoefficients( symmetric, n, delta, NULL, ncoefs, basis, bstbs, b, 1.0, gamma );
  int nfz = 0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) nfz += fz[i][k];
  if ( nfz == 0 ) center( n, p, z );
  double h = ( double )( n - 1 );

  // calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = 0.0;
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double gij = gamma[i][j];
      const double work = gij - d[i][j];
      fold += work * work;
      scale += gij * gij;
    }
  }
  fold /= scale;
  double fhalf = 0.0;
  double fnew = 0.0;
  ( *lastdif ) = fabs( fnew - fold );

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute transformation update
    gamma[1][1] = ( *lastdif ); 
    polynomialcoefficients( symmetric, n, d, NULL, ncoefs, basis, bstbs, b, 1.0 / ( 1.0 - fold ), gamma );

    // intermediate results
    if ( echo == true ) {
      scale = dssq( n * n, &gamma[1][1], 1 );
      fhalf = dsse( n * n, &gamma[1][1], 1, &d[1][1], 1 ) / scale;
    }

    // compute update for z
    for ( size_t k = 1; k <= p; k++ ) {
      double work = 0.0;
      for ( size_t i = 1; i <= n; i++ ) work += z[i][k];
      s[k] = work;
    }
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      if ( symmetric == true ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * gamma[i][j] / d[i][j] );
      }
      else {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -0.5 * ( gamma[i][j] + gamma[j][i] ) / d[i][j] );
      }
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) if ( fz[i][k] == 0 ) {
        double t = 0.0;
        for ( size_t j = 1; j <= n; j++ ) t += imb[j] * zold[j][k];
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
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) delta[i][j] = gamma[i][j];

  // de-allocate memory
  freevector( imb );
  freevector( s );
  freematrix( zold );
  freematrix( gamma );
  freematrix( basis );
  freematrix( bstbs );
  freevector( b );

  return( fnew );
} // fxdsplmds

void Cfxdsplmds( int* rn, double* rdelta, int* rp, double* rz, int* rfz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cfxdsplmds() performs polynomial multidimensional scaling with fixed coordinates.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  int** fz = getimatrix( n, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) fz[i][j] = rfz[k];
  size_t ninner = *rninner;
  double* iknots = getvector( max_t( 1, ninner ), 0.0 );
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) iknots[i] = riknots[k];
  double** d = getmatrix( n, n, 0.0 );
  size_t degree = *rdegree;
  bool anchor = ( *ranchor ) != 0;
  int knotstype = *rknotstype;
  size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = fxdsplmds( n, delta, p, z, fz, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rdelta[k] = delta[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) riknots[k] = iknots[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );
  freevector( iknots );

} // Cfxdsplmds

double fxdsplwgtmds( const size_t n, double** delta, double** w, const size_t p, double** z, int** fz, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function fxdsplwgtmds() performs polynomial weighted multidimensional scaling with fixed coordinates.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double* imb = getvector( n, 0.0 );
  double* h = getvector( n, 0.0 );
  double** gamma = getmatrix( n, n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  // determine symmetry
  bool symmetric = true;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( isnotequal( w[i][j], w[j][i] ) || isnotequal( delta[i][j], delta[j][i] ) ) {
    symmetric = false;
    break;
  }

  // initialization
  double** basis = polynomialbasis( symmetric, n, delta, w, ninner, iknots, degree, anchor, knotstype, true );
  size_t ncoefs = anchor + ninner + degree;
  double* b = getvector( ncoefs, 1.0 );
  b[1] = 0.0;
  polynomialcoefficients( symmetric, n, delta, w, ncoefs, basis, NULL, b, 1.0, gamma );
  int nfz = 0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) nfz += fz[i][k];
  if ( nfz == 0 ) center( n, p, z );
  for ( size_t i = 1; i <= n; i++ ) h[i] = dsum( n, &w[i][1], 1 );

  // calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = 0.0;
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double wij = w[i][j];
      if ( isnotzero( wij ) ) {
        const double gij = gamma[i][j];
        const double work = gij - d[i][j];
        fold += wij * work * work;
        scale += wij * gij * gij;
      }
    }
  }
  fold /= scale;
  double fhalf = 0.0;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute transformation update
    polynomialcoefficients( symmetric, n, d, w, ncoefs, basis, NULL, b, 1.0 / ( 1.0 - fold ), gamma );

    // intermediate results
    if ( echo == true ) {
      scale = dwssq( n * n, &gamma[1][1], 1, &w[1][1], 1 );
      fhalf = dwsse( n * n, &gamma[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
    }

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      if ( symmetric == true ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -w[i][j] * gamma[i][j] / d[i][j] );
      }
      else {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -0.5 * ( w[i][j] * gamma[i][j] + w[j][i] * gamma[j][i] ) / d[i][j] );
      }
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) if ( fz[i][k] == 0 ) {
        double t = 0.0;
        for ( size_t j = 1; j <= n; j++ ) t += imb[j] * zold[j][k];
        double s = 0.0;
        for ( size_t j = 1; j <= n; j++ ) s += w[i][j] * zold[j][k];
        z[i][k] = ( t + s ) / h[i];
      }
    }

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    scale = dwssq( n * n, &gamma[1][1], 1, &w[1][1], 1 );
    fnew = dwsse( n * n, &gamma[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

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
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) delta[i][j] = gamma[i][j];

  // de-allocate memory
  freevector( imb );
  freevector( h );
  freematrix( zold );
  freematrix( gamma );
  freematrix( basis );
  freevector( b );

  return( fnew );
} // fxdsplwgtmds

void Cfxdsplwgtmds( int* rn, double* rdelta, double* rw, int* rp, double* rz, int* rfz, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cfxdsplwgtmds() performs polynomial weighted multidimensional scaling with fixed coordinates.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  int** fz = getimatrix( n, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) fz[i][j] = rfz[k];
  size_t ninner = *rninner;
  double* iknots = getvector( ninner, 0.0 );
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) iknots[i] = riknots[k];
  double** d = getmatrix( n, n, 0.0 );
  size_t degree = *rdegree;
  bool anchor = ( *ranchor ) != 0;
  int knotstype = *rknotstype;
  size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = fxdsplwgtmds( n, delta, w, p, z, fz, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rdelta[k] = delta[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) riknots[k] = iknots[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );
  freevector( iknots );

} // Cfxdsplwgtmds

double varsplmds( const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function varsplmds() performs restricted polynomial multidimensional scaling.
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
  double** gamma = getmatrix( n, n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // determine symmetry
  bool symmetric = true;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( isnotequal( delta[i][j], delta[j][i] ) ) {
    symmetric = false;
    break;
  }

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) q[i][j] -= mn;
  }

  // initialization
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
  for ( size_t j = 1; j <= p; j++ ) {
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t k = 1; k <= h; k++ ) work += q[i][k] * v[k][j];
      z[i][j] = work;  
    }
  }
  freematrix( v );
  dgemm( true, false, h, p, n, 1.0, q, z, 0.0, qtbz );
  dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
  dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  double** basis = polynomialbasis( symmetric, n, delta, NULL, ninner, iknots, degree, anchor, knotstype, true );
  size_t ncoefs = anchor + ninner + degree;
  double* pb = getvector( ncoefs, 1.0 );
  pb[1] = 0.0;
  double** bstbs = getmatrix( ncoefs, ncoefs, 0.0 );
  const size_t nn = ( symmetric == true ? n * ( n - 1 ) / 2 : n * n - n );
  dgemm( true, false, ncoefs, ncoefs, nn, 1.0, basis, basis, 0.0, bstbs );
  polynomialcoefficients( symmetric, n, delta, NULL, ncoefs, basis, bstbs, pb, 1.0, gamma );

  // update distances and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = 0.0;
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double gij = gamma[i][j];
      const double work = gij - d[i][j];
      fold += work * work;
      scale += gij * gij;
    }
  }
  fold /= scale;
  double fhalf = 0.0;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute transformation update
    polynomialcoefficients( symmetric, n, d, NULL, ncoefs, basis, bstbs, pb, 1.0 / ( 1.0 - fold ), gamma );

    // intermediate results
    if ( echo == true ) {
      scale = dssq( n * n, &gamma[1][1], 1 );
      fhalf = dsse( n * n, &gamma[1][1], 1, &d[1][1], 1 ) / scale;
    }

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      if ( symmetric == true ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * gamma[i][j] / d[i][j] );
      }
      else {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -0.5 * ( gamma[i][j] + gamma[j][i] ) / d[i][j] );
      }
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) {
        double work = 0.0;
        for ( size_t j = 1; j <= n; j++ ) work += imb[j] * z[j][k];
        bz[i][k] = work;
      }
    }
    dgemm( true, false, h, p, n, 1.0, q, bz, 0.0, qtbz );
    dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
    dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );

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
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) delta[i][j] = gamma[i][j];

  // de-allocate memory
  freematrix( qtvq );
  freevector( imb );
  freematrix( z );
  freematrix( bz );
  freematrix( qtbz );
  freematrix( gamma );
  freematrix( basis );
  freematrix( bstbs );
  freevector( pb );
  freematrix( zold );

  return( fnew );
} // varsplmds

void Cvarsplmds( int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cvarsplmds() performs restricted polynomial multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t h = *rh;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  size_t ninner = *rninner;
  double* iknots = getvector( max_t( 1, ninner ), 0.0 );
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) iknots[i] = riknots[k];
  double** d = getmatrix( n, n, 0.0 );
  size_t degree = *rdegree;
  bool anchor = ( *ranchor ) != 0;
  int knotstype = *rknotstype;
  size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = varsplmds( n, delta, p, h, q, b, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rdelta[k] = delta[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) riknots[k] = iknots[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( q );
  freematrix( b );
  freematrix( d );
  freevector( iknots );

} // Cvarsplmds

double varsplwgtmds( const size_t n, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** d, const size_t degree, const size_t ninner, double* iknots, const bool anchor, const int knotstype, const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function varsplwgtmds() performs restricted polynomial weighted multidimensional scaling.
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
  double* m = getvector( n, 0.0 );
  double** gamma = getmatrix( n, n, 0.0 );
  double** zold = getmatrix( n, p, 0.0 );

  // determine symmetry
  bool symmetric = true;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) if ( isnotequal( delta[i][j], delta[j][i] ) ) {
    symmetric = false;
    break;
  }

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) q[i][j] -= mn;
  }

  // initialization
  for ( size_t i = 1; i <= n; i++ ) m[i] = dsum( n, &w[i][1], 1 );
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
  for ( size_t j = 1; j <= p; j++ ) {
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t k = 1; k <= h; k++ ) work += q[i][k] * v[k][j];
      z[i][j] = work;  
    }
  }
  freematrix( v );
  dgemm( true, false, h, p, n, 1.0, q, z, 0.0, qtbz );
  dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
  dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );
  dcopy( n * p, &z[1][1], 1, &zold[1][1], 1 );

  double** basis = polynomialbasis( symmetric, n, delta, NULL, ninner, iknots, degree, anchor, knotstype, true );
  size_t ncoefs = anchor + ninner + degree;
  double* pb = getvector( ncoefs, 1.0 );
  pb[1] = 0.0;
  polynomialcoefficients( symmetric, n, delta, w, ncoefs, basis, NULL, pb, 1.0, gamma );

  // update distances, scale gamma, and calculate normalized stress
  euclidean1( n, p, z, d );
  double fold = 0.0;
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= n; j++ ) if ( i != j ) {
      const double wij = w[i][j];
      if ( isnotzero( wij ) ) {
        const double gij = gamma[i][j];
        const double work = gij - d[i][j];
        fold += wij * work * work;
        scale += wij * gij * gij;
      }
    }
  }
  fold /= scale;
  double fhalf = 0.0;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold ); 

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute transformation update
    polynomialcoefficients( symmetric, n, d, w, ncoefs, basis, NULL, pb, 1.0 / ( 1.0 - fold ), gamma );

    // intermediate results
    if ( echo == true ) {
      scale = dwssq( n * n, &gamma[1][1], 1, &w[1][1], 1 );
      fhalf = dwsse( n * n, &gamma[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;
    }

    // compute update for z
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      if ( symmetric == true ) {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -1.0 * w[i][j] * gamma[i][j] / d[i][j] );
      }
      else {
        for ( size_t j = 1; j <= n; j++ ) if ( i != j ) work += imb[j] = ( d[i][j] < TINY ? 0.0 : -0.5 * ( w[i][j] * gamma[i][j] + w[j][i] * gamma[j][i] ) / d[i][j] );
      }
      imb[i] = -1.0 * work;
      for ( size_t k = 1; k <= p; k++ ) {
        double work = 0.0;
        for ( size_t j = 1; j <= n; j++ ) work += imb[j] * z[j][k];
        bz[i][k] = work;
      }
    }
    dgemm( true, false, h, p, n, 1.0, q, bz, 0.0, qtbz );
    dgemm( false, false, h, p, h, 1.0, qtvq, qtbz, 0.0, b );
    dgemm( false, false, n, p, h, 1.0, q, b, 0.0, z );

    // update distances and calculate normalized stress
    euclidean1( n, p, z, d );
    scale = dwssq( n * n, &gamma[1][1], 1, &w[1][1], 1 );
    fnew = dwsse( n * n, &gamma[1][1], 1, &d[1][1], 1, &w[1][1], 1 ) / scale;

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
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= n; j++ ) delta[i][j] = gamma[i][j];

  // de-allocate memory
  freematrix( qtvq );
  freevector( imb );
  freematrix( z );
  freematrix( bz );
  freematrix( qtbz );
  freevector( m );
  freematrix( gamma );
  freematrix( basis );
  freevector( pb );
  freematrix( zold );

  return( fnew );
} // varsplwgtmds

void Cvarsplwgtmds( int* rn, double* rdelta, double* rw, int* rp, int* rh, double* rq, double* rb, double* rd, int* rdegree, int* rninner, double* riknots, int* ranchor, int* rknotstype, int* rmaxiter, double* rfdif, double* rzdif, double* rfvalue, int* recho )
// Function Cvarsplmds() performs restricted polynomial weighted multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  size_t h = *rh;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  size_t ninner = *rninner;
  double* iknots = getvector( max_t( 1, ninner ), 0.0 );
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) iknots[i] = riknots[k];
  double** d = getmatrix( n, n, 0.0 );
  size_t degree = *rdegree;
  bool anchor = ( *ranchor ) != 0;
  int knotstype = *rknotstype;
  size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = varsplwgtmds( n, delta, w, p, h, q, b, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rdelta[k] = delta[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  for ( size_t i = 1, k = 0; i <= ninner; i++, k++ ) riknots[k] = iknots[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = ( iszero( FCRIT ) ? 0.0 : lastdif );
  ( *rzdif ) = ( iszero( ZCRIT ) ? 0.0 : lastdif );
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( q );
  freematrix( b );
  freematrix( d );
  freevector( iknots );

} // Cvarsplwgtmds

void splmds_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test splmds at ", dt );
  printstring( "\n", "==============================================================\n" );

  randomize( &seed );
 
  size_t n = 20;
  size_t p = 2;
  size_t degree = 2;
  size_t ninner = 2;
  bool anchor = true;
  int knotstype = KNOTSTYPE.UNIFORM;
  double* iknots = getvector( ninner, 0.0 );

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
  splmds( n, delta, p, z, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, true );
  printscalar( "elapsed for splmds", getelapsedtime( tm ) );

  freematrix( delta );
  freematrix( w );
  freematrix( z );
  freeimatrix( fz );
  freematrix( d );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test splmds at ", dt );
  printstring( "\n", "==============================================================\n" );
} // unittest
