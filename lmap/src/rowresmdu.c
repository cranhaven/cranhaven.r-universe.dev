//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#include "flib.h"
#include "fmdu.h"


double rowresmdu( const size_t n, const size_t m, double** delta, const size_t p, const size_t h, double** q, double** b, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function rowresmdu() performs row restricted weighted multidimensional unfolding.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12

  // allocate memory
  double** x = getmatrix( n, p, 0.0 );
  double** imb = getmatrix( n, m, 0.0 );
  double** xtilde = getmatrix( n, p, 0.0 );
  double** ytilde = getmatrix( m, p, 0.0 );
  double** hhh = getmatrix( h, h, 0.0 );
  double** hhm = getmatrix( h, m, 0.0 );
  double** hhp = getmatrix( h, p, 0.0 );
  double** hmp = getmatrix( m, p, 0.0 );

  // initialization
  double wr = ( double ) ( m );
  double wc = ( double ) ( n );
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= m; j++ ) {
      const double work = delta[i][j];
      scale += work * work;
    }
  }
  for ( size_t i = 1; i <= h; i++ ) {
    for ( size_t j = 1; j <= h; j++ ) {
      double work = 0.0;
      for ( size_t k = 1; k <= n; k++ ) work += q[k][i] * wr * q[k][j];
      hhh[i][j] = work;
    }
  }
  inverse( h, hhh );
  for ( size_t k = 1; k <= h; k++ ) {
    double work = 0.0;
    for ( size_t i = 1; i <= n; i++ ) work += q[i][k];
    for ( size_t j = 1; j <= m; j++ ) hhm[k][j] = work;
  }
  int nfy = 0;
  for ( size_t j = 1; j <= m; j++ ) for ( size_t k = 1; k <= p; k++ ) nfy += fy[j][k];

  // update distances and calculate normalized stress
  gemm( false, false, n, p, h, 1.0, q, b, 0.0, x );
  euclidean2( n, p, x, m, y, d );
  double fold = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= m; j++ ) {
      const double work = delta[i][j] - d[i][j];
      fold += work * work;
    }
  }
  fold /= scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold );

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute B matrix
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) imb[i][j] = ( d[i][j] < TINY ? 0.0 : delta[i][j] / d[i][j] );
    }

    // compute preliminary updates: xtilde and xtilde
    for ( size_t i = 1; i <= n; i++ ) {
      double rsb = 0.0;
      for ( size_t k = 1; k <= m; k++ ) rsb += imb[i][k];
      for ( size_t j = 1; j <= p; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= m; k++ ) work += imb[i][k] * y[k][j];
        xtilde[i][j] = rsb * x[i][j] - work;
      }
    }
    for ( size_t i = 1; i <= m; i++ ) {
      double csb = 0.0;
      for ( size_t k = 1; k <= n; k++ ) csb += imb[k][i];
      for ( size_t j = 1; j <= p; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= n; k++ ) work += imb[k][i] * x[k][j];
        ytilde[i][j] = csb * y[i][j] - work;
      }
    }

    // update b
    gemm( false, false, h, p, m, 1.0, hhm, y, 0.0, hhp );
    for ( size_t i = 1; i <= h; i++ ) {
      for ( size_t j = 1; j <= p; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= n; k++ ) work += q[k][i] * xtilde[k][j];
        hhp[i][j] += work;
      }
    }
    gemm( false, false, h, p, h, 1.0, hhh, hhp, 0.0, b );

    // update x
    gemm( false, false, n, p, h, 1.0, q, b, 0.0, x );

    // update y
    for ( size_t k = 1; k <= p; k++ ) {
      double work = 0.0;
      for ( size_t i = 1; i <= n; i++ ) work += x[i][k];
      for ( size_t j = 1; j <= m; j++ ) hmp[j][k] = work;
    }
    for ( size_t i = 1; i <= m; i++ ) {
      for ( size_t j = 1; j <= p; j++ ) if ( fy[i][j] == 0 ) y[i][j] = ( ytilde[i][j] + hmp[i][j] ) / wc;
    }

    // update distances and calculate normalized stress
    euclidean2( n, p, x, m, y, d );
    fnew = 0.0;
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) {
        double work = delta[i][j] - d[i][j];
        fnew += work * work;
      }
    }
    fnew /= scale;

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fold, fnew );

    // check convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
    if ( fdif <= FCRIT ) break;
    fold = fnew;
  }
  ( *lastiter ) = iter;

  // rotate to principal axes of x
  if ( nfy == 0 ) rotateplusplus( n, p, x, h, b, m, y );

  // de-allocate memory
  freematrix( x );
  freematrix( imb );
  freematrix( xtilde );
  freematrix( ytilde );
  freematrix( hhh );
  freematrix( hhm );
  freematrix( hhp );
  freematrix( hmp );

  return( fnew );
} // rowresmdu

void Crowresmdu( int* rn, int* rm, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho )
// Function Crowresmdu() performs row restricted weighted multidimensional unfolding.
{
  // transfer to C
  size_t n = *rn;
  size_t m = *rm;
  size_t h = *rh;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  double** y = getmatrix( m, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= m; i++, k++ ) y[i][j] = ry[k];
  int** fy = getimatrix( m, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= m; i++, k++ ) fy[i][j] = rfy[k];
  double** d = getmatrix( n, m, 0.0 );
  double FCRIT = *rfdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = rowresmdu( n, m, delta, p, h, q, b, y, fy, d, MAXITER, FCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rq[k] = q[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= m; i++, k++ ) ry[k] = y[i][j];
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( q );
  freematrix( b );
  freematrix( y );
  freeimatrix( fy );
  freematrix( d );

} // Crowresmdu
