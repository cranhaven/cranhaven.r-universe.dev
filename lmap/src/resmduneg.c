//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#include "flib.h"
#include "fmdu.h"


double resmduneg( const size_t n, const size_t m, double** delta, const size_t p, const size_t hx, double** qx, double** bx, const size_t hy, double** qy, double** by, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function resmduneg() performs row restricted multidimensional unfolding.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double** x = getmatrix( n, p, 0.0 );
  double** y = getmatrix( m, p, 0.0 );
  double** imb = getmatrix( n, m, 0.0 );
  double** imw = getmatrix( n, m, 0.0 );
  double** xtilde = getmatrix( n, p, 0.0 );
  double** ytilde = getmatrix( m, p, 0.0 );
  double* wr = getvector( n, 0.0 );
  double* wc = getvector( m, 0.0 );
  double** hxx = getmatrix( hx, hx, 0.0 );
  double** hhm = getmatrix( hx, m, 0.0 );
  double** hhp = getmatrix( hx, p, 0.0 );
  double** hmp = getmatrix( m, p, 0.0 );
  double** hyy = getmatrix( hy, hy, 0.0 );
  double** hhn = getmatrix( hy, m, 0.0 );
  double** hnp = getmatrix( m, p, 0.0 );

  // initialization
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= m; j++ ) {
      const double work = delta[i][j];
      scale += work * work;
    }
  }

  // update distances and calculate normalized stress
  gemm( false, false, n, p, hx, 1.0, qx, bx, 0.0, x );
  gemm( false, false, m, p, hy, 1.0, qy, by, 0.0, y );
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

    // compute original B and W matrices, based on Heiser (1989)
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= m; j++ ) {
        imb[i][j] = ( delta[i][j] < 0.0 || d[i][j] < DISCRIT ? 0.0 : delta[i][j] / d[i][j] );
        if ( delta[i][j] < 0.0 ) {
          if ( d[i][j] < DISCRIT ) {
            const double work = fabs( delta[i][j] );
            imw[i][j] = ( EPSCRIT + work * work ) / EPSCRIT;
          }
          else imw[i][j] = ( d[i][j] + fabs( delta[i][j] ) ) / d[i][j];
        }
        else imw[i][j] = 1.0;
      }
    }
    for ( size_t i = 1; i <= n; i++ ) {
      double work = 0.0;
      for ( size_t j = 1; j <= m; j++ ) work += imw[i][j];
      wr[i] = work;
    }
    for ( size_t j = 1; j <= m; j++ ) {
      double work = 0.0;
      for ( size_t i = 1; i <= n; i++ ) work += imw[i][j];
      wc[j] = work;
    }

    // compute preliminary updates: xtilde and ytilde
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

    // update bx
    for ( size_t i = 1; i <= hx; i++ ) {
      for ( size_t j = 1; j <= hx; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= n; k++ ) work += qx[k][i] * wr[k] * qx[k][j];
        hxx[i][j] = work;
      }
    }
    inverse( hx, hxx );
    gemm( true, false, hx, m, n, 1.0, qx, imw, 0.0, hhm );
    gemm( false, false, hx, p, m, 1.0, hhm, y, 0.0, hhp );
    for ( size_t i = 1; i <= hx; i++ ) {
      for ( size_t j = 1; j <= p; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= n; k++ ) work += qx[k][i] * xtilde[k][j];
        hhp[i][j] += work;
      }
    }
    gemm( false, false, hx, p, hx, 1.0, hxx, hhp, 0.0, bx );

    // update x
    gemm( false, false, n, p, hx, 1.0, qx, bx, 0.0, x );

    // update by
    for ( size_t i = 1; i <= hy; i++ ) {
      for ( size_t j = 1; j <= hy; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= m; k++ ) work += qy[k][i] * wc[k] * qy[k][j];
        hyy[i][j] = work;
      }
    }
    inverse( hy, hyy );
    gemm( true, true, hy, n, m, 1.0, qy, imw, 0.0, hhn );
    gemm( false, false, hy, p, n, 1.0, hhn, x, 0.0, hhp );
    for ( size_t i = 1; i <= hy; i++ ) {
      for ( size_t j = 1; j <= p; j++ ) {
        double work = 0.0;
        for ( size_t k = 1; k <= m; k++ ) work += qy[k][i] * ytilde[k][j];
        hhp[i][j] += work;
      }
    }
    gemm( false, false, hy, p, hy, 1.0, hyy, hhp, 0.0, by );

    // update y
    gemm( false, false, m, p, hy, 1.0, qy, by, 0.0, y );

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
  rotateplusplus( n, p, x, hx, bx, hy, by );

  // de-allocate memory
  freematrix( x );
  freematrix( y );
  freematrix( imb );
  freematrix( imw );
  freematrix( xtilde );
  freematrix( ytilde );
  freevector( wr );
  freevector( wc );
  freematrix( hxx );
  freematrix( hhm );
  freematrix( hhp );
  freematrix( hmp );
  freematrix( hyy );
  freematrix( hhn );
  freematrix( hnp );

  return( fnew );
} // resmduneg

void Cresmduneg( int* rn, int* rm, double* rdelta, int* rp, int* rhx, double* rqx, double* rbx, int* rhy, double* rqy, double* rby, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho )
// Function Cresmduneg() performs row restricted multidimensional unfolding.
{
  // transfer to C
  size_t n = *rn;
  size_t m = *rm;
  size_t hx = *rhx;
  size_t hy = *rhy;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** qx = getmatrix( n, hx, 0.0 );
  for ( size_t j = 1, k = 0; j <= hx; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) qx[i][j] = rqx[k];
  double** bx = getmatrix( hx, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= hx; i++, k++ ) bx[i][j] = rbx[k];
  double** qy = getmatrix( n, hy, 0.0 );
  for ( size_t j = 1, k = 0; j <= hy; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) qy[i][j] = rqy[k];
  double** by = getmatrix( hy, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= hy; i++, k++ ) by[i][j] = rby[k];
  double** d = getmatrix( n, m, 0.0 );
  double FCRIT = *rfdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  double fvalue = resmduneg( n, m, delta, p, hx, qx, bx, hy, qy, by, d, MAXITER, FCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= hx; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rqx[k] = qx[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= hx; i++, k++ ) rbx[k] = bx[i][j];
  for ( size_t j = 1, k = 0; j <= hy; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rqy[k] = qy[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= hy; i++, k++ ) rby[k] = by[i][j];
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( qx );
  freematrix( bx );
  freematrix( qy );
  freematrix( by );
  freematrix( d );

} // Cresmduneg
