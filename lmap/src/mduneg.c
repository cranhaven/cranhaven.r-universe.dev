//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#include "flib.h"
#include "fmdu.h"


double mduneg( const size_t n, const size_t m, double** delta, const size_t p, double** x, int** fx, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function mduneg() performs multidimensional unfolding allowing negative dissimilarities.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000
  const double TINY = pow( 10.0, ( log10( EPS ) + log10( TOL ) ) / 2.0 );  // 1.8189894035458617e-12
  const double DISCRIT = TINY;
  const double EPSCRIT = 0.25 * TINY;

  // allocate memory
  double** imb = getmatrix( n, m, 0.0 );
  double** imw = getmatrix( n, m, 0.0 );
  double* wr = getvector( n, 0.0 );
  double* wc = getvector( m, 0.0 );
  double** xtilde = getmatrix( n, p, 0.0 );
  double** ytilde = getmatrix( m, p, 0.0 );

  // initialization
  double scale = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= m; j++ ) {
      const double work = delta[i][j];
      scale += work * work;
    }
  }
  int nfx = 0;
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) nfx += fx[i][k];
  int nfy = 0;
  for ( size_t j = 1; j <= m; j++ ) for ( size_t k = 1; k <= p; k++ ) nfy += fy[j][k];

  // update distances and calculate normalized stress
  euclidean2( n, p, x, m, y, d );
  double fold = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= m; j++ ) {
      double work = delta[i][j] - d[i][j];
      fold += work * work;
    }
  }
  fold /= scale;
  double fnew = 0.0;

  // echo intermediate results
  if ( echo == true ) echoprogress( 0, fold, fold, fold );

  // start unfolding loop
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

    // configuration update: x and y
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t k = 1; k <= p; k++ ) if ( fx[i][k] == 0 ) {
        double upper = xtilde[i][k];
        for ( size_t j = 1; j <= m; j++ ) upper += imw[i][j] * y[j][k];
        const double lower = wr[i];
        if ( isnotzero( lower ) ) x[i][k] = upper / lower;
      }
    }
    for ( size_t j = 1; j <= m; j++ ) {
      for ( size_t k = 1; k <= p; k++ ) if ( fy[j][k] == 0 ) {
        double upper = ytilde[j][k];
        for ( size_t i = 1; i <= n; i++ ) upper += imw[i][j] * x[i][k];
        const double lower = wc[j];
        if ( isnotzero( lower ) ) y[j][k] = upper / lower;
      }
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
  if ( nfx == 0 && nfy == 0 ) rotateplus( n, p, x, m, y );

  // de-allocate memory
  freematrix( imb );
  freematrix( imw );
  freevector( wr );
  freevector( wc );
  freematrix( xtilde );
  freematrix( ytilde );

  return( fnew );
} // mduneg

void Cmduneg( int* rn, int* rm, double* rdelta, int* rp, double* rx, int* rfx, double* ry, int* rfy, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho )
// Function Cmduneg() performs multidimensional unfolding allowing negative dissimilarities.
{
  // transfer to C
  size_t n = *rn;
  size_t m = *rm;
  size_t p = *rp;
  size_t MAXITER = *rmaxiter;
  double** delta = getmatrix( n, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** x = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) x[i][j] = rx[k];
  int** fx = getimatrix( n, p, 0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) fx[i][j] = rfx[k];
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
  double fvalue = mduneg( n, m, delta, p, x, fx, y, fy, d, MAXITER, FCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rx[k] = x[i][j];
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= m; i++, k++ ) ry[k] = y[i][j];
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( x );
  freeimatrix( fx );
  freematrix( y );
  freeimatrix( fy );
  freematrix( d );

} // Cmduneg
