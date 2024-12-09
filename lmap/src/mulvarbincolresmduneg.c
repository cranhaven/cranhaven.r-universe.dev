//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "flib.h"
#include "fmdu.h"
#include "lmap.h"

double mulvarbincolresmduneg( const size_t n, const size_t r, double** y, const size_t m, double** u, const size_t p, double** x, double** b, const bool mains, double* mu, const size_t MAXINNER, const double FCRIT, const size_t MAXITER, const double DCRIT, size_t* lastiter, double* lastdif )
// mulvarbincolresmduneg() performs multivariate binary row restricted multidimensional unfolding.
{
  // constants
  const double EPS = DBL_EPSILON;   // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );   // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );  // 0.00012207031250000000

  // allocate memory
  double** v = getmatrix( r, m, 0.0 );
  double** q = getmatrix( n, r, 0.0 );
  double** d = getmatrix( n, r, 0.0 );
  double** theta = getmatrix( n, r, 0.0 );
  double** z = getmatrix( n, r, 0.0 );
  double** delta = getmatrix( n, r, 0.0 );
  int** fu = getimatrix( n, m, 0 );

  // compute initial q = 2 y - 1
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= r; j++ ) q[i][j] = 2.0 * y[i][j] - 1.0;
  }

  // compute initial mu = column means of 4Q
  if ( mains ) {
    for ( size_t j = 1; j <= r; j++ ) {
      double work = 0.0;
      for ( size_t i = 1; i <= n; i++ ) work += q[i][j];
      mu[j] = 4.0 * work / ( double ) ( n );
    }
  }

  // compute initial deviance
  gemm( false, false, r, m, p, 1.0, x, b, 0.0, v );
  euclidean2( n, m, u, r, v, d );
  for ( size_t j = 1; j <= r; j++ ) {
    double work = mu[j];
    for ( size_t i = 1; i <= n; i++ ) theta[i][j] = work - d[i][j];
  }
  double dold = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= r; j++ ) {
      const double work = q[i][j] * theta[i][j];
      dold += logl( plogis( work ) );
    }
  }
  dold *= -2.0;

  // start iterations
  size_t iter = 0;
  double ddif = 0.0;
  double dnew = 0.0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute working response
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= r; j++ ) {
        const double work = q[i][j] * theta[i][j];
        z[i][j] = theta[i][j] + 4.0 * q[i][j] * ( 1.0 - plogis( work ) );
      }
    }

    // update main effects
    if ( mains ) {
      for ( size_t j = 1; j <= r; j++ ) {
        double work = 0.0;
        for ( size_t i = 1; i <= n; i++ ) work += z[i][j] + d[i][j];
        mu[j] = work / ( double ) ( n );
      }
    }

    // update U and V
    for ( size_t j = 1; j <= r; j++ ) {
      const double work = mu[j];
      for ( size_t i = 1; i <= n; i++ ) delta[i][j] = work - z[i][j];
    }

    // nonnegative unfolding
    size_t inner = 0;
    double fdif = 0.0;
    colresmduneg( n, r, delta, m, u, fu, p, x, b, d, MAXINNER, FCRIT, &inner, &fdif, false );
    if ( fdif < -1.0 * CRIT ) break;

    // compute deviance
    for ( size_t j = 1; j <= r; j++ ) {
      double work = mu[j];
      for ( size_t i = 1; i <= n; i++ ) theta[i][j] = work - d[i][j];
    }
    dnew = 0.0;
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= r; j++ ) {
        const double work = q[i][j] * theta[i][j];
        dnew += logl( plogis( work ) );
      }
    }
    dnew *= -2.0;

    // check convergence
    ( *lastdif ) = dold - dnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    ddif = 2.0 * ( *lastdif ) / ( dold + dnew );
    if ( ddif <= DCRIT ) break;
    dold = dnew;
  }
  ( *lastiter ) = iter;

  // rotate solution to principal axes
  gemm( false, false, r, m, p, 1.0, x, b, 0.0, v );
  rotateplusplus( n, m, u, r, v, p, b );

  // de-allocate memory
  freematrix( v );
  freematrix( q );
  freematrix( d );
  freematrix( theta );
  freematrix( z );
  freematrix( delta );
  freeimatrix( fu );

  return( dnew );
} // Cmulvarbincolresmduneg

void Cmulvarbincolresmduneg( int* rn, int* rr, double* ry, int* rm, double* ru, int* rp, double* rx, double* rb, int* rmains, double* rmu, int* rmaxinner, double* rfcrit, int* rmaxiter, double* rdcrit, double* rdeviance )
// Cmulvarbincolresmduneg() performs multivariate binary row restricted multidimensional unfolding.
{
  // transfer to C
  const size_t n = *rn;
  const size_t r = *rr;
  const size_t m = *rm;
  const size_t p = *rp;
  double** y = getmatrix( n, r, 0.0 );
  for ( size_t j = 1, k = 0; j <= r; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) y[i][j] = ry[k];
  double** u = getmatrix( n, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) u[i][j] = ru[k];
  double** x = getmatrix( r, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= r; i++, k++ ) x[i][j] = rx[k];
  double** b = getmatrix( p, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= p; i++, k++ ) b[i][j] = rb[k];
  const bool mains = ( *rmains ) != 0;
  double* mu = getvector( r, 0.0 );
  const size_t MAXITER = *rmaxiter;
  const double DCRIT = *rdcrit;
  const size_t MAXINNER = *rmaxinner;
  const double FCRIT = *rfcrit;

  // analysis
  size_t lastiter = 0;
  double lastdif = 0.0;
  const double dnew = mulvarbincolresmduneg( n, r, y, m, u, p, x, b, mains, mu, MAXINNER, FCRIT, MAXITER, DCRIT, &lastiter, &lastdif );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) ru[k] = u[i][j];
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= p; i++, k++ ) rb[k] = b[i][j];
  for ( size_t i = 1, k = 0; i <= r; i++, k++ ) rmu[k] = mu[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rdcrit ) = lastdif;
  ( *rdeviance ) = dnew;

  // de-allocate memory
  freematrix( y );
  freematrix( u );
  freematrix( x );
  freematrix( b );
  freevector( mu );

} // Cmulvarbincolresmduneg
