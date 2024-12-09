//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "flib.h"
#include "fmdu.h"
#include "lmap.h"

double mulnomrowresmduneg( const size_t n, const size_t c, double** g, const size_t p, double** x, const size_t m, double** b, double** v, double** u, double** theta, const size_t MAXINNER, const double FCRIT, const size_t MAXITER, const double DCRIT, size_t* lastiter, double* lastdif )
// mulnomrowresmduneg() performs multinomial row restricted unfolding allowing negative dissimilarities.
{
  // constants
  const double EPS = DBL_EPSILON;   // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );   // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );  // 0.00012207031250000000

  // allocate memory
  double** pi = getmatrix( n, c, 0.0 );
  double** delta = getmatrix( n, c, 0.0 );
  int** fv = getimatrix( c, m, 0 );

  // initialization: u, based on x and b, and pi, based on theta
  gemm( false, false, n, m, p, 1.0, x, b, 0.0, u );
  euclidean2( n, m, u, c, v, theta );
  for ( size_t i = 1; i <= n; i++ ) {
    double sum = 0.0;
    for ( size_t j = 1; j <= c; j++ ) sum += pi[i][j] = exp( -1.0 * theta[i][j] );
    for ( size_t j = 1; j <= c; j++ ) pi[i][j] /= sum;
  }

  // compute old deviance
  double dold = 0.0;
  for ( size_t i = 1; i <= n; i++ ) {
    for ( size_t j = 1; j <= c; j++ ) dold += g[i][j] * logl( pi[i][j] );
  }
  dold *= -2.0;

  // start iterations
  double ddif = 0.0;
  double dnew = 0.0;
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // compute delta based on current distances (theta) and pi 
    //bool negs = false;
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= c; j++ ) {
        delta[i][j] = theta[i][j] - 4.0 * ( g[i][j] - pi[i][j] );
        //if ( delta[i][j] < 0.0 ) negs = true;
      }
    }

    // row restricted mdu allowing negative dissimilarities or not (faster)
    size_t inner = 0;
    double fdif = 0.0;
    rowresmduneg( n, c, delta, m, p, x, b, v, fv, theta, MAXINNER, FCRIT, &inner, &fdif, false );
    //if ( negs == true ) rowresmduneg( n, c, delta, m, p, x, b, v, theta, MAXINNER, FCRIT, &inner, &fdif );
    //else rowresmdu( n, c, delta, m, p, x, b, v, theta, MAXINNER, FCRIT, &inner, &fdif );
    if ( fdif < -1.0 * CRIT ) break;

    // compute new pi
    for ( size_t i = 1; i <= n; i++ ) {
      double sum = 0.0;
      for ( size_t j = 1; j <= c; j++ ) sum += pi[i][j] = exp( -1.0 * theta[i][j] );
      for ( size_t j = 1; j <= c; j++ ) pi[i][j] /= sum;
    }

    // compute new deviance
    dnew = 0.0;
    for ( size_t i = 1; i <= n; i++ ) {
      for ( size_t j = 1; j <= c; j++ ) dnew += g[i][j] * logl( pi[i][j] );
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
  gemm( false, false, n, m, p, 1.0, x, b, 0.0, u );
  rotateplusplus( n, m, u, c, v, p, b );

  // de-allocate memory
  freematrix( pi );
  freematrix( delta );
  freeimatrix( fv );

  return( dnew );
} // mulnomrowresrmduneg

void Cmulnomrowresmduneg( int* rn, int* rc, double* rg, int* rp, double* rx, int* rm, double* rb, double* rv, double* ru, double* rtheta, int* rmaxinner, double* rfcrit, int* rmaxiter, double* rdcrit, double* rdeviance )
// Cmulnomrowresmduneg() performs multinomial row restricted unfolding allowing negative dissimilarities.
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const size_t c = *rc;
  const size_t m = *rm;
  double** g = getmatrix( n, c, 0.0 );
  for ( size_t j = 1, k = 0; j <= c; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) g[i][j] = rg[k];
  double** x = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) x[i][j] = rx[k];
  double** b = getmatrix( p, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= p; i++, k++ ) b[i][j] = rb[k];
  double** v = getmatrix( c, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= c; i++, k++ ) v[i][j] = rv[k];
  double** u = getmatrix( n, m, 0.0 );
  double** theta = getmatrix( n, c, 0.0 );
  const size_t MAXITER = *rmaxiter;
  const double DCRIT = *rdcrit;
  const size_t MAXINNER = *rmaxinner;
  const double FCRIT = *rfcrit;

  // analysis
  size_t lastiter = 0;
  double lastdif = 0.0;
  const double dnew = mulnomrowresmduneg( n, c, g, p, x, m, b, v, u, theta, MAXINNER, FCRIT, MAXITER, DCRIT, &lastiter, &lastdif );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= p; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= c; i++, k++ ) rv[k] = v[i][j];
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) ru[k] = u[i][j];
  for ( size_t j = 1, k = 0; j <= c; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rtheta[k] = theta[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rdcrit ) = lastdif;
  ( *rdeviance ) = dnew;

  // de-allocate memory
  freematrix( g );
  freematrix( x );
  freematrix( b );
  freematrix( v );
  freematrix( u );
  freematrix( theta );

} // Cmulnomrowresrmduneg
