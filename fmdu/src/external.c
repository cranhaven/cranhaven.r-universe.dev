//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#include "fmdu.h"

static void pdist( const size_t n, double* d, double* r )
// create probabilities from distances
{
  double TOL = sqrt( DBL_EPSILON );
  double sm = sum( n, &d[1], 1 ) + ( double )( n ) * TOL;
  for ( size_t i = 1; i <= n; i++ ) r[i] = sm / ( d[i] + TOL );
  double alpha = sum( n, &r[1], 1 );
  if ( iszero( alpha ) ) alpha = TOL;
  scal( n, 1.0 / alpha, &r[1], 1 );
} // pdist

double external( const size_t n, const size_t m, double** delta, double** w, const size_t p, double** fixed, double** z, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo )
// Function extermal() performs external unfolding.
{
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000

  // allocate memory
  double* b = getvector( m, 0.0 );
  double* dwork = getvector( m, 0.0 );
  double fvalue = 0.0;

  // loop over objects (one at the time)
  for ( size_t i = 1; i <= n; i++ ) {

    // work with temporary delta
    for ( size_t j = 1; j <= m; j++ ) dwork[j] = delta[i][j];

    // init variables
    const double sumw = sum( m, &w[i][1], 1 );
    const double scale = wssq( m, &dwork[1], 1, &w[i][1], 1 );

    // initial z: probability weighted average
    if ( iszero( ssq( p, &z[i][1], 1 ) ) ) {
      double* probs = getvector( m, 0.0 );
      pdist( m, dwork, probs );
      for ( size_t k = 1; k <= p; k++ ) {
        double work = 0.0;
        for ( size_t j = 1; j <= m; j++ ) work += probs[j] * fixed[j][k];
        z[i][k] = work;
      }
      freevector( probs );
    }

    // administration: distances
    for ( size_t j = 1; j <= m; j++ ) {
      double sum = 0.0;
      for ( size_t k = 1; k <= p; k++ ) {
        const double diff = z[i][k] - fixed[j][k];
        if ( diff != 0.0 ) sum += diff * diff;
      }
      d[i][j] = sqrt( sum );
    }

    // administration: function value
    double fold = wnrm2( m, &dwork[1], 1, &d[i][1], 1, &w[i][1], 1 );
    fold /= scale;
    double fnew = 0.0;

    // echo intermediate results
    if ( echo == true ) echoprogress( 0, 1.0, 1.0, fold );

    // algorithm loop
    size_t iter = 0;
    for ( iter = 1; iter <= MAXITER; iter++ ) {

      // scale delta
      const double lower = wssq( m, &dwork[1], 1, &w[i][1], 1 );
      const double upper = dot( m, &dwork[1], 1, &d[i][1], 1 );
      const double alpha = ( lower < DBL_EPSILON ? 1.0 : upper / lower );
      scal( m, alpha, &dwork[1], 1 );

      // update configuration
      for ( size_t j = 1; j <= m; j++ ) b[j] = ( isnotzero( d[i][j] ) ? w[i][j] * dwork[j] / d[i][j] : 0.0 );

      // update: xtilde
      const double pi = sum( m, &b[1], 1 );
      for ( size_t k = 1; k <= p; k++ ) {
        const double by = dot( m, &b[1], 1, &fixed[1][k], p );
        double xtilde = pi * z[i][k] - by;
        for ( size_t j = 1; j <= m; j++ ) xtilde += w[i][j] * fixed[j][k];
        z[i][k] = xtilde / sumw;
      }

      // administration
      for ( size_t j = 1; j <= m; j++ ) {
        double sum = 0.0;
        for ( size_t k = 1; k <= p; k++ ) {
          const double diff = z[i][k] - fixed[j][k];
          if ( diff != 0.0 ) sum += diff * diff;
        }
        d[i][j] = sqrt( sum );
      }
      fnew = wnrm2( m, &dwork[1], 1, &d[i][1], 1, &w[i][1], 1 );
      fnew /= scale;

      // echo intermediate results
      if ( echo == true ) echoprogress( iter, fold, fold, fnew );

      // check divergence and convergence
      ( *lastdif ) = fold - fnew;
      if ( ( *lastdif ) <= -1.0 * CRIT ) break;
      const double fdif = 2.0 * ( *lastdif ) / ( fold + fnew );
      if ( fdif <= FCRIT ) break;
      fold = fnew;
    }
    ( *lastiter ) += iter;
    fvalue += fnew;
  }

  // de-allocate memory
  freevector( b );
  freevector( dwork );

  // return function value
  return fvalue;

} // external

void Cexternal( int* rn, int* rm, double* rdelta, double* rw, int* rp, double* rfixed, double* rz, double* rd, int* rmaxiter, double* rfdif, double* rfvalue, int* recho )
// Function Cexternal() performs external unfolding.
{
  // transfer to C
  const size_t n = *rn;
  const size_t m = *rm;
  double** delta = getmatrix( n, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  double** w = getmatrix( n, m, 1.0 );
  if ( isnull( w ) == false ) for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) w[i][j] = rw[k];
  size_t p = *rp;
  double** fixed = getmatrix( m, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= m; i++, k++ ) fixed[i][j] = rfixed[k];
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  double** d = getmatrix( n, m, 0.0 );
  const size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  bool echo = ( *recho ) != 0;

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  const double fvalue = external( n, m, delta, w, p, fixed, z, d, MAXITER, FCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate memory
  freematrix( delta );
  freematrix( w );
  freematrix( fixed );
  freematrix( z );
  freematrix( d );

} // Cexternal
