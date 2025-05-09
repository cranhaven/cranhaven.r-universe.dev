//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

void rcvpenvarmds( const size_t NREPEATS, const size_t NFOLDS, const size_t NLAMBDA, double* lambda, const double alpha, const bool grouped,
                   const size_t n, double** delta, const size_t p, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, const double ZCRIT, const bool echo,
                   double* mserror, double* sdev, double* stderror ) 
{
  // allocate memory
  double** z = getmatrix( n, p, 0.0 );
  size_t* idx = getvector_t( n, 0 );
  size_t* fsizes = getvector_t( NFOLDS, 0 );
  double** btrain = getmatrix( h, p, 0.0 );
  double* pe = getvector( NREPEATS * NFOLDS, 0.0 );

  // initial z and b by restricted classical scasling
  for ( size_t i = 1; i <= n; i++ ) idx[i] = i;
  for ( size_t i = 0; i < n; i++ ) fsizes[1 + ( i % NFOLDS )]++;

  // loop over lambda
  for ( size_t l = 1; l <= NLAMBDA; l++ ) {

    // initialize
    size_t lastiter = 0;
    double lastdif = 0.0;
    const double current = lambda[l];
    respcoa( n, delta, h, q, p, 0.0, b, z );
    penvarmds( n, delta, p, h, q, b, current, alpha, grouped, d, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

    // repeats
    for ( size_t repl = 1, vals = 1; repl <= NREPEATS; repl++ ) {

      // loop over folds
      permutate_t( n, idx );
      for ( size_t fold = 1; fold <= NFOLDS; fold++, vals++ ) {

        // set data sizes
        const size_t ntest = fsizes[fold];
        const size_t ntrain = n - ntest;

        // set training data
        double** deltatrain = getmatrix( ntrain, ntrain, 0.0 );
        double** qtrain = getmatrix( ntrain, h, 0.0 );
        for ( size_t i = 1; i <= ntrain; i++ ) {
          const size_t ii = idx[i];
          for ( size_t k = 1; k <= h; k++ ) qtrain[i][k] = q[ii][k];
          for ( size_t j = 1; j <= ntrain; j++ ) {
            const size_t jj = idx[j];
            deltatrain[i][j] = delta[ii][jj];
          }
        }
        double** dtrain = getmatrix( ntrain, ntrain, 0.0 );
        dcopy( h * p, &b[1][1], 1, &btrain[1][1], 1 );

        // set test data
        double** deltatest = getmatrix( ntest, ntest, 0.0 );
        double** qtest = getmatrix( ntest, ntest, 0.0 );
        for ( size_t i = 1; i <= ntest; i++ ) {
          const size_t ii = idx[ntrain+i];
          for ( size_t k = 1; k <= h; k++ ) qtest[i][k] = q[ii][k];
          for ( size_t j = 1; j <= ntest; j++ ) {
            const size_t jj = idx[ntrain+j];
            deltatest[i][j] = delta[ii][jj];
          }
        }
        double** ztest = getmatrix( ntest, ntest, 0.0 );
        double** dtest = getmatrix( ntest, ntest, 0.0 );
      
        size_t lastiter = 0;
        double lastdif = 0.0;
        penvarmds( ntrain, deltatrain, p, h, qtrain, btrain, current, alpha, grouped, dtrain, MAXITER, FCRIT, ZCRIT, &lastiter, &lastdif, echo );

        dgemm( false, false, ntest, p, h, 1.0, qtest, btrain, 0.0, ztest );
        euclidean1( ntest, p, ztest, dtest );
        pe[vals] = dsse( ntest * ntest, &deltatest[1][1], 1, &dtest[1][1], 1 ) / ( double )( ntest );
  
        for ( size_t i = 1; i <= ntest; i++ ) {
          const size_t k = idx[1];
          for ( size_t j = 2; j <= n; j++ ) idx[j-1] = idx[j];
          idx[n] = k;
        }

        // de-allocate temporary memory
        freematrix( deltatrain );
        freematrix( qtrain );
        freematrix( dtrain );
        freematrix( deltatest );
        freematrix( qtest );
        freematrix( ztest );
        freematrix( dtest );
      }
    }
 
    // return mean squared error and corresponding standard error
    mserror[l] = dsum( NREPEATS * NFOLDS, &pe[1], 1 ) / ( double )( NREPEATS * NFOLDS );
    sdev[l] = stddev( NREPEATS * NFOLDS, &pe[1], 1 );
    stderror[l] = sdev[l] / sqrt( ( double )( NREPEATS * NFOLDS ) );
  }

  // de-allocate memory
  freematrix( z );
  freevector_t( idx );
  freevector_t( fsizes );
  freematrix( btrain );
  freevector( pe );

} // rcvpenvarmds

void Crcvpenvarmds( int* rnrepeats, int* rnfolds, int* rnlambda, double* rlambda, double* ralpha, int* rgrouped,
                    int* rn, double* rdelta, int* rp, int* rh, double* rq, double* rb, double* rd, int* rmaxiter, double* rfdif, double* rzdif, int* recho,
                    double* rmserror, double* rstddev, double* rstderror )
// Function Crcvpenvarmds() performs repeated cross validation penalized variable restricted multidimensional scaling.
{
  // transfer to C
  const size_t NREPEATS = *rnrepeats;
  const size_t NFOLDS = *rnfolds;
  const size_t NLAMBDA = *rnlambda;
  double* lambda = getvector( NLAMBDA, 0.0 );
  for ( size_t i = 1, k = 0; i <= NLAMBDA; i++, k++ ) lambda[i] = rlambda[k];
  const double alpha = *ralpha;
  const bool grouped = ( *rgrouped ) != 0;

  const size_t n = *rn;
  double** delta = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) delta[i][j] = rdelta[k];
  const size_t p = *rp;
  const size_t h = *rh;
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  double** b = getmatrix( h, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) b[i][j] = rb[k];
  double** d = getmatrix( n, n, 0.0 );
  const size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const double ZCRIT = *rzdif;
  const bool echo = ( *recho ) != 0;

  double* mserror = getvector( NLAMBDA, 0.0 );
  double* stddev = getvector( NLAMBDA, 0.0 );
  double* stderror = getvector( NLAMBDA, 0.0 );

  // run function
  rcvpenvarmds( NREPEATS, NFOLDS, NLAMBDA, lambda, alpha, grouped, n, delta, p, h, q, b, d, MAXITER, FCRIT, ZCRIT, echo, mserror, stddev, stderror );

  // transfer to R
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rd[k] = d[i][j];
  for ( size_t i = 1, k = 0; i <= NLAMBDA; i++, k++ ) rmserror[k] = mserror[i];
  for ( size_t i = 1, k = 0; i <= NLAMBDA; i++, k++ ) rstddev[k] = stddev[i];
  for ( size_t i = 1, k = 0; i <= NLAMBDA; i++, k++ ) rstderror[k] = stderror[i];

  // de-allocate memory
  freevector( lambda );
  freematrix( delta );
  freematrix( q );
  freematrix( b );
  freematrix( d );
  freevector( mserror );
  freevector( stddev );
  freevector( stderror );

} // Crcvpenvarmds
