//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

static size_t binsearch( const size_t n, double* a, const double z )
// return position of z in a based on binary search
{
  size_t lo = 0;
  size_t hi = n - 1;
  while ( hi - lo > 1 ) {
    size_t mid = ( hi + lo ) >> 1;
    if ( z < a[mid] ) hi = mid;
    else lo = mid;
  }
  return ( z < a[lo] ? lo : lo + 1 );
} // binsearch

static double kdistance( const size_t n, double* rdelta, const size_t k )
// return k-distance
{
  double* __restrict pdelta = &rdelta[0];
  double* __restrict values = ( double* ) calloc( k, sizeof( double ) );
  memcpy( &values[0], &pdelta[0], k * sizeof( double ) );                 // use first k distances first
  dsort0( k, &values[-1] );                                               // sort first k distances
  double boundary = values[k-1];
  for ( size_t i = k; i < n; i++ ) {
    const double val = pdelta[i];
    if ( val > boundary ) continue;                                              // new distance not in first k
    const size_t pos = binsearch( k, values, val );                              // find position new distance
    memmove( &values[pos+1], &values[pos], ( k - pos - 1) * sizeof( double ) );  // shift larger distances
    values[pos] = val;                                                           // insert new distance
    boundary = values[k-1];                                                      // set new boundary 
  }
  free( values );
  return( boundary + DBL_EPSILON );
} // kdistance

static double standarddistance( const size_t n, double* rdelta, const double boundary )
// return root mean sum-of-squares smaller equal to k-distance (based on LOOP)
{
  double* __restrict pdelta = &rdelta[0];
  double sumd = 0.0;
  size_t nsumd = 0;
  for ( size_t i = 0; i < n; i++ ) {
    const double val = pdelta[i];
    if ( val <= boundary ) {       // if value smaller or equal to boundary
      sumd += val * val;           // sum-of-squares
      nsumd++;                     // number of ...
    }
  }
  return( sqrt( sumd / ( double )( nsumd ) ) );  // return root mean sum-of-squares
}

static double outlierfactor( const size_t n, double* rdelta, double* boundary, double* pd, const size_t i )
// return mean maximum of shared neighborhood unless number of equals zero (based on RDOF)
{
  double* __restrict pdelta = &rdelta[0];
  double* __restrict pboundary = &boundary[0];
  double* __restrict ppd = &pd[0];
  double mx = 0.0;
  size_t nRNS = 0;
  for ( size_t j = 0; j < n; j++ ) {
    const double val = pdelta[j];
    if ( val <= pboundary[i] && val <= pboundary[j] ) {  // if value smaller or equal to boundaries of both i and j
      nRNS++;                                            // number of ...
      if ( ppd[j] > mx ) mx = ppd[j];                    // save maximum
    }
  }
  return( nRNS == 0 ? pd[i] : mx / ( double )( nRNS ) );  // return mean maximum unless number of equals zero
}

void Crdop( int* rn, double* rdelta, int* rk, double* rlambda, double* rscores )
{
  const size_t n = *rn;
  const size_t k = *rk;
  const double LAMBDA = *rlambda;

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pscores = &rscores[0];

  double* __restrict boundary = ( double* ) calloc( n, sizeof( double ) );  // k-distances
  double* __restrict pd = ( double* ) calloc( n, sizeof( double ) );        // probabilistic distances

  // compute boundary k-distance
  // compute probabilistic distance based on standard distance
  for ( size_t i = 0, j = 0; i < n; i++, j += n ) {
    pdelta[j+i] = DBL_MAX;
    boundary[i] = kdistance( n, &pdelta[j], k );                      // sort and loop with binary search
    pd[i] = LAMBDA * standarddistance( n, &pdelta[j], boundary[i] );  // loop for root mean sum-of-squares
  }

  // compute modified relative density-based outlier factor
  // aggregate values for normalization, based on LOOP
  double nmrdof = 0.0;
  for ( size_t i = 0, j = 0; i < n; i++, j += n ) {
    const double work = pscores[i] = outlierfactor( n, &pdelta[j], boundary, pd, i );  // loop for mean maximum
    nmrdof += work * work;
  }
  nmrdof = LAMBDA * sqrt( nmrdof / ( double )( n ) );

  // compute relative density-based outlier probabilities, based on LOOP
  nmrdof *= sqrt( 2.0 );
  for ( size_t i = 0; i < n; i++ ) {
    const double ratio = pscores[i] / nmrdof;
    pscores[i] = fmax( 0.0, erf( ratio ) );
  }  

//  printvector( "pscores", n, &pscores[-1] );

  free( boundary );
  free( pd );

} // Crdop

void Cerdop( int* rn, double* rdelta, int* rk, double* rlambda, double* rw, double* ralpha, double* rbeta )
{
  const size_t n = *rn;

  const double alpha = fmax( 0.0, fmin( 1.0, *ralpha ) );
  const double beta = fmax( 0.0, fmin( 1.0, *rbeta ) );

  double* __restrict pdelta = &rdelta[0];
  double* __restrict pw = &rw[0];

  size_t nn_t = n * ( n - 1 ) / 2;
  double* __restrict big = ( double* ) calloc( nn_t * nn_t, sizeof( double ) );
  for ( size_t i = 0, k = 0; i < n-1; i++ ) {
    for ( size_t j = i+1; j < n; j++, k++ ) {
      const double p = pdelta[i*n+j];
      for ( size_t ii = 0, kk = 0; ii < n-1; ii++ ) {
        for ( size_t jj = ii+1; jj < n; jj++, kk++ ) {
          const double d = fabs( p - pdelta[ii*n+jj] );
          big[k*nn_t+kk] = d;
          big[kk*nn_t+k] = d;
        }
      }
    }
  }  

  int nn = ( int )( nn_t );
  double* __restrict scores = ( double* ) calloc( nn_t, sizeof( double ) );
  Crdop( &nn, big, rk, rlambda, scores );

  for ( size_t i = 0, k = 0; i < n-1; i++ ) {
    for ( size_t j = i+1; j < n; j++, k++ ) {
      const double score = scores[k];
      const double w = 1.0 + exp( 100.0 * alpha * ( score - beta ) );
      pw[i*n+j] = pw[j*n+i] = 1.0 / w;
    }
  }  
  for ( size_t i = 0, k = 0; i < n; i++, k += (n+1) ) pw[k] = 0.0;

  free( big );
  free( scores );
} // Cerdop

void rdop_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test rdop at ", dt );
  printstring( "\n", "==============================================================\n" );

  randomize( &seed );

  size_t n = 0;
  size_t m = 0; 
  double** fulldata = readmatrix( "iris1234.txt", &n, &m );
  n = 25;
  double** idata = getmatrix( n, m, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= 4; j++ ) idata[i][j] = fulldata[i][j];
  freematrix( fulldata );
  double** id = getmatrix( n, n, 0.0 );
  euclidean1( n, m, idata, id );

  int intn = ( int )( n );
  int intk = ( int )( sqrt( 2 * n ) );
  double* iscores = getvector( n, 0.0 );
  double** rw = getmatrix( n, n, 0.0 );
  double ralpha = 0.2;
  double rbeta = 0.25;
  double lambda = 3.0;
  intk = 9;
  size_t tm = setstarttime();
//  Crdop( &intn, &id[1][1], &intk, &lambda, &iscores[1] );
  Cerdop( &intn, &id[1][1], &intk, &lambda, &rw[1][1], &ralpha, &rbeta );

  printscalar( "elapsed for Crdop", getelapsedtime( tm ) );
  printvector( "scores = ", n, iscores );

  double** tdata = readmatrix( "tortula.dat", &n, &m );
  double** td = getmatrix( n, n, 0.0 );
  euclidean1( n, m, tdata, td );

  intn = ( int )( n );
  intk = ( int )( sqrt( 2 * n ) );
  double* tscores = getvector( n, 0.0 );
  lambda = 3.0;

  tm = setstarttime();
  Crdop( &intn, &td[1][1], &intk, &lambda, &tscores[1] );
  printscalar( "elapsed for Crdop", getelapsedtime( tm ) );
  printvector( "scores = ", n, tscores );

  double alpha = 0.2;
  double beta = 0.25;    
  double** tw = getmatrix( n, n, 0.0 );

  tm = setstarttime();
  Cerdop( &intn, &td[1][1], &intk, &lambda, &tw[1][1], &alpha, &beta );
  printscalar( "elapsed for Cerdop", getelapsedtime( tm ) );
  printmatrix( "w = ", n, n, tw );

  freematrix( tdata );
  freematrix( td );
  freevector( tscores );
  freematrix( tw );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test rdop at ", dt );
  printstring( "\n", "==============================================================\n" );
} // unittest
