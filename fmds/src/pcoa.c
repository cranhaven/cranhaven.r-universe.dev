//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

static void scalarproducts( const size_t n, double** d, double** b )
// square, double center, and multiply with -0.5
{
  double tsum = 0.0;
  double* h = getvector( n, 0.0 );
  for ( size_t i = 2; i <= n; i++ ) {
    for ( size_t j = 1; j <= i - 1; j++ ) {
      const double d2 = b[i][j] = d[i][j] * d[i][j];
      tsum += d2;
      h[i] += d2;
      h[j] += d2;
    }
  }
  tsum = 2.0 * tsum / ( double )( n * n );
  for ( size_t i = 1; i <= n; i++ ) h[i] /= ( double )( n );
  b[1][1] = h[1] - 0.5 * tsum;
  for ( size_t i = 2; i <= n; i++ ) {
    b[i][i] = h[i] - 0.5 * tsum;
    for ( size_t j = 1; j <= i - 1; j++ ) b[j][i] = b[i][j] = -0.5 * ( b[i][j] - h[i] - h[j] + tsum );
  }
  freevector( h );
} // scalarproducts

int pcoa( const size_t n, double** d, const size_t p, const double ac, double** z )
// classical scaling or principal co-ordinates analysis: see Gower (1966)
// distance matrix in d has dimensions n x n
// requested dimensionality in p and resulting coordinates in z which is n x p
{
  int result = 1;

  // allocate memory
  double** b = getmatrix( n, n, 0.0 );
  double** evecs = getmatrix( n, n, 0.0 );
  double* evals = getvector( n, 0.0 );

  // add constant when different from zero
  if ( isnotzero( ac ) ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j <= i - 1; j++ ) b[j][i] = b[i][j] = d[i][j] + ac;
  else dcopy( n * n, &d[1][1], 1, &b[1][1], 1 );

  // square, double center, and multiply with -0.5
  scalarproducts( n, b, evecs );

  // eigenvalue decomposition
  result = evdcmp( n, evecs, evals );
  if ( result == 0 ) {
    dset( n * p, 0.0, &z[1][1], 1 );
    for ( size_t k = 1; k <= p; k++ ) {
      double alpha = ( evecs[1][k] < 0.0 ? -1.0 : 1.0 ) * sqrt( fmax( 0.0, evals[k] ) );
      daxpy( n, alpha, &evecs[1][k], n, &z[1][k], p );
    }
  }

  // de-allocate memory
  freematrix( b );
  freematrix( evecs );
  freevector( evals );

  return result;
} // pcoa

void Cpcoa( int* rn, double* rd, int* rp, double* rac, double* rz )
// Function Cpcoa() performs principal coordinate analysis or classical multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  double** d = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) d[i][j] = rd[k];
  const double ac = *rac;
  double** z = getmatrix( n, p, 0.0 );

  // run function
  int rvalue = pcoa( n, d, p, ac, z );

  // transfer to R
  if ( rvalue == 0 ) for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];

  // de-allocate memory
  freematrix( d );
  freematrix( z );

} // Cpcoa

int fastpcoa( const size_t n, double** d, const size_t p, const double ac, double** z )
// classical scaling or principal co-ordinates analysis: see Gower (1966)
// distance matrix in d has dimensions n x n
// requested dimensionality in p and resulting coordinates in z which is n x p
{
  int result = 1;

  // allocate memory
  double** b = getmatrix( n, n, 0.0 );
  double** evecs = getmatrix( n, n, 0.0 );
  double* evals = getvector( n, 0.0 );

  // add constant when different from zero
  if ( isnotzero( ac ) ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j <= i - 1; j++ ) b[j][i] = b[i][j] = d[i][j] + ac;
  else dcopy( n * n, &d[1][1], 1, &b[1][1], 1 );

  // square, double center, and multiply with -0.5
  scalarproducts( n, b, b );

  // eigenvalue decomposition
  result = evdcmp_lanczos( n, b, evecs, evals, p );
  if ( result == 0 ) {
    for ( size_t k = 1; k <= p; k++ ) {
      double alpha = ( evecs[1][k] < 0.0 ? -1.0 : 1.0 ) * sqrt( fmax( 0.0, evals[k] ) );
      daxpy( n, alpha, &evecs[1][k], n, &z[1][k], p );
    }
  }

  // de-allocate memory
  freematrix( b );
  freematrix( evecs );
  freevector( evals );

  return result;
} // fastpcoa

void Cfastpcoa( int* rn, double* rd, int* rp, double* rac, double* rz )
// Function Cpcoa() performs principal coordinate analysis or classical multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t p = *rp;
  double** d = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) d[i][j] = rd[k];
  const double ac = *rac;
  double** z = getmatrix( n, p, 0.0 );

  // run function
  int rvalue = fastpcoa( n, d, p, ac, z );

  // transfer to R
  if ( rvalue == 0 ) for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];

  // de-allocate memory
  freematrix( d );
  freematrix( z );

} // Cfastpcoa

int respcoa( const size_t n, double** d, const size_t h, double** q, const size_t p, const double ac, double** c, double** z )
// restricted classical scaling: see Carroll, Green, and Carmone (1976), De Leeuw and Heiser (1982), and Ter Braak (1992).
// distance matrix in d has dimensions n x n, external variables are n x h dimensioned.
// requested dimensionality in p and resulting coefficients in c which is h x p as z = qc is n x p.
{
  int result = 1;

  // allocate memory
  double** qc = getmatrix( n, h, 0.0 );
  double** u = getmatrix( n, n, 0.0 );
  double** v = getmatrix( h, h, 0.0 );
  double* w = getvector( min_t( n, h ), 0.0 );

  // center variables
  for ( size_t j = 1; j <= h; j++ ) {
    const double mn = dsum( n, &q[1][j], h ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) qc[i][j] = q[i][j] - mn;
  }

  // singular value decomposition of q
  result = svdcmp( n, h, qc, u, w, v );
  if ( result == 0 ) { 

    // allocate memory
    double** b = getmatrix( n, n, 0.0 );
    double** nh = getmatrix( n, h, 0.0 );
    double** hn = getmatrix( h, n, 0.0 );
    double** hh = getmatrix( h, h, 0.0 );
    double* vh = getvector( h, 0.0 );

    // transfer right-hand vectors to smaller matrix
    for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= h; j++ ) nh[i][j] = u[i][j];

    // add constant to d when ac is different from zero, otherwise simply copy
    if ( isnotzero( ac ) ) for ( size_t i = 2; i <= n; i++ ) for ( size_t j = 1; j <= i - 1; j++ ) b[j][i] = b[i][j] = d[i][j] + ac;
    else dcopy( n * n, &d[1][1], 1, &b[1][1], 1 );
  
    // square, double center, and multiply d with -0.5 to obtain b matrix
    scalarproducts( n, b, b );
  
    // resize problem for coefficients
    dgemm( true, false, h, n, n, 1.0, nh, b, 0.0, hn );
    dgemm( false, false, h, h, n, 1.0, hn, nh, 0.0, hh );

    // eigenvalue decomposition 
    result = evdcmp( h, hh, vh );
    if ( result == 0 ) {

      // allocate memory
      double** hp = getmatrix( h, p, 0.0 );

      // set intermediate coefficients
      for ( size_t k = 1; k <= p; k++ ) {
        double alpha = ( hh[1][k] < 0.0 ? -1.0 : 1.0 ) * sqrt( fmax( 0.0, vh[k] ) );
        daxpy( h, alpha, &hh[1][k], h, &hp[1][k], p );
      }

      // set final coefficients and coordinates
      for ( size_t j = 1; j <= h; j++ ) for ( size_t k = 1; k <= p; k++ ) hp[j][k] = ( isnotzero( w[j] ) ? hp[j][k] / w[j] : 0.0 );
      dgemm( false, false, h, p, h, 1.0, v, hp, 0.0, c );
      dgemm( false, false, n, p, h, 1.0, qc, c, 0.0, z );

      // de-allocate memory
      freematrix( hp );
    }

    // de-allocate memory
    freematrix( b );
    freematrix( nh );
    freematrix( hn );
    freematrix( hh );
    freevector( vh );
  }

  // de-allocate memory
  freematrix( u );
  freematrix( v );
  freevector( w );
  freematrix( qc );

  return result;
} // respcoa

void Crespcoa( int* rn, double* rd, int* rh, double* rq, int* rp, double* rac, double* rb, double* rz )
// Function Crespcoa() performs restricted classical multidimensional scaling.
{
  // transfer to C
  const size_t n = *rn;
  const size_t h = *rh;
  const size_t p = *rp;
  double** d = getmatrix( n, n, 0.0 );
  for ( size_t j = 1, k = 0; j <= n; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) d[i][j] = rd[k];
  double** q = getmatrix( n, h, 0.0 );
  for ( size_t j = 1, k = 0; j <= h; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) q[i][j] = rq[k];
  const double ac = *rac;
  double** b = getmatrix( h, p, 0.0 );
  double** z = getmatrix( n, p, 0.0 );

  // run function
  int rvalue = respcoa( n, d, h, q, p, ac, b, z );

  // transfer to R
  if ( rvalue == 0 ) {
    for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= h; i++, k++ ) rb[k] = b[i][j];
    for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) rz[k] = z[i][j];
  }

  // de-allocate memory
  freematrix( d );
  freematrix( q );
  freematrix( b );
  freematrix( z );

} // Crespcoa

void Cfasterpcoa( int* rn, int* rm, double* rx, int* rp, int* rk, double* rz, int* rseed )
// Function Cfasterpcoa() performs landmark principal coordinate analysis or classical multidimensional scaling.
{
  // transfer to C
  size_t n = *rn;
  size_t m = *rm;
  size_t p = *rp;
  size_t nk = *rk;
  long xseed = ( long )( *rseed );
  randomize( &xseed );

  double* __restrict px = &rx[0];
  double* __restrict pz = &rz[0];

  double** psdx = getmatrix( m, m, 0.0 );

  if ( nk == 0 ) {

    // compute covariance matrix
    for ( size_t i = 1, ix = 0; i <= m; i++, ix++ ) {
      psdx[i][i] = variance( n, &px[ix], m );
      for ( size_t j = 1, jx = 0; j < i; j++, jx++ ) psdx[j][i] = psdx[i][j] = covariance( n, &px[ix], m, &px[jx], m );
    }
  }
  else if ( nk == 1 ) {
    double* mu = getvector( m, 0.0 );
    for ( size_t j = 1, incy = 0; j <= m; j++, incy++ ) {
      double s = 0.0;
      for ( size_t i = 1, skip = incy; i <= n; i++, skip += m ) s += *( px + skip );
      mu[j] = s / ( double )( n );
    }
    double *cx = px;
    for ( size_t i = 1; i <= m; i++, cx++ ) {
      const double xmu = mu[i];
      double *cy = cx;
      for ( size_t j = i; j <= m; j++, cy++ ) {
        const double ymu = mu[j];
        double s = 0.0;
        for ( size_t k = 1, skip = 0; k <= n; k++, skip += m ) s += ( *( cx + skip ) - xmu ) * ( *( cy + skip ) - ymu );
        psdx[i][j] = psdx[j][i] = s / ( double )( n );
      }
    }
    freevector( mu );
  }
  else {
  
    // select landmark data
    size_t* __restrict idx = ( size_t* ) calloc( n, sizeof( size_t ) );
    for ( size_t i = 0; i < n; i++ ) idx[i] = i;
    for ( size_t i = 0; i < nk; i++ ) {
      size_t k = i + nextsize_t() % ( n - i );
      size_t j = idx[i];
      idx[i] = idx[k];
      idx[k] = j;
    }
    double** __restrict subx = getmatrix( nk, m, 0.0 );
    for ( size_t i = 1; i <= nk; i++ ) {
      size_t ix = idx[i-1] * m;
      for ( size_t j = 1; j <= m; j++, ix++ ) subx[i][j] = px[ix];
    }
    free( idx );

    // compute covariance matrix
    for ( size_t i = 1; i <= m; i++ ) {
      psdx[i][i] = variance( nk, &subx[1][i], m );
      for ( size_t j = 1; j < i; j++ ) psdx[j][i] = psdx[i][j] = covariance( nk, &subx[1][i], m, &subx[1][j], m );
    }
    freematrix( subx );
  }

  // eigenvalue decomposition
  double* vals = getvector( m, 0.0 );
  evdcmp( m, psdx, vals );
  freevector( vals );

  // projection
  for ( size_t i = 1, ix = 0, iz = 0; i <= n; i++, ix += m ) {
    for ( size_t k = 1; k <= p; k++, iz++ ) pz[iz] = ddot( m, &px[ix], 1, &psdx[1][k], m );
  }

  freematrix( psdx );

} // Cfasterpcoa

void pcoa_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test pcoa at ", dt );
  printstring( "\n", "==============================================================\n" );

  randomize( &seed );

  size_t n = 0;
  size_t m = 0; 
  size_t h = 0;
  double** delta = readmatrix( "facial.dat", &n, &m );
  double** vars = readmatrix( "facial.properties", &n, &h );
  double** w = getmatrix( n, n, 1.0 );
  for ( size_t i = 1; i <= n; i++ ) w[i][i] = 0.0;
  size_t p = 2;
  double ac = 0.0;
  double** z = getmatrix( n, p, 0.0 );
  double** tz = getmatrix( p, n, 0.0 );
  double** b = getmatrix( h, p, 0.0 );
  double** tb = getmatrix( p, h, 0.0 );
  double** tvars = getmatrix( h, n, 0.0 );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t j = 1; j <= h; j++ ) tvars[j][i] = vars[i][j];
  double** d = getmatrix( n, n, 0.0 );
  int intn = ( int )( n );
  int intp = ( int )( p );
  int inth = ( int )( h );
  int intk = 13;
  int intseed = ( int )( seed );

  size_t tm = setstarttime();
  pcoa( n, delta, p, 0.0, z );
  printscalar( "elapsed for pcoa", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  tm = setstarttime();
  Cpcoa( &intn, &delta[1][1], &intp, &ac, &tz[1][1] );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) z[i][k] = tz[k][i];
  printscalar( "elapsed for Cpcoa", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  tm = setstarttime();
  fastpcoa( n, delta, p, 0.0, z );
  printscalar( "elapsed for fastpcoa", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  tm = setstarttime();
  Cfastpcoa( &intn, &delta[1][1], &intp, &ac, &tz[1][1] );
  for ( size_t i = 1; i <= n; i++ ) for ( size_t k = 1; k <= p; k++ ) z[i][k] = tz[k][i];
  printscalar( "elapsed for Cfastpcoa", getelapsedtime( tm ) );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  tm = setstarttime();
  respcoa( n, delta, h, vars, p, ac, b, z );
  printscalar( "elapsed for respcoa", getelapsedtime( tm ) );
  dgemm( false, false, n, p, h, 1.0, vars, b, 0.0, z );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  tm = setstarttime();
  Crespcoa( &intn, &delta[1][1], &inth, &tvars[1][1], &intp, &ac, &tb[1][1], &tz[1][1] );
  printscalar( "elapsed for Crespcoa", getelapsedtime( tm ) );
  for ( size_t j = 1; j <= h; j++ ) for ( size_t k = 1; k <= p; k++ ) b[j][k] = tb[k][j];
  dgemm( false, false, n, p, h, 1.0, vars, b, 0.0, z );
  euclidean1( n, p, z, d );
  printscalar( "n-stress", nstress( n, delta, d, w ) );

  tm = setstarttime();
  Cfasterpcoa( &intn, &inth, &vars[1][1], &intp, &intk, &z[1][1], &intseed );
  printscalar( "elapsed for Cfasterpcoa", getelapsedtime( tm ) );

  tm = setstarttime();
  intk = 30;
  Cfasterpcoa( &intn, &inth, &vars[1][1], &intp, &intk, &z[1][1], &intseed );
  printscalar( "elapsed for Cfasterpcoa", getelapsedtime( tm ) );

  tm = setstarttime();
  intk = 1;
  Cfasterpcoa( &intn, &inth, &vars[1][1], &intp, &intk, &z[1][1], &intseed );
  printscalar( "elapsed for Cfasterpcoa", getelapsedtime( tm ) );

  freematrix( delta );
  freematrix( vars );
  freematrix( w );
  freematrix( z );
  freematrix( d );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test pcoa at ", dt );
  printstring( "\n", "==============================================================\n" );
}
