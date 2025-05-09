//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#include "fmds.h"

double explain( const size_t n, const size_t p, double** z, double* q, double* w, double* a, double* e, const int level, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo )
// function explain performs linear regression with transformations: transform(q) = Za' 
{
  // set constants
  const double EPS = DBL_EPSILON;                                          // 2.2204460492503131e-16
  const double TOL = sqrt( EPS );                                          // 1.4901161193847656e-08
  const double CRIT = sqrt( TOL );                                         // 0.00012207031250000000

  // allocate memory
  size_t* index = getvector_t( n, ( size_t )( 0 ) );
  size_t* tbl = getvector_t( n, ( size_t )( 0 ) );
  double* vd = getvector( n, 0.0 );
  double* vw = getvector( n, 0.0 );
  double** ztwz = getmatrix( p, p, 0.0 );
  double* ztwq = getvector( p, 0.0 );

  // center configuration
  for ( size_t k = 1; k <= p; k++ ) {
    const double mn = dsum( n, &z[1][k], p ) / ( double )( n );
    for ( size_t i = 1; i <= n; i++ ) z[i][k] -= mn;
  }
  
  // prepare ordinal transform
  size_t count = 0;
  size_t ntb = 1;
  if ( level >= 3 ) {
    size_t last = n;
    for ( size_t i = 1; i <= n; i++ ) {
      if ( isnotzero( w[i] ) ) {
        count++;
        index[count] = i;
        vd[count] = q[i];
      }
      else {
        index[last] = i;
        last--;
      }
    }
    dsort( count, vd, index );
    tbl[ntb] = 1;
    for ( size_t i = 2; i <= count; i++ ) {
      if ( isequal( vd[i], vd[i - 1] ) ) tbl[ntb]++;
      else {
        ntb++;
        tbl[ntb] = 1;
      }
    }
  }

  // (always) center properties
  const double wsm = dwsum( n, &q[1], 1, &w[1], 1 );
  const double scale = dsum( n, &w[1], 1 );
  const double wmn = wsm / scale;
  for ( size_t i = 1; i <= n; i++ ) q[i] = ( iszero( w[i] ) ? 0.0 : q[i] - wmn );

  // normalize q, except for absolute
  if ( level > 0 ) {
    const double wssq = dwssq( n, &q[1], 1, &w[1], 1 );
    if ( isnotzero( wssq ) ) {
      const double alpha = sqrt( ( double )( n - 1 ) / wssq );
      dscal( n, alpha, &q[1], 1 );
    }
  }

  // initialize a and fold
  for ( size_t k = 1; k <= p; k++ ) {
    ztwz[k][k] = dwssq( n, &z[1][k], p, &w[1], 1 );
    for ( size_t l = 1; l < k; l++ ) ztwz[k][l] = ztwz[l][k] = dwdot( n, &z[1][k], p, &z[1][l], p, &w[1], 1 );
  }
  inverse( p, ztwz );
  for ( size_t k = 1; k <= p; k++ ) ztwq[k] = dwdot( n, &z[1][k], p, &q[1], 1, &w[1], 1 );
  for ( size_t k = 1; k <= p; k++ ) a[k] = ddot( p, &ztwz[k][1], 1, &ztwq[1], 1 );
  for ( size_t i = 1; i <= n; i++ ) e[i] = ddot( p, &z[i][1], 1, &a[1], 1 );
  double fold = dwsse( n, &q[1], 1, &e[1], 1, &w[1], 1 );

  // start main loop
  size_t iter = 0;
  for ( iter = 1; iter <= MAXITER; iter++ ) {

    // optimal scale factor: none (0)
    if ( level == TRANSFORMATIONLEVEL.ABSOLUTE ) {

    }

    // optimal scale factor: ratio (1)
    else if ( level == TRANSFORMATIONLEVEL.RATIO ) {
      double wssq = dwssq( n, &q[1], 1, &w[1], 1 );
      const double cross = dwdot( n, &q[1], 1, &e[1], 1, &w[1], 1 );
      if ( iszero( wssq ) ) wssq = EPS;
      const double b = ( cross < 0.0 ? 0.0 : cross / wssq );
      if ( isnotzero( b ) ) dscal( n, b, &q[1], 1 );
    }

    // optimal intercept: linear (2)
    else if ( level == TRANSFORMATIONLEVEL.LINEAR ) {
      const double sumw = dsum( n, &w[1], 1 );
      const double wsumx = dwsum( n, &q[1], 1, &w[1], 1 );
      const double wsumy = dwsum( n, &e[1], 1, &w[1], 1 );
      const double wssqx = dwssq( n, &q[1], 1, &w[1], 1 );
      const double cross = dwdot( n, &e[1], 1, &q[1], 1, &w[1], 1 );
      const double work = wssqx * sumw - wsumx * wsumx;
      const double b = ( isnotzero( work ) ? ( cross * sumw - wsumx * wsumy ) / work : 0.0 );
      const double a = ( wsumy - b * wsumx ) / sumw;
      if ( isnotzero( b ) ) for ( size_t i = 1; i <= n; i++ ) q[i] = a + b * q[i];
    }

    // optimal steps: ordinal (6)
    else if ( level == TRANSFORMATIONLEVEL.ORDINAL ) {
      for ( size_t k = 1; k <= count; k++ ) {
        vd[k] = e[index[k]];
        vw[k] = w[index[k]];
      }
      for ( size_t b = 1, k = 0; b <= ntb; b++ ) {
        if ( tbl[b] > 1 ) dsort( tbl[b], &vd[k], &index[k] );
        for ( size_t j = 1; j <= tbl[b]; j++ ) vw[k+j] = w[index[k+j]];
        k += tbl[b];
      }
      monotone( count, vd, vw );
      for ( size_t k = 1; k <= count; k++ ) q[index[k]] = vd[k];
    }

    // optimal steps: nominal (7)
    else if ( level == TRANSFORMATIONLEVEL.NOMINAL ) {
      size_t k = 0;
      for ( size_t b = 1; b <= ntb; b++ ) {
        double d1 = 0.0;
        double d2 = 0.0;
        for ( size_t j = 1; j <= tbl[b]; j++ ) {
          const size_t p = index[k + j];
          d1 += w[p] * e[p];
          d2 += w[p];
        }
        if ( iszero( d2 ) ) for ( size_t j = 1; j <= tbl[b]; j++ ) q[index[k + j]] = 0.0;
        else {
          const double wmn = d1 / d2;
          for ( size_t j = 1; j <= tbl[b]; j++ ) q[index[k + j]] = wmn;
        }
        k += tbl[b];
      }
    }

    // normalize q
    if ( level > 0 ) {
      const double wssq = dwssq( n, &q[1], 1, &w[1], 1 );
      if ( isnotzero( wssq ) ) {
        const double alpha = sqrt( ( double )( n - 1 ) / wssq );
        dscal( n, alpha, &q[1], 1 );
      }
    }

    // intermediate results
    const double fhalf = dwsse( n, &q[1], 1, &e[1], 1, &w[1], 1 );

    // update a
    for ( size_t k = 1; k <= p; k++ ) ztwq[k] = dwdot( n, &z[1][k], p, &q[1], 1, &w[1], 1 );
    for ( size_t k = 1; k <= p; k++ ) a[k] = ddot( p, &ztwz[k][1], 1, &ztwq[1], 1 );

    // administration
    for ( size_t i = 1; i <= n; i++ ) e[i] = ddot( p, &z[i][1], 1, &a[1], 1 );
    const double fnew = dwsse( n, &q[1], 1, &e[1], 1, &w[1], 1 );

    // echo intermediate results
    if ( echo == true ) echoprogress( iter, fold, fhalf, fnew );

    // check divergence and convergence
    ( *lastdif ) = fold - fnew;
    if ( ( *lastdif ) <= -1.0 * CRIT ) break;
    if ( fnew < FCRIT ) break;
    if ( 2.0 * ( *lastdif ) <= FCRIT * ( fold + fnew + EPS ) ) break;
    fold = fnew;
  }
  ( *lastiter ) += iter;

  const double vaf = pow( pearson( n, q, e, w ), 2.0 );
 
  // // variance accounted for
  // for ( size_t k = 1; k <= p; k++ ) {
  //   for ( size_t i = 1; i <= n; i++ ) vd[i] = a[k] * z[i][k];
  //   vafdim[k] = pow( pearson( n, q, vd, w ), 2.0 );
  // }
  
  // // calculate std error of coefficients
  // double rss = 0.0;
  // for ( size_t i = 1; i <= dt.n; i++ ) {
  //   double resid = out.propa[g][i] - out.eprop[g][i];
  //   rss += out.wprop[g][i] * resid * resid;
  // }
  // double ssq = rss / double( dt.n - in.ndims );
  // double sumw = dsum( dt.n, &out.wprop[g][1], 1 );
  // out.secprop[g] = ssq / sumw;
  // for ( size_t k = 1; k <= in.ndims; k++ ) out.seaprop[g][k] = sqrt( ztwz[k][k] * ssq );
  
  // // multiple nominal coordinates
  // // discrimination measure
  // // proportion correct
  // if ( in.prop_transformation[g-1] == Transformation::NOMINAL ) {
  //   for ( size_t c = 1; c <= out.ncprop[g]; c++ ) {
  //     double w = 0.0;
  //     for ( size_t i = 1; i <= dt.n; i++ ) if ( isequal( out.prop[g][i], out.ctprop[g][c] ) ) {
  //       w += 1.0;
  //       for ( size_t p = 1; p <= in.ndims; p++ ) out.ccprop[g][c][p] += out.z[i][p];
  //     }
  //     out.csprop[g][c] = w;
  //     dscal( in.ndims, 1.0 / w, &out.ccprop[g][c][1], 1 );
  //     for ( size_t p = 1; p <= in.ndims; p++ ) out.dcprop[g][p] += w * pow( out.ccprop[g][c][p], 2.0 );
  //   }
  //   for ( size_t p = 1; p <= in.ndims; p++ ) out.dcprop[g][p] = out.dcprop[g][p] / double( dt.n );
  
  //   double pe = 0.0;
  //   double po = 0.0;
  //   double **d = getmatrix( dt.n, out.ncprop[g], 0.0 );
  //   euclidean( dt.n, in.ndims, out.z, out.ncprop[g], out.ccprop[g], d );
  //   for ( size_t c = 1; c <= out.ncprop[g]; c++ ) {
  //     double cat = out.ctprop[g][c];

  //     double tp = 0.0;
  //     double fp = 0.0;
  //     double tn = 0.0;
  //     double fn = 0.0;
  //     for ( size_t i = 1; i <= dt.n; i++ ) {
  //       double mind = d[i][1];
  //       size_t minc = 1;
  //       for ( size_t j = 2; j <= out.ncprop[g]; j++ ) if ( d[i][j] < mind ) {
  //         mind = d[i][j];
  //         minc = j;
  //       }
  //       if ( isequal( cat, out.prop[g][i] ) ) {
  //         if ( isequal( out.prop[g][i], out.ctprop[g][minc] ) ) tp += 1.0;  // true positive
  //         else fp += 1.0;
  //       }
  //       else {
  //         if ( isequal( cat, out.ctprop[g][minc] ) ) fn += 1.0;            // false negative
  //         else tn += 1.0;
  //       }
  //     }
  //     out.preprop[g][c] = tp / out.csprop[g][c];
  //     out.senprop[g][c] = tp / ( tp + fn );
  //     po += tp;
  //     pe += ( fn + tp ) * ( fp + tp ) / dt.n;
  //   }
  //   freematrix( d );
  //   out.accprop[g] = po / dt.n;
  //   out.kppprop[g] = ( po - pe ) / ( dt.n - pe );
  // }
  
  // // compute model variance accounted for
  // out.mprop = dsum( in.nprops, &out.vprop[1], 1 ) / double( in.nprops );
  
  // // directions
  // for ( size_t g = 1; g <= in.nprops; g++ ) {
  //   for ( size_t p = 1; p <= in.ndims; p++ ) out.dprop[g][p] = pearson( dt.n, &out.propa[g][1], 1, &out.z[1][p], in.ndims, &out.wprop[g][1], 1 );
  // }

  // de-allocate memory
  freevector_t( index );
  freevector_t( tbl );
  freevector( vw );
  freevector( vd );
  freematrix( ztwz );
  freevector( ztwq );

  return( vaf );
} // explain

void CRexplain( int* rn, int* rp, double* rz, double* rq, double* rw, double* ra, double* re, int* rl, int* rmaxiter, double* rfdif, double* rfvalue, int* recho )
// function CRexplain
{
  // transfer to C
  const size_t n = *rn;
  const size_t p = *rp;
  const int level = *rl;
  const size_t MAXITER = *rmaxiter;
  const double FCRIT = *rfdif;
  const bool echo = ( *recho ) != 0;

  // allocate one-based memory
  double** z = getmatrix( n, p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) z[i][j] = rz[k];
  double* q = getvector( n, 0.0 );
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) q[i] = rq[k];
  double* w = getvector( n, 0.0 );
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) w[i] = rw[k];
  double* a = getvector( p, 0.0 );
  for ( size_t j = 1, k = 0; j <= p; j++, k++ ) a[j] = ra[k];
  double* e = getvector( n, 0.0 );
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) e[i] = re[k];

  // run function
  size_t lastiter = 0;
  double lastdif = 0.0;
  const double fvalue = explain( n, p, z, q, w, a, e, level, MAXITER, FCRIT, &lastiter, &lastdif, echo );

  // transfer to R
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) rq[k] = q[i];
  for ( size_t j = 1, k = 0; j <= p; j++, k++ ) ra[k] = a[j];
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) re[k] = e[i];
  ( *rmaxiter ) = ( int ) ( lastiter );
  ( *rfdif ) = lastdif;
  ( *rfvalue ) = fvalue;

  // de-allocate one-based memory
  freematrix( z );
  freevector( q );
  freevector( w );
  freevector( a );
  freevector( e );

} // CRexplain

void explain_unittest( long seed )
{
  char* dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "started unit test explain at ", dt );
  printstring( "", "==============================================================\n" );

  randomize( &seed );
 
//  pcoa( n, delta, p, z );
//  euclidean1( n, p, z, d2 );
//  double fvalue = mds( n, delta, p, z, d2, MAXITER, FCRIT, &lastiter, &lastdif, echo );
//  double vaf = explain( n, p, z, q, w, a, d1, 4, MAXITER, FCRIT, &lastiter, &lastdif, echo );

  dt = getdatetime();
  printstring( "\n", "==============================================================\n" );
  printstring( "finished unit test explain at ", dt );
  printstring( "", "==============================================================\n" );
}
