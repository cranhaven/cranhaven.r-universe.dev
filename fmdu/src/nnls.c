//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#include "flib.h"

void Cnnls( int* rn, int* rm, double* ra, double* rb, int* rtype, double* rx, int* rMAXITER, double* rFCRIT )
// Function Cnnls() uses different algorithms performing nonnegative least squares.
{
  // transfer to C
  size_t n = *rn;
  size_t m = *rm;
  double** a = getmatrix( n, m, 0.0 );
  for ( size_t j = 1, k = 0; j <= m; j++ ) for ( size_t i = 1; i <= n; i++, k++ ) a[i][j] = ra[k];
  double* b = getvector( n, 0.0 );
  for ( size_t i = 1, k = 0; i <= n; i++, k++ ) b[i] = rb[k];
  double* x = getvector( m, 0.0 );
  for ( size_t i = 1, k = 0; i <= m; i++, k++ ) x[i] = rx[k];
  size_t type = *rtype;
  size_t MAXITER = *rMAXITER;
  double FCRIT = *rFCRIT;

  // run function
  int rvalue = 0;
  if ( type == 1 ) rvalue = nnls( n, m, a, x, b, &MAXITER, &FCRIT );
  if ( type == 2 ) rvalue = nnals( n, m, a, x, b, &MAXITER, &FCRIT );
  if ( type == 3 ) rvalue = fastnnls( n, m, a, x, b, &MAXITER, &FCRIT );
  if ( type == 4 ) rvalue = nnccd( n, m, a, x, b, &MAXITER, &FCRIT );

  // transfer to R
  if ( rvalue == 0 ) for ( size_t i = 1, k = 0; i <= m; i++, k++ ) rx[k] = x[i];
  *rMAXITER = MAXITER;
  *rFCRIT = FCRIT;

  // de-allocate memory
  freematrix( a );
  freevector( b );
  freevector( x );

} // Cnnls
