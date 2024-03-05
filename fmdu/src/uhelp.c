//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#include "uhelp.h"

void rotateplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1 )
// rotate Z to principal axes, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  rotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  gemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  copy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  freematrix( r );
} // rotateplus

void rotateplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2 )
// rotate Z to principal axes, plus, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  rotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  gemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  copy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  double** tz2 = getmatrix( n2, p, 0.0 );
  gemm( false, false, n2, p, p, 1.0, z2, r, 0.0, tz2 );
  copy( n2 * p, &tz2[1][1], 1, &z2[1][1], 1 );
  freematrix( tz2 );
  freematrix( r );
} // rotateplusplus

void rotateplusplusplus( const size_t n, const size_t p, double** z, const size_t n1, double** z1, const size_t n2, double** z2, const size_t n3, double** z3 )
// rotate Z to principal axes, plus, plus, plus
{
  double* ev = getvector( n, 0.0 );
  double** r = getmatrix( p, p, 0.0 );
  rotation( n, p, z, r, ev );
  freevector( ev );
  double** tz = getmatrix( n, p, 0.0 );
  gemm( false, false, n, p, p, 1.0, z, r, 0.0, tz );
  copy( n * p, &tz[1][1], 1, &z[1][1], 1 );
  freematrix( tz );
  double** tz1 = getmatrix( n1, p, 0.0 );
  gemm( false, false, n1, p, p, 1.0, z1, r, 0.0, tz1 );
  copy( n1 * p, &tz1[1][1], 1, &z1[1][1], 1 );
  freematrix( tz1 );
  double** tz2 = getmatrix( n2, p, 0.0 );
  gemm( false, false, n2, p, p, 1.0, z2, r, 0.0, tz2 );
  copy( n2 * p, &tz2[1][1], 1, &z2[1][1], 1 );
  freematrix( tz2 );
  double** tz3 = getmatrix( n3, p, 0.0 );
  gemm( false, false, n3, p, p, 1.0, z3, r, 0.0, tz3 );
  copy( n3 * p, &tz3[1][1], 1, &z3[1][1], 1 );
  freematrix( tz3 );
  freematrix( r );
} // rotateplusplus
