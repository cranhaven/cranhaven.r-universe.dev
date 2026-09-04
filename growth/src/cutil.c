/*
 *  rmutil : A Library of Special Functions for Repeated Measurements
 *  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  SYNOPSIS
 *
 *    void F77_CALL(flgamma)(double *x,double *y)
 *    void F77_CALL(flbeta)(double *a,double*b,double *y)
 *    void F77_CALL(fbesselk)(double *x,double *alpha,double *y)
 *
 *  DESCRIPTION
 *
 *    This function allows a Fortran program to call the lgamma, lbeta,
 * and bessel_k functions written in C.
 *
 */

#include <math.h>
#include "R.h"
#include "Rmath.h"
#include "R_ext/RS.h"

extern double lgammafn(double x);
void F77_CALL(flgamma)(double *x,double *y){
  *y=lgammafn(*x);}

extern double lbeta(double a, double b);
void F77_CALL(flbeta)(double *a,double *b,double *y){
  *y=lbeta(*a, *b);}

extern double bessel_k(double x, double alpha, double expo);
void F77_CALL(fbesselk)(double *x,double *alpha, double *y){
  *y=bessel_k(*x, *alpha, 1.0);}
