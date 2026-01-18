/*
// ISNA(x): true for R's NA only
// ISNAN(x): true for R's NA and IEEE NaN
// R_FINITE(x): false for Inf,-Inf,NA,NaN
// R_IsNaN(x): true for NaN but not NA

// Rprintf:  printing from a C routine compiled into R

 */

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <stdio.h>
#include <math.h>
#include "R_ext/Applic.h"

double fhat2d(double x0, double dx, int nx,
	      double y0, double dy, int ny,
	      double z[], double x, double y)
{
  int i,ix,iy;
  double res=0.0;
  ix = floor((x-x0)/dx);
  iy = floor((y-y0)/dy);

  if(ix > 0 && iy > 0 && ix < nx-1.0 && iy < ny-1.0){
    i = ix * ny + iy - 1;
    res = z[i] + z[i+1];
    i = i + ny;
    res += z[i] + z[i+1];
  }
  return(0.25*res);
}

void arl1(double *lcl, double *x, int *nx,
	 double *y, int *ny, double *z, int *n0, double *pm0,
	 double *pI0, double *lambda)
{
  int i, k, iter = 50000, B=10000000;
  double rl, rM, rZ, EM, EZ, n, fxy, x0, y0, dx, dy;

  x0 = x[0]; y0 = y[0];
  dx = (x[1] - x[0])/(nx[0]-1.0);
  dy = (y[1] - y[0])/(ny[0]-1.0);
  n = (double) n0[0];
  
  GetRNGstate();

  rl = 0.0;
  for(i=0; i<iter; i++){
    rM = rbinom(n, pm0[0]);
    rZ = rbinom(1.0, pI0[0]);
    EM = n * pm0[0];
    EZ = pI0[0];
    k = 1;
    while(k < B){
      k++;
      rM = rbinom(n, pm0[0]);
      rZ = rbinom(1.0, pI0[0]);
      EM = lambda[0] * rM + (1.0-lambda[0])*EM;
      EZ = lambda[0] * rZ + (1.0-lambda[0])*EZ;
      fxy = fhat2d(x0,dx,nx[0],y0,dy,ny[0],z,EM,EZ);
      if(fxy < lcl[0]){
	rl += (double) k;
	break;
      }
    }
  }
  PutRNGstate();
  lcl[0] = rl/((double) iter);
}

void arl0(double *lcl, double *x, int *nx,
	 double *y, int *ny, double *z, int *n0, double *pm0,
	 double *pI0, double *lambda)
{
  int i, j, k, iter = 50000, B=10000000, M=20;
  double xa=0.00001, xb=0.99999, xc,xa1,xb1;
  double rl, rM, rZ, EM, EZ, n, fxy, x0, y0, dx, dy;
  double delta,dK,dmin=99.0,K0,A0,ARL0=lcl[0];

  x0 = x[0]; y0 = y[0];
  dx = (x[1] - x[0])/(nx[0]-1.0);
  dy = (y[1] - y[0])/(ny[0]-1.0);
  n = (double) n0[0];
  
  GetRNGstate();
  for(j=0; j<M; j++){
    xc = 0.5 * (xa+xb);
    dK = xb - xa;
    rl = 0.0;
    for(i=0; i<iter; i++){
      rM = rbinom(n, pm0[0]);
      rZ = rbinom(1.0, pI0[0]);
      EM = n * pm0[0];
      EZ = pI0[0];
      k = 1;
      while(k < B){
	k++;
	rM = rbinom(n, pm0[0]);
	rZ = rbinom(1.0, pI0[0]);
	EM = lambda[0] * rM + (1.0-lambda[0])*EM;
	EZ = lambda[0] * rZ + (1.0-lambda[0])*EZ;
	fxy = fhat2d(x0,dx,nx[0],y0,dy,ny[0],z,EM,EZ);
	if(fxy < xc){
	  rl += (double) k;
	  break;
	}
      }
    }
    rl = rl/((double) iter);
    if(rl < ARL0){
      xb = xc;
    }else{
      xa = xc;
    }
    delta = fabs(rl - ARL0);
    if(delta < dmin){
      dmin = delta;
      K0 = xc;
      A0 = rl;
      xa1 = xa; xb1 = xb;
    }
    
    if(delta < 0.1){
      lcl[0] = xc;
      lambda[0] = rl;
      break;
    }
    if(dK < 0.0001){
      lcl[0] = K0;
      lambda[0] = A0;
      break;
    }
  }

  if(delta > 0.2){
    dmin = 99.0;
    dK = (xb1-xa1)*0.05;
    xa1 += dK;
    for(j=0; j<20; j++){
      rl = 0.0;
      for(i=0; i<iter; i++){
	rM = rbinom(n, pm0[0]);
	rZ = rbinom(1.0, pI0[0]);
	EM = n * pm0[0];
	EZ = pI0[0];
	k = 1;
	while(k < B){
	  k++;
	  rM = rbinom(n, pm0[0]);
	  rZ = rbinom(1.0, pI0[0]);
	  EM = lambda[0] * rM + (1.0-lambda[0])*EM;
	  EZ = lambda[0] * rZ + (1.0-lambda[0])*EZ;
	  fxy = fhat2d(x0,dx,nx[0],y0,dy,ny[0],z,EM,EZ);
	  if(fxy < xa1){
	    rl += (double) k;
	    break;
	  }
	}
      }
      rl = rl/((double) iter);
      delta = fabs(rl - ARL0);
      if(delta < dmin){
	dmin = delta;
	K0 = xa1;
	A0 = rl;
      }
    }
    lcl[0] = K0;
    lambda[0] = A0;
  }
  PutRNGstate();
}

void simucc(int *B, int *T, int *n0, double *pm0,
	    double *pI0, double *lambda,
	    double *Md, double *D)
{
  int i,j;
  double rM, rZ, EM, EZ, n;

  n = (double) n0[0];
  
  GetRNGstate();

  for(j=0; j<B[0]; j++){
    rM = rbinom(n, pm0[0]);
    rZ = rbinom(1.0, pI0[0]);
    EM = n * pm0[0];
    EZ = pI0[0];
    for(i=1; i<T[0]; i++){
      rM = rbinom(n, pm0[0]);
      rZ = rbinom(1.0, pI0[0]);
      EM = lambda[0] * rM + (1.0-lambda[0])*EM;
      EZ = lambda[0] * rZ + (1.0-lambda[0])*EZ;
    }
    Md[j] = EM;
    D[j] = EZ;
  }
  PutRNGstate();
}
