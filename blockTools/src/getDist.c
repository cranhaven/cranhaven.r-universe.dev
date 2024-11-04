#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>
#include "util.h"

double cmahal(double *x1, double *capX, int *n, double *x2){
  int i, j; 
  double diff[*n], x1X[*n], ans=0;
  for(i=0; i<*n; i++){
    diff[i] = x1[i] - x2[i];
  }

  for(j=0;j < *n; j++){
    x1X[j]=0;
   for(i=0;i < *n;i++){
      x1X[j] += diff[i] * capX[i + j*(*n)];
    }
  }

  for(i=0; i<*n; i++){
    ans += x1X[i] * diff[i]; 
      }
  return sqrt(ans);
}

double * allmahal(int *ncol, int *nrow, int n, double *data, double *vcovi, double *vec)
{
  double X[*nrow][*ncol], tmp1[*ncol], tmp2[*ncol];
  int i, ii, j, k=0;
  for(j=0; j<*ncol; j++){
    for(i=0; i<*nrow; i++){
      X[i][j] = data[k]; 
      k++;
    }
  }
  k=0;
  for(i=1; i<*nrow; i++){
    for(j=0; j<i; j++){
      for(ii=0; ii<*ncol; ii++){
	tmp1[ii] = X[i][ii];
	tmp2[ii] = X[j][ii];
      }
      vec[k] = cmahal(tmp1, vcovi, ncol, tmp2);
      k++;
    }
  }
  return vec;
}

double * cleanUp(int l2, int *l1names, int valid, double *validvar, double validlb, double validub, int n, double *vec)
{
  int i, j;
  if(l2 == 1){ 
    for(i=0; i<n; i++){
      j = mycol(i); 
      if(l1names[j-1] == l1names[myrow(i)-1]){
	vec[i] = HUGE_VAL; 
      }
    }
  }
  if(valid == 1){ 
    for(i=0; i<n; i++){
      j = mycol(i);
      if(sqrt(pow((validvar[j-1] - validvar[myrow(i)-1]), 2)) < validlb || sqrt(pow((validvar[j-1] - validvar[myrow(i)-1]), 2)) > validub)
	{
	  vec[i] = HUGE_VAL;
	}
    }
  }
  return vec;
}

