#include <R.h>
#include <Rmath.h>
#include <math.h>

void plLambda(double *sl, double *tij, double *yi, double *weights,
	      int *n, int *N, // n is the length of sl
	      // output
	      double *res) {
  int i, j;
  double dl = 0, Rl = 0;
  for (j = 0; j < *n; j++) {
    res[j] = 1;
    dl = 0;
    Rl = 0;
    for (i = 0; i < *N; i++) {
      if (sl[j] >= tij[i] && sl[j] <= yi[i]) {
	Rl = Rl + weights[i];
      }
      if (sl[j] == tij[i] && tij[i] != yi[i]) {
	dl = dl + weights[i];
      }
    }
    if (Rl > 0) {
      if (j == 0)
	res[j] = (dl / Rl);
      if (j > 0)
	res[j] = res[j - 1] + (dl / Rl);
    }
  }
  //res;
}

void alphaEq1(double *X, double *Lambda, int *mt, int *n, int *p, double *res) {
  int i, j, r; 
  for (i = 0; i < *n; i++) {
    for (j = 0; j < *n; j++) {
      for (r = 0; r < *p; r++) {
	if (Lambda[i] != 0 && Lambda[j] != 0) {
	  res[r] += X[i + r * *n] * (mt[i] / Lambda[i] -  mt[j] / Lambda[j]);
	}
	if (Lambda[i] != 0 && Lambda[j] == 0) {
	  res[r] += X[i + r * *n] * (mt[i] / Lambda[i]);
	}
	if (Lambda[i] == 0 && Lambda[j] != 0) {
	  res[r] += X[i + r * *n] * (0 - mt[j] / Lambda[j]);
	}
      }
    }
  }
  //res;
}		 
