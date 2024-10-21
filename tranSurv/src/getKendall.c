#include <R.h>
#include <Rmath.h>
#include <math.h>

// This is a C version of cor(x, y, method = Kendall)
// n = length(x) = length(y)
void uCondKendall(double *x, double *y, int *n, double *out) {
  int i, j;
  double M = 0.0;
  double tmp = 0.0;
  double tau = 0.0;
  for (i = 0; i < (*n - 1); i++) {
    for (j = (i + 1); j < *n; j++) {
      tmp = (y[i] - y[j]) * (x[i] - x[j]);
      tau += (tmp > 0) - (tmp < 0);
      M += 1;
    }
  }
  out[0] = tau / M;
}

// t1 = truncation times
// t2 = failure times
// d is the censoring indicator for t2
// n = length(t1) = length(t2)
// weights = IPWs, i.e., the inversed survival probabilities here.
// meth = 1: MB
// meth = 2: IPW 1 from Austin and Betensky (2014)'s Model 1 in Section 3
// meth = 3: IPW 2 from Austin and Betensky (2014)'s Model 2 in Section 4
void condKendallC(double *t1, double *t2, double *d, int *n, double *weights, 
		    int *meth, double *out) {
  int i, j;
  double *bb = Calloc(*n * (*n - 1), double);
  double Uc = 0.0; 
  double Um = 0.0; 
  double tmp = 0.0;
  double wgt = 0.0;
  double sgn = 0.0;
  double v = 0.0;
  double v1 = 0.0;
  double v2 = 0.0;
  for (i = 0; i < (n[0] - 1); i++) {
    for (j = i + 1; j < n[0]; j++) {
      if (fmax(t1[i], t1[j]) <= fmin(t2[i], t2[j]) &&
	  d[i] * (t2[i] <= t2[j]) + d[j] * (t2[j] <= t2[i]) > 0 &&
	  weights[i] * weights[j] * weights[i + n[0]] * weights[j + n[0]] > 0) {
	tmp = (t1[i] - t1[j]) * (t2[i] - t2[j]);
	sgn = (tmp > 0) - (tmp < 0);
	if (meth[0] == 1) wgt = 1;
	if (meth[0] == 2) wgt = fmax(weights[j + n[0]], weights[i + n[0]]) *
			  fmax(weights[j + n[0]], weights[i + n[0]]) / (weights[j] * weights[i]);
	if (meth[0] == 3) {
	  wgt = weights[j + n[0]] * weights[i + n[0]] / (weights[j] * weights[i]);
	  Uc += d[i] * d[j] * sgn / wgt;
	  Um += d[i] * d[j] / wgt;
	  bb[i * (n[0] - 1) + j - 1] = bb[j * (n[0] - 1) + i] = d[i] * d[j] * sgn / wgt;
	} else {
	  Uc += sgn / wgt;
	  Um += 1 / wgt;
	  bb[i * (n[0] - 1) + j - 1] = bb[j * (n[0] - 1) + i] = sgn / wgt;
	}
	wgt = 0.0;
      }
    }
  }
  out[0] = Uc / Um;
  for (i = 0; i < n[0]; i++) {
    for (j = 0; j < (n[0] - 1); j++) {
      v1 += bb[i * (n[0] - 1) + j];
      v2 += bb[i * (n[0] - 1) + j] * bb[i * (n[0] - 1) + j];
    }
    v += (v1 * v1 - v2) / n[0];
    v1 = 0.0; 
    v2 = 0.0;
  }
  out[1] = v * n[0] * (n[0] - 1) / (Um * Um * (n[0] - 2));
  Free(bb);
}

// conditionaled Product-moment correlation coefficient for UNCENSORED data
//
// t1 is the truncation time
// t2 is the failure time
// n = length(t1) = length(t2)
void pmccC(double *t1, double *t2, int *n, double *out) {
  int i, j;
  double STT = 0.0;
  double SXT = 0.0;
  double SXX = 0.0;
  double tmp = 0.0; 
  for (i = 0; i < *n; i++) {
    for (j = 0; j < *n; j++) {
      if (fmax(t1[i], t1[j]) <= fmin(t2[i], t2[j])) {
	STT += (t1[i] - t1[j]) * (t1[i] - t1[j]);
	SXT += (t1[i] - t1[j]) * (t2[i] - t2[j]);
	SXX += (t2[i] - t2[j]) * (t2[i] - t2[j]);
      }
    }
  }
  out[0] = SXT / sqrt(STT * SXX); 
  for (i = 0; i < *n; i++) {
    tmp = 0.0;
    for (j = 0; j < (*n - 1); j++) {
      if (fmax(t1[i], t1[j]) <= fmin(t2[i], t2[j])) {
	tmp += (t2[i] - t2[j]) * (t2[i] - t2[j]) / SXX + (t1[i] - t1[j]) * (t1[i] - t1[j]) / STT -
	  2 * (t1[i] - t1[j]) * (t2[i] - t2[j]) / SXT;
      }
    }
    out[1] += tmp * tmp;     
  }
  out[1] = out[1] * out[0] * out[0]; 
}


// Weighted conditional Kendall's tau (with perturbation weight) for UNCENSORED data
// 
// t1 = truncation times, t' or t'(a).
// t2 = failure times
// d is the censoring indicator for t2
// n = length(t1) = length(t2)
// wgt = perturbation weights
void wKendallC(double *t1, double *t2, int *n, double *d, double *wgt, double *out) {
  int i, j;
  double Uc = 0.0; // sum_i sum_j wi * wj * \sgn(...) I(Omega)
  double Um = 0.0; // sum_i sum_j wi * wj * I(Omega)
  double tmp = 0.0;
  for (i = 0; i < (*n - 1); i++) {
    for (j = i + 1; j < *n; j++) {
      if (fmax(t1[i], t1[j]) <= fmin(t2[i], t2[j]) &&
	  d[i] * (t2[i] <= t2[j]) + d[j] * (t2[j] <= t2[i]) > 0) {
	tmp = (t1[i] - t1[j]) * (t2[i] - t2[j]);
	Uc += ((tmp > 0) - (tmp < 0)) * wgt[i] * wgt[j];
	Um += wgt[i] * wgt[j];
    }
    }
  }
  out[0] = Uc / Um;
}

