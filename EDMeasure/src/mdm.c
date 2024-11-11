#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
///// declarations
////////////////////////////////////////////////////////////////////////////////////////////////////

/* functions.c */
extern void double_center(int n, int p, double *X, double *XX);
extern double inner_prod(int n, double *XX, double *YY);
extern double inner_prod_perm(int n, int *P, double *XX, double *YY);
extern void square_dist(double *X, double *D, int nobs, int ndim, int ncomp, int *ICOMP);
extern void next_index_complete(int *index, int nobs, int ncomp);
extern void next_index_incomplete(int *index, int nobs, int ncomp);

/* squared dCov */
void dCov(double *X, double *Y, double *XX, double *YY, double *V, int *NOBS, int *NDIMX, int *NDIMY);

void dCov_perm(double *XX, double *YY, double *V, int *NOBS, int *IPERM);

/* asymmetric MDM based on dCov */
double dCov_asymmetric_single(double *D, int nobs, int ncomp, int start);
void dCov_asymmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double dCov_asymmetric_single_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
void dCov_asymmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

/* symmetric MDM based on dCov */
double dCov_symmetric_single(double *D, int nobs, int ncomp, int start);
void dCov_symmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double dCov_symmetric_single_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
void dCov_symmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

/* common term in MDM based on (simplified) complete MDM */
double MDM_term1(double *D, int nobs, int ncomp, int start);

double MDM_term1_perm(double *D, int nobs, int ncomp, int start, int *IPERM);

/* complete MDM */
double MDM_term2_complete(double *D, int nobs, int ncomp);
double MDM_term3_complete(double *D, int nobs, int ncomp);
void MDM_complete(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double MDM_term2_complete_perm(double *D, int nobs, int ncomp, int *IPERM);
double MDM_term3_complete_perm(double *D, int nobs, int ncomp, int *IPERM);
void MDM_complete_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

/* simplified complete MDM */
double MDM_term2_complete_simple(double *D, int nobs, int ncomp);
double MDM_term3_complete_simple(double *D, int nobs, int ncomp);
void MDM_complete_simple(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double MDM_term2_complete_simple_perm(double *D, int nobs, int ncomp, int *IPERM);
double MDM_term3_complete_simple_perm(double *D, int nobs, int ncomp, int *IPERM);
void MDM_complete_simple_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

/* asymmetric MDM based on complete MDM */
double MDM_term2_asymmetric(double *D, int nobs, int ncomp, int start);
double MDM_term3_asymmetric(double *D, int nobs, int ncomp, int start);
void MDM_asymmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double MDM_term2_asymmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
double MDM_term3_asymmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
void MDM_asymmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

/* simplified asymmetric MDM based on simplified complete MDM */
double MDM_term2_asymmetric_simple(double *D, int nobs, int ncomp, int start);
double MDM_term3_asymmetric_simple(double *D, int nobs, int ncomp, int start);
void MDM_asymmetric_simple(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double MDM_term2_asymmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
double MDM_term3_asymmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
void MDM_asymmetric_simple_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

/* symmetric MDM based on complete MDM */
double MDM_term2_symmetric(double *D, int nobs, int ncomp, int start);
double MDM_term3_symmetric(double *D, int nobs, int ncomp, int start);
void MDM_symmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double MDM_term2_symmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
double MDM_term3_symmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
void MDM_symmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

/* simplified symmetric MDM based on simplified complete MDM */
double MDM_term2_symmetric_simple(double *D, int nobs, int ncomp, int start);
double MDM_term3_symmetric_simple(double *D, int nobs, int ncomp, int start);
void MDM_symmetric_simple(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP);

double MDM_term2_symmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
double MDM_term3_symmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM);
void MDM_symmetric_simple_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM);

////////////////////////////////////////////////////////////////////////////////////////////////////
///// mutual dependence measures based on distance covariance
////////////////////////////////////////////////////////////////////////////////////////////////////

/* squared dCov */
void dCov(double *X, double *Y, double *XX, double *YY, double *V, int *NOBS, int *NDIMX, int *NDIMY) {
  int nobs = NOBS[0];
  int ndimx = NDIMX[0];
  int ndimy = NDIMY[0];

  double_center(nobs, ndimx, X, XX);
  double_center(nobs, ndimy, Y, YY);

  V[0] = inner_prod(nobs, XX, YY);
}

void dCov_perm(double *XX, double *YY, double *V, int *NOBS, int *IPERM) {
  int nobs = NOBS[0];

  V[0] = inner_prod_perm(nobs, IPERM, XX, YY);
}

/* asymmetric MDM based on dCov */
double dCov_asymmetric_single(double *D, int nobs, int ncomp, int start) {
  double* X_row_sum = (double*) calloc(nobs, sizeof(double));
  double* X_col_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_row_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_col_sum = (double*) calloc(nobs, sizeof(double));
  double* XX = (double*) calloc(nobs * nobs, sizeof(double));
  double* YY = (double*) calloc(nobs * nobs, sizeof(double));

  double X_total_sum = 0.0;
  double Y_total_sum = 0.0;
  double X_part_sum, Y_part_sum;
  int i, j, k;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      if (i != j) {
        X_part_sum = D[ncomp * (i + j * nobs) + start];
        Y_part_sum = 0.0;

        // XX[i, j] = |X[i, ] - X[j, ]|
        for (k = start + 1; k < ncomp; ++k) {
          Y_part_sum += D[ncomp * (i + j * nobs) + k];
        }

        X_part_sum = sqrt(X_part_sum);
        Y_part_sum = sqrt(Y_part_sum);

        XX[i + j * nobs] = X_part_sum;
        X_row_sum[i] += X_part_sum;
        X_col_sum[j] += X_part_sum;
        X_total_sum += X_part_sum;

        YY[i + j * nobs] = Y_part_sum;
        Y_row_sum[i] += Y_part_sum;
        Y_col_sum[j] += Y_part_sum;
        Y_total_sum += Y_part_sum;

      } else {
        XX[i + j * nobs] = 0.0;
        YY[i + j * nobs] = 0.0;
      }
    }
  }

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      XX[i + j * nobs] -= X_row_sum[i] / nobs + X_col_sum[j] / nobs - X_total_sum / nobs / nobs;
      YY[i + j * nobs] -= Y_row_sum[i] / nobs + Y_col_sum[j] / nobs - Y_total_sum / nobs / nobs;
    }
  }

  free(X_row_sum);
  free(X_col_sum);
  free(Y_row_sum);
  free(Y_col_sum);

  // XX, YY
  double v = inner_prod(nobs, XX, YY);

  free(XX);
  free(YY);

  return v;
}

void dCov_asymmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double curr;
  // double temp;

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // printf("========== asymmetric mutual independence measure ==========\n");

  // calculate V
  for (i = 0; i < ncomp - 1; ++i) {
    curr = dCov_asymmetric_single(D, nobs, ncomp, i);
    sum += curr;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

double dCov_asymmetric_single_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  double* X_row_sum = (double*) calloc(nobs, sizeof(double));
  double* X_col_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_row_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_col_sum = (double*) calloc(nobs, sizeof(double));
  double* XX = (double*) calloc(nobs * nobs, sizeof(double));
  double* YY = (double*) calloc(nobs * nobs, sizeof(double));

  double X_total_sum = 0.0;
  double Y_total_sum = 0.0;
  double X_part_sum, Y_part_sum;
  int i, j, k;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      if (i != j) {
        X_part_sum = D[ncomp * (IPERM[ncomp * i + start] + IPERM[ncomp * j + start] * nobs) + start];
        Y_part_sum = 0.0;

        // XX[i, j] = |X[i, ] - X[j, ]|
        for (k = start + 1; k < ncomp; ++k) {
          Y_part_sum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * j + k] * nobs) + k];
        }

        X_part_sum = sqrt(X_part_sum);
        Y_part_sum = sqrt(Y_part_sum);

        XX[i + j * nobs] = X_part_sum;
        X_row_sum[i] += X_part_sum;
        X_col_sum[j] += X_part_sum;
        X_total_sum += X_part_sum;

        YY[i + j * nobs] = Y_part_sum;
        Y_row_sum[i] += Y_part_sum;
        Y_col_sum[j] += Y_part_sum;
        Y_total_sum += Y_part_sum;

      } else {
        XX[i + j * nobs] = 0.0;
        YY[i + j * nobs] = 0.0;
      }
    }
  }

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      XX[i + j * nobs] -= X_row_sum[i] / nobs + X_col_sum[j] / nobs - X_total_sum / nobs / nobs;
      YY[i + j * nobs] -= Y_row_sum[i] / nobs + Y_col_sum[j] / nobs - Y_total_sum / nobs / nobs;
    }
  }

  free(X_row_sum);
  free(X_col_sum);
  free(Y_row_sum);
  free(Y_col_sum);

  // XX, YY
  double v = inner_prod(nobs, XX, YY);

  free(XX);
  free(YY);

  return v;
}

void dCov_asymmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double curr;
  // double temp;

  // calculate V
  for (i = 0; i < ncomp - 1; ++i) {
    curr = dCov_asymmetric_single_perm(D, nobs, ncomp, i, IPERM);
    sum += curr;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

/* symmetric MDM based on dCov */
double dCov_symmetric_single(double *D, int nobs, int ncomp, int start) {
  double* X_row_sum = (double*) calloc(nobs, sizeof(double));
  double* X_col_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_row_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_col_sum = (double*) calloc(nobs, sizeof(double));
  double* XX = (double*) calloc(nobs * nobs, sizeof(double));
  double* YY = (double*) calloc(nobs * nobs, sizeof(double));

  double X_total_sum = 0.0;
  double Y_total_sum = 0.0;
  double X_part_sum, Y_part_sum;
  int i, j, k;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      if (i != j) {
        X_part_sum = D[ncomp * (i + j * nobs) + start];
        Y_part_sum = 0.0;

        // XX[i, j] = |X[i, ] - X[j, ]|
        for (k = 0; k < ncomp; ++k) {
          if (k != start) {
            Y_part_sum += D[ncomp * (i + j * nobs) + k];
          }
        }

        X_part_sum = sqrt(X_part_sum);
        Y_part_sum = sqrt(Y_part_sum);

        XX[i + j * nobs] = X_part_sum;
        X_row_sum[i] += X_part_sum;
        X_col_sum[j] += X_part_sum;
        X_total_sum += X_part_sum;

        YY[i + j * nobs] = Y_part_sum;
        Y_row_sum[i] += Y_part_sum;
        Y_col_sum[j] += Y_part_sum;
        Y_total_sum += Y_part_sum;

      } else {
        XX[i + j * nobs] = 0.0;
        YY[i + j * nobs] = 0.0;
      }
    }
  }

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      XX[i + j * nobs] -= X_row_sum[i] / nobs + X_col_sum[j] / nobs - X_total_sum / nobs / nobs;
      YY[i + j * nobs] -= Y_row_sum[i] / nobs + Y_col_sum[j] / nobs - Y_total_sum / nobs / nobs;
    }
  }

  free(X_row_sum);
  free(X_col_sum);
  free(Y_row_sum);
  free(Y_col_sum);

  // XX, YY
  double v = inner_prod(nobs, XX, YY);

  free(XX);
  free(YY);

  return v;
}

void dCov_symmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double curr;
  // double temp;

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // printf("========== symmetric mutual independence measure ==========\n");

  // calculate V
  for (i = 0; i < ncomp; ++i) {
    curr = dCov_symmetric_single(D, nobs, ncomp, i);
    sum += curr;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

double dCov_symmetric_single_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  double* X_row_sum = (double*) calloc(nobs, sizeof(double));
  double* X_col_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_row_sum = (double*) calloc(nobs, sizeof(double));
  double* Y_col_sum = (double*) calloc(nobs, sizeof(double));
  double* XX = (double*) calloc(nobs * nobs, sizeof(double));
  double* YY = (double*) calloc(nobs * nobs, sizeof(double));

  double X_total_sum = 0.0;
  double Y_total_sum = 0.0;
  double X_part_sum, Y_part_sum;
  int i, j, k;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      if (i != j) {
        X_part_sum = D[ncomp * (IPERM[ncomp * i + start] + IPERM[ncomp * j + start] * nobs) + start];
        Y_part_sum = 0.0;

        // XX[i, j] = |X[i, ] - X[j, ]|
        for (k = 0; k < ncomp; ++k) {
          if (k != start) {
            Y_part_sum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * j + k] * nobs) + k];
          }
        }

        X_part_sum = sqrt(X_part_sum);
        Y_part_sum = sqrt(Y_part_sum);

        XX[i + j * nobs] = X_part_sum;
        X_row_sum[i] += X_part_sum;
        X_col_sum[j] += X_part_sum;
        X_total_sum += X_part_sum;

        YY[i + j * nobs] = Y_part_sum;
        Y_row_sum[i] += Y_part_sum;
        Y_col_sum[j] += Y_part_sum;
        Y_total_sum += Y_part_sum;

      } else {
        XX[i + j * nobs] = 0.0;
        YY[i + j * nobs] = 0.0;
      }
    }
  }

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      XX[i + j * nobs] -= X_row_sum[i] / nobs + X_col_sum[j] / nobs - X_total_sum / nobs / nobs;
      YY[i + j * nobs] -= Y_row_sum[i] / nobs + Y_col_sum[j] / nobs - Y_total_sum / nobs / nobs;
    }
  }

  free(X_row_sum);
  free(X_col_sum);
  free(Y_row_sum);
  free(Y_col_sum);

  // XX, YY
  double v = inner_prod(nobs, XX, YY);

  free(XX);
  free(YY);

  return v;
}

void dCov_symmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double curr;
  // double temp;

  // calculate V
  for (i = 0; i < ncomp; ++i) {
    curr = dCov_symmetric_single_perm(D, nobs, ncomp, i, IPERM);
    sum += curr;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
///// mutual dependence measures based on complete measure
////////////////////////////////////////////////////////////////////////////////////////////////////

/* common term in MDM based on (simplified) complete MDM */
double MDM_term1(double *D, int nobs, int ncomp, int start) {
  int i, j, k;
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // |X[i] - X[j]|^2
      if (i != j) {
        for (k = start; k < ncomp; ++k) {
          sqsum += D[ncomp * (i + j * nobs) + k];
        }
      }
        
      total += sqrt(sqsum);
    }
  } 

  return total / nobs / nobs;
}

/*double MDM_term1_mutual(double *D, int nobs, int ncomp) {
  int i, j, k;
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // |X[i] - X[j]|^2
      if (i != j) {
        for (k = 0; k < ncomp; ++k) {
          sqsum += D[ncomp * (i + j * nobs) + k];
        }
      }
        
      total += sqrt(sqsum);
    }
  } 

  return total / nobs / nobs;
}*/

double MDM_term1_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k;
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // |X[i] - X[j]|^2
      if (i != j) {
        for (k = start; k < ncomp; ++k) {
          sqsum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * j + k] * nobs) + k];
        }
      }
       
      total += sqrt(sqsum);
    }
  } 

  return total / nobs / nobs;
}

/* complete MDM */
double MDM_term2_complete(double *D, int nobs, int ncomp) {
  int i, j, k;
  int niter = pow(nobs, ncomp);
  // int index[ncomp];
  // memset(index, 0, ncomp * sizeof(int));
  int* index = (int*) calloc(ncomp, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (i + index[k] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_complete(index, nobs, ncomp);
  } 

  free(index);

  return 2 * total / niter / nobs;
}

double MDM_term3_complete(double *D, int nobs, int ncomp) {
  int i, j, k;
  int niter = pow(nobs, ncomp);
  // int left[ncomp];
  // memset(left, 0, ncomp * sizeof(int));
  // int right[ncomp];
  // memset(right, 0, ncomp * sizeof(int));
  int* left = (int*) calloc(ncomp, sizeof(int));
  int* right = (int*) calloc(ncomp, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < niter; ++i) {
      sqsum = 0.0;

      // |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (left[k] + right[k] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_complete(left, nobs, ncomp);
    }

    next_index_complete(right, nobs, ncomp);
    memset(left, 0, ncomp * sizeof(int));
  } 

  free(left);
  free(right);

  return total / niter / niter;
}

void MDM_complete(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // calculate V
  double v1 = MDM_term1(D, nobs, ncomp, 0);
  double v2 = MDM_term2_complete(D, nobs, ncomp);
  double v3 = MDM_term3_complete(D, nobs, ncomp);

  // printf("========== complete mutual independence measure ==========\n");
  // printf("v1: %g\n", v1);  
  // printf("v2: %g\n", v2);
  // printf("v3: %g\n", v3);
 
  V[0] = v2 - v1 - v3; 
  // printf("dCov: %g\n", V[0]);
}

double MDM_term2_complete_perm(double *D, int nobs, int ncomp, int *IPERM) {
  int i, j, k;
  int niter = pow(nobs, ncomp);
  // int index[ncomp];
  // memset(index, 0, ncomp * sizeof(int));
  int* index = (int*) calloc(ncomp, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // calculate |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * index[k] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_complete(index, nobs, ncomp);
  } 

  free(index);

  return 2 * total / niter / nobs;
}

double MDM_term3_complete_perm(double *D, int nobs, int ncomp, int *IPERM) {
  int i, j, k;
  int niter = pow(nobs, ncomp);
  // int left[ncomp];
  // memset(left, 0, ncomp * sizeof(int));
  // int right[ncomp];
  // memset(right, 0, ncomp * sizeof(int));
  int* left = (int*) calloc(ncomp, sizeof(int));
  int* right = (int*) calloc(ncomp, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < niter; ++i) {
      sqsum = 0.0;

      // |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * left[k] + k] + IPERM[ncomp * right[k] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_complete(left, nobs, ncomp);
    }

    next_index_complete(right, nobs, ncomp);
    memset(left, 0, ncomp * sizeof(int));
  } 

  free(left);
  free(right);

  return total / niter / niter;
}

void MDM_complete_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];

  // calculate V
  double v1 = MDM_term1_perm(D, nobs, ncomp, 0, IPERM);
  double v2 = MDM_term2_complete_perm(D, nobs, ncomp, IPERM);
  double v3 = MDM_term3_complete_perm(D, nobs, ncomp, IPERM);
  // printf("v1: %g\n", v1);
  // printf("v2: %g\n", v2);
  // printf("v3: %g\n", v3);

  V[0] = v2 - v1 - v3; 
  // printf("perm dCov: %g\n", V[0]);
}

/* simplified complete MDM */
double MDM_term2_complete_simple(double *D, int nobs, int ncomp) {
  int i, j, k, l;
  int* index = (int*) malloc(ncomp * sizeof(int));
  for (l = 0; l < ncomp; ++l) {
    index[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (i + index[k] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_incomplete(index, nobs, ncomp);
  } 

  free(index);

  return 2 * total / nobs / nobs;
}

double MDM_term3_complete_simple(double *D, int nobs, int ncomp) {
  int i, j, k, l;
  int* left = (int*) malloc(ncomp * sizeof(int));
  for (l = 0; l < ncomp; ++l) {
    left[l] = l;
  }
  int* right = (int*) malloc(ncomp * sizeof(int));
  for (l = 0; l < ncomp; ++l) {
    right[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (left[k] + right[k] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_incomplete(left, nobs, ncomp);
    }

    next_index_incomplete(right, nobs, ncomp);
    for (l = 0; l < ncomp; ++l) {
      left[l] = l;
    }
  } 

  free(left);
  free(right);

  return total / nobs / nobs;
}

void MDM_complete_simple(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // calculate V
  double v1 = MDM_term1(D, nobs, ncomp, 0);
  double v2 = MDM_term2_complete_simple(D, nobs, ncomp);
  double v3 = MDM_term3_complete_simple(D, nobs, ncomp);

  // printf("========== incomplete mutual independence measure ==========\n");
  // printf("v1: %g\n", v1);  
  // printf("v2: %g\n", v2);
  // printf("v3: %g\n", v3);
 
  V[0] = v2 - v1 - v3; 
  // printf("dCov: %g\n", V[0]);
}

double MDM_term2_complete_simple_perm(double *D, int nobs, int ncomp, int *IPERM) {
  int i, j, k, l;
  int* index = (int*) malloc(ncomp * sizeof(int));
  for (l = 0; l < ncomp; ++l) {
    index[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // calculate |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * index[k] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_incomplete(index, nobs, ncomp);
  } 

  free(index);

  return 2 * total / nobs / nobs;
}

double MDM_term3_complete_simple_perm(double *D, int nobs, int ncomp, int *IPERM) {
  int i, j, k, l;
  int* left = (int*) malloc(ncomp * sizeof(int));
  for (l = 0; l < ncomp; ++l) {
    left[l] = l;
  }
  int* right = (int*) malloc(ncomp * sizeof(int));
  for (l = 0; l < ncomp; ++l) {
    right[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // calculate |X[i] - X[?]|^2
      for (k = 0; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * left[k] + k] + IPERM[ncomp * right[k] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_incomplete(left, nobs, ncomp);
    }

    next_index_incomplete(right, nobs, ncomp);
    for (l = 0; l < ncomp; ++l) {
      left[l] = l;
    }
  } 

  free(left);
  free(right);

  return total / nobs / nobs;
}

void MDM_complete_simple_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];

  // calculate V
  double v1 = MDM_term1_perm(D, nobs, ncomp, 0, IPERM);
  double v2 = MDM_term2_complete_simple_perm(D, nobs, ncomp, IPERM);
  double v3 = MDM_term3_complete_simple_perm(D, nobs, ncomp, IPERM);
  // printf("v1: %g\n", v1);  
  // printf("v2: %g\n", v2);
  // printf("v3: %g\n", v3);
 
  V[0] = v2 - v1 - v3; 
  // printf("dCov: %g\n", V[0]);
}

/* asymmetric MDM based on complete MDM */
double MDM_term2_asymmetric(double *D, int nobs, int ncomp, int start) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* index = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (i + index[0] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (i + index[1] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_complete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / niter / nobs;
}

double MDM_term3_asymmetric(double *D, int nobs, int ncomp, int start) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* left = (int*) calloc(2, sizeof(int));
  int* right = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < niter; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (left[0] + right[0] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (left[1] + right[1] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_complete(left, nobs, 2);
    }

    next_index_complete(right, nobs, 2);
    memset(left, 0, 2 * sizeof(int));
  } 

  free(left);
  free(right);

  return total / niter / niter;
}

void MDM_asymmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // printf("========== asymmetric mutual independence measure ==========\n");

  // calculate V
  for (i = 0; i < ncomp - 1; ++i) {
    double v1 = MDM_term1(D, nobs, ncomp, i);
    double v2 = MDM_term2_asymmetric(D, nobs, ncomp, i);
    double v3 = MDM_term3_asymmetric(D, nobs, ncomp, i);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

double MDM_term2_asymmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* index = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (IPERM[ncomp * i + start] + IPERM[ncomp * index[0] + start] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * index[1] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_complete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / niter / nobs;
}

double MDM_term3_asymmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* left = (int*) calloc(2, sizeof(int));
  int* right = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < niter; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (IPERM[ncomp * left[0] + start] + IPERM[ncomp * right[0] + start] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * left[1] + k] + IPERM[ncomp * right[1] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_complete(left, nobs, 2);
    }

    next_index_complete(right, nobs, 2);
    memset(left, 0, 2 * sizeof(int));
  } 

  free(left);
  free(right);

  return total / niter / niter;
}

void MDM_asymmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate V
  for (i = 0; i < ncomp - 1; ++i) {
    double v1 = MDM_term1_perm(D, nobs, ncomp, i, IPERM);
    double v2 = MDM_term2_asymmetric_perm(D, nobs, ncomp, i, IPERM);
    double v3 = MDM_term3_asymmetric_perm(D, nobs, ncomp, i, IPERM);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

/* simplified asymmetric MDM based on simplified complete MDM */
double MDM_term2_asymmetric_simple(double *D, int nobs, int ncomp, int start) {
  int i, j, k, l;
  int* index = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    index[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (i + index[0] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (i + index[1] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_incomplete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / nobs / nobs;
}

double MDM_term3_asymmetric_simple(double *D, int nobs, int ncomp, int start) {
  int i, j, k, l;
  int* left = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    left[l] = l;
  }
  int* right = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    right[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (left[0] + right[0] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (left[1] + right[1] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_incomplete(left, nobs, 2);
    }

    next_index_incomplete(right, nobs, 2);
    for (l = 0; l < 2; ++l) {
      left[l] = l;
    }
  } 

  free(left);
  free(right);

  return total / nobs / nobs;
}

void MDM_asymmetric_simple(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // printf("========== asymmetric mutual independence measure ==========\n");

  // calculate V
  for (i = 0; i < ncomp - 1; ++i) {
    double v1 = MDM_term1(D, nobs, ncomp, i);
    double v2 = MDM_term2_asymmetric_simple(D, nobs, ncomp, i);
    double v3 = MDM_term3_asymmetric_simple(D, nobs, ncomp, i);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

double MDM_term2_asymmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k, l;
  int* index = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    index[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (IPERM[ncomp * i + start] + IPERM[ncomp * index[0] + start] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * index[1] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
    }

    next_index_incomplete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / nobs / nobs;
}

double MDM_term3_asymmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k, l;
  int* left = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    left[l] = l;
  }
  int* right = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    right[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start
      sqsum += D[ncomp * (IPERM[ncomp * left[0] + start] + IPERM[ncomp * right[0] + start] * nobs) + start];

      // start + 1, ..., end
      for (k = start + 1; k < ncomp; ++k) {
        sqsum += D[ncomp * (IPERM[ncomp * left[1] + k] + IPERM[ncomp * right[1] + k] * nobs) + k];
      }

      total += sqrt(sqsum);
      next_index_incomplete(left, nobs, 2);
    }

    next_index_incomplete(right, nobs, 2);
    for (l = 0; l < 2; ++l) {
      left[l] = l;
    }
  } 

  free(left);
  free(right);

  return total / nobs / nobs;
}

void MDM_asymmetric_simple_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate V
  for (i = 0; i < ncomp - 1; ++i) {
    double v1 = MDM_term1_perm(D, nobs, ncomp, i, IPERM);
    double v2 = MDM_term2_asymmetric_simple_perm(D, nobs, ncomp, i, IPERM);
    double v3 = MDM_term3_asymmetric_simple_perm(D, nobs, ncomp, i, IPERM);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

/* symmetric MDM based on complete MDM */
double MDM_term2_symmetric(double *D, int nobs, int ncomp, int start) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* index = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (i + index[0] * nobs) + start];
        } else {
          sqsum += D[ncomp * (i + index[1] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
    }

    next_index_complete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / niter / nobs;
}

double MDM_term3_symmetric(double *D, int nobs, int ncomp, int start) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* left = (int*) calloc(2, sizeof(int));
  int* right = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < niter; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (left[0] + right[0] * nobs) + start];
        } else {
          sqsum += D[ncomp * (left[1] + right[1] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
      next_index_complete(left, nobs, 2);
    }

    next_index_complete(right, nobs, 2);
    memset(left, 0, 2 * sizeof(int));
  } 

  free(left);
  free(right);

  return total / niter / niter;
}

void MDM_symmetric(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // printf("========== symmetric mutual independence measure ==========\n");

  // calculate V
  for (i = 0; i < ncomp; ++i) {
    double v1 = MDM_term1(D, nobs, ncomp, 0);
    double v2 = MDM_term2_symmetric(D, nobs, ncomp, i);
    double v3 = MDM_term3_symmetric(D, nobs, ncomp, i);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

double MDM_term2_symmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* index = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (IPERM[ncomp * i + start] + IPERM[ncomp * index[0] + start] * nobs) + start];
        } else {
          sqsum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * index[1] + k] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
    }

    next_index_complete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / niter / nobs;
}

double MDM_term3_symmetric_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k;
  int niter = pow(nobs, 2);
  int* left = (int*) calloc(2, sizeof(int));
  int* right = (int*) calloc(2, sizeof(int));
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < niter; ++j) {
    for (i = 0; i < niter; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (IPERM[ncomp * left[0] + start] + IPERM[ncomp * right[0] + start] * nobs) + start];
        } else {
          sqsum += D[ncomp * (IPERM[ncomp * left[1] + k] + IPERM[ncomp * right[1] + k] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
      next_index_complete(left, nobs, 2);
    }

    next_index_complete(right, nobs, 2);
    memset(left, 0, 2 * sizeof(int));
  } 

  free(left);
  free(right);

  return total / niter / niter;
}

void MDM_symmetric_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate V
  for (i = 0; i < ncomp; ++i) {
    double v1 = MDM_term1_perm(D, nobs, ncomp, 0, IPERM);
    double v2 = MDM_term2_symmetric_perm(D, nobs, ncomp, i, IPERM);
    double v3 = MDM_term3_symmetric_perm(D, nobs, ncomp, i, IPERM);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

/* simplified symmetric MDM based on simplified complete MDM */
double MDM_term2_symmetric_simple(double *D, int nobs, int ncomp, int start) {
  int i, j, k, l;
  int* index = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    index[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (i + index[0] * nobs) + start];
        } else {
          sqsum += D[ncomp * (i + index[1] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
    }

    next_index_incomplete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / nobs / nobs;
}

double MDM_term3_symmetric_simple(double *D, int nobs, int ncomp, int start) {
  int i, j, k, l;
  int* left = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    left[l] = l;
  }
  int* right = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    right[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (left[0] + right[0] * nobs) + start];
        } else {
          sqsum += D[ncomp * (left[1] + right[1] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
      next_index_incomplete(left, nobs, 2);
    }

    next_index_incomplete(right, nobs, 2);
    for (l = 0; l < 2; ++l) {
      left[l] = l;
    }
  } 

  free(left);
  free(right);

  return total / nobs / nobs;
}

void MDM_symmetric_simple(double *X, double *D, double *V, int *NOBS, int *NDIM, int *NCOMP, int *ICOMP) {
  int nobs = NOBS[0];
  int ndim = NDIM[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate D
  square_dist(X, D, nobs, ndim, ncomp, ICOMP);

  // printf("========== symmetric mutual independence measure ==========\n");

  // calculate V
  for (i = 0; i < ncomp; ++i) {
    double v1 = MDM_term1(D, nobs, ncomp, 0);
    double v2 = MDM_term2_symmetric_simple(D, nobs, ncomp, i);
    double v3 = MDM_term3_symmetric_simple(D, nobs, ncomp, i);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}

double MDM_term2_symmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k, l;
  int* index = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    index[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (IPERM[ncomp * i + start] + IPERM[ncomp * index[0] + start] * nobs) + start];
        } else {
          sqsum += D[ncomp * (IPERM[ncomp * i + k] + IPERM[ncomp * index[1] + k] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
    }

    next_index_incomplete(index, nobs, 2);
  } 

  free(index);

  return 2 * total / nobs / nobs;
}

double MDM_term3_symmetric_simple_perm(double *D, int nobs, int ncomp, int start, int *IPERM) {
  int i, j, k, l;
  int* left = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    left[l] = l;
  }
  int* right = (int*) malloc(2 * sizeof(int));
  for (l = 0; l < 2; ++l) {
    right[l] = l;
  }
  double total = 0.0; 
  double sqsum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      sqsum = 0.0;

      // start, ..., end
      for (k = 0; k < ncomp; ++k) {
        if (k == start) {
          sqsum += D[ncomp * (IPERM[ncomp * left[0] + start] + IPERM[ncomp * right[0] + start] * nobs) + start];
        } else {
          sqsum += D[ncomp * (IPERM[ncomp * left[1] + k] + IPERM[ncomp * right[1] + k] * nobs) + k];
        }
      }

      total += sqrt(sqsum);
      next_index_incomplete(left, nobs, 2);
    }

    next_index_incomplete(right, nobs, 2);
    for (l = 0; l < 2; ++l) {
      left[l] = l;
    }
  } 

  free(left);
  free(right);
  
  return total / nobs / nobs;
}

void MDM_symmetric_simple_perm(double *D, double *V, int *NOBS, int *NCOMP, int *IPERM) {
  int nobs = NOBS[0];
  int ncomp = NCOMP[0];
  int i;
  double sum = 0.0;
  double temp;

  // calculate V
  for (i = 0; i < ncomp; ++i) {
    double v1 = MDM_term1_perm(D, nobs, ncomp, 0, IPERM);
    double v2 = MDM_term2_symmetric_simple_perm(D, nobs, ncomp, i, IPERM);
    double v3 = MDM_term3_symmetric_simple_perm(D, nobs, ncomp, i, IPERM);

    // printf("step %d\n", i);
    // printf("v1: %g\n", v1);  
    // printf("v2: %g\n", v2);
    // printf("v3: %g\n", v3);
   
    temp = v2 - v1 - v3; 
    // printf("dCov: %g\n", temp);
    sum += temp;
  }

  V[0] = sum;
  // printf("total dCov: %g\n", sum);
}
