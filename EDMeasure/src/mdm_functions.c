#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
///// declarations
////////////////////////////////////////////////////////////////////////////////////////////////////

void double_center(int n, int p, double *X, double *XX);
double inner_prod(int n, double *XX, double *YY);
double inner_prod_perm(int n, int *P, double *XX, double *YY);
void square_dist(double *X, double *D, int nobs, int ndim, int ncomp, int *ICOMP);
void next_index_complete(int *index, int nobs, int ncomp);
void next_index_incomplete(int *index, int nobs, int ncomp);

////////////////////////////////////////////////////////////////////////////////////////////////////
///// functions
////////////////////////////////////////////////////////////////////////////////////////////////////

// X is a p x n matrix, XX is an n x n matrix
void double_center(int n, int p, double *X, double *XX) {
  double* row_sum = (double*) calloc(n, sizeof(double));
  double* col_sum = (double*) calloc(n, sizeof(double));
  
  double total_sum = 0.0;
  double curr, part_sum;
  int i, j, k;

  for (j = 0; j < n; ++j) {
    for (i = 0; i < n; ++i) {
      if (i != j) {
        part_sum = 0.0;

        // XX[i, j] = |X[i, ] - X[j, ]|
        for (k = 0; k < p; ++k) {
          curr = X[i * p + k] - X[j * p + k];
          part_sum += curr * curr;
        }

        part_sum = sqrt(part_sum);

        XX[i + j * n] = part_sum;
        row_sum[i] += part_sum;
        col_sum[j] += part_sum;
        total_sum += part_sum;
      } else {
        XX[i + j * n] = 0.0;
      }
    }
  }

  for (j = 0; j < n; ++j) {
    for (i = 0; i < n; ++i) {
      XX[i + j * n] -= row_sum[i] / n + col_sum[j] / n - total_sum / n / n;
    }
  }

  free(row_sum);
  free(col_sum);
}

// XX is an n x n matrix, YY is an n x n matrix
double inner_prod(int n, double *XX, double *YY) {
  double sum = 0.0; 
  int i, j;

  for (j = 0; j < n; ++j) {
    for (i = 0; i < n; ++i) {
      // XX[i, j] * YY[i, j]
      sum += XX[i + j * n] * YY[i + j * n];
    }
  }

  return sum / n / n;
}

// XX is an n x n matrix, YY is an n x n matrix
double inner_prod_perm(int n, int *P, double *XX, double *YY) {
  double sum = 0.0; 
  int i, j;

  for (j = 0; j < n; ++j) {
    for (i = 0; i < n; ++i) {
      // XX[i, j] * YY[P[i], P[j]]
      sum += XX[i + j * n] * YY[P[i] + P[j] * n];
    }
  }

  return sum / n / n;
}

// D is a ncomp x nobs x nobs vector
void square_dist(double *X, double *D, int nobs, int ndim, int ncomp, int *ICOMP) {
  int i, j, k, l;
  double curr, sq_sum;

  for (j = 0; j < nobs; ++j) {
    for (i = 0; i < nobs; ++i) {
      for (k = 0; k < ncomp; ++k) {
        // |X[i, k] - X[j, k]|^2
        sq_sum = 0.0;

        if (i != j) {
          for (l = ICOMP[k]; l < ICOMP[k + 1]; ++l) { 
            curr = X[i * ndim + l] - X[j * ndim + l];
            sq_sum += curr * curr;
          }
        }

        D[ncomp * (i + j * nobs) + k] = sq_sum;
      }
    }
  } 
}

// find next index for complete V-statistics
void next_index_complete(int *index, int nobs, int ncomp) {
  int i, j;

  // find last index whose value is not nobs - 1
  for (i = ncomp - 1; i >= 0; --i) {
    if (index[i] != nobs - 1) {
      index[i] += 1;

      // update the index after to be 0
      for (j = i + 1; j < ncomp; ++j) {
        index[j] = 0;
      }

      break;
    }
  } 
}

/*int next_index_complete(int *index, int nobs, int ncomp) {
  int i, j;
  int flag = 0;

  // find last index whose value is not nobs - 1
  for (i = ncomp - 1; i >= 0; --i) {
    if (index[i] != nobs - 1) {
      index[i] += 1;

      // update the index after to be 0
      for (j = i + 1; j < ncomp; ++j) {
        index[j] = 0;
      }

      flag = 1;
      break;
    }
  }

  return flag;
}*/

// find next index for incomplete V-statistics
void next_index_incomplete(int *index, int nobs, int ncomp) {
  int i, j;

  for (i = 0; i < ncomp; ++i) {
    j = index[i] + 1;
    index[i] = j % nobs;
  }
}
