/**
    This file is part of "kdpee", kd-partitioning entropy estimator.
    (c) 2009 Dan Stowell and Queen Mary University of London
    All rights reserved.

    kdpee is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    kdpee is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with kdpee.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <math.h>
#include <stdbool.h>

#include <R.h>
#include <Rinternals.h>

#define CHECK_ARG_IS_REAL_MATRIX(A)                                            \
  if (!isReal(A) || !isMatrix(A))                                              \
    error("Argument '" #A "' is not a real matrix.");

#define CHECK_ARG_IS_REAL_VECTOR(A)                                            \
  if (!isReal(A) || !isVector(A))                                              \
    error("Argument '" #A "' is not a real vector.");

/*
 * Unpack a real matrix stored in SEXP S.
 */
#define UNPACK_REAL_MATRIX(S, D, N, K)                                         \
  CHECK_ARG_IS_REAL_MATRIX(S);                                                 \
  double *D = REAL(S);                                                         \
  const R_len_t N = nrows(S);                                                  \
  const R_len_t K = ncols(S);

#define UNPACK_REAL_VECTOR(S, D, N)                                            \
  CHECK_ARG_IS_REAL_VECTOR(S);                                                 \
  double *D = REAL(S);                                                         \
  const R_len_t N = length(S);

#define UNPACK_REAL(S, R)                                                      \
  CHECK_ARG_IS_REAL_VECTOR(S);                                                 \
  double R = REAL(S)[0];

static void kdpee_hoarePartition(const double *oneRow, int *keys, int minindex,
                                 int maxindex, int l0, int r0, double fulcrum,
                                 int *l, int *r) {
  int tmp;
  *l = l0;
  *r = r0;
  while (*l <= *r) {
    // left_scan
    while (*l <= maxindex && oneRow[keys[*l]] < fulcrum) {
      ++*l;
    }
    // right_scan
    while (*r >= minindex && oneRow[keys[*r]] > fulcrum) {
      --*r;
    }
    // check and exchange (keys)
    if (*l <= *r) {
      tmp = keys[*l];
      keys[*l] = keys[*r];
      keys[*r] = tmp;
      // then
      ++*l;
      --*r;
    }
  }
}

static double kdpee_hoareFind(const double *oneRow, int *keys, int minindex,
                              int maxindex, int findThis) {
  int l, r, i, j;
  i = minindex;
  j = maxindex;
  findThis += minindex; // offset so that we're actually in the desired section
  while (i < j) {
    kdpee_hoarePartition(oneRow, keys, minindex, maxindex, i, j,
                         oneRow[keys[findThis]], &l, &r);
    if (r < findThis) {
      // kth smallest is in right split
      i = l;
    }
    if (findThis < l) {
      // kth smallest is in left split
      j = r;
    }
  }

  // The desired element is now in desired position, so return its value
  return oneRow[keys[findThis]];
}

// by rearranging the keys between keys[minindex] & keys[maxindex] (inclusive),
//   find the median value in that section of oneRow
static double kdpee_hoareMedian(const double *oneRow, int *keys, int minindex,
                                int maxindex) {
  int num = 1 + maxindex - minindex;

  if ((num & 1) == 1) { // odd
    return kdpee_hoareFind(oneRow, keys, minindex, maxindex, (num - 1) / 2);
  } else { // even
    return (kdpee_hoareFind(oneRow, keys, minindex, maxindex, (num / 2)) +
            kdpee_hoareFind(oneRow, keys, minindex, maxindex, (num / 2) - 1)) *
           0.5;
  }
}

static double kdpee_recurse(const double **dimrefs, const int n, const int d,
                            double *mins, double *maxs, const double zcut,
                            int *keys, bool mayTerminate, int curlev,
                            const double n_rec, int minindex, int maxindex,
                            int minlev) {

  int dimno = curlev % d; // The dimension along which we're intending to split
  int thesize = 1 + maxindex - minindex; // No of points in this subset

  // As well as returning the median, this PARTITIONS the data (keys) in-place
  double median = kdpee_hoareMedian(dimrefs[dimno], keys, minindex, maxindex);
  double zscore;

  if (curlev == minlev) {
    mayTerminate = true; // We have passed the lower termination depth
  }

  if (mayTerminate) {
    zscore = (sqrt(thesize) * (median + median - mins[dimno] - maxs[dimno]) /
              (maxs[dimno] - mins[dimno]));
    if (zscore < 0.) {
      zscore = 0. - zscore;
    }
  }

  if (thesize == 1 || (mayTerminate && (zscore < zcut))) {
    // allowed to terminate, and z-score doesn't disprove uniformity,
    // so let's calc the negsurprisal!
    double frac = thesize * n_rec;
    double volume = maxs[0] - mins[0];
    int i;
    for (i = 1; i < d; ++i) {
      volume *= maxs[i] - mins[i];
    }

    if (volume == 0.) {
      return 0.;
    } else {
      return log(volume / frac) * frac;
    }
  } else {
    // We need to partition and recurse
    double oldextremum;
    double left, right;

    int newmaxindex, newminindex;
    if ((thesize & 1) == 0) { // even # points
      newmaxindex = minindex + thesize / 2 - 1;
      newminindex = minindex + thesize / 2;
    } else { // odd # points
      newmaxindex = maxindex - (thesize + 1) / 2;
      newminindex = minindex + (thesize - 1) / 2;
    }

    // Remember the outer extremum, replace with median, then recurse
    oldextremum = maxs[dimno];
    maxs[dimno] = median;
    left = kdpee_recurse(dimrefs, n, d, mins, maxs, zcut, keys, mayTerminate,
                         curlev + 1, n_rec, minindex, newmaxindex, minlev);
    // put the extremum back in place
    maxs[dimno] = oldextremum;

    // Remember the outer extremum, replace with median, then recurse
    oldextremum = mins[dimno];
    mins[dimno] = median;
    right = kdpee_recurse(dimrefs, n, d, mins, maxs, zcut, keys, mayTerminate,
                          curlev + 1, n_rec, newminindex, maxindex, minlev);
    // put the extremum back in place
    mins[dimno] = oldextremum;

    return left + right;
  }
}

static double kdpee(const double **dimrefs, const int n, const int d,
                    double *mins, double *maxs, const double zcut, int *keys) {
  int minlev = ceil(0.5 * log2(n)); // Min partitioning level
  int i;
  double result;

  for (i = 0; i < n; ++i) {
    keys[i] = i; // initialise keys
  }
  result = kdpee_recurse(dimrefs, n, d, mins, maxs, zcut, keys, false, 0,
                         1. / n, 0, n - 1, minlev);
  return result;
}

SEXP do_kdpee(SEXP sX, SEXP sZ, SEXP sMinX, SEXP sMaxX) {
  /* Unpack arguments */
  UNPACK_REAL_MATRIX(sX, X, n, d);
  UNPACK_REAL(sZ, z);
  UNPACK_REAL_VECTOR(sMinX, mins, minn);
  UNPACK_REAL_VECTOR(sMaxX, maxs, maxn);

  if (minn != d)
    error("Incorrect length for sMinX vector. Expected %i, got %i.", d, minn);

  if (maxn != d)
    error("Incorrect length for sMaxX vector. Expected %i, got %i.", d, maxn);

  /* Allocate work memory */
  int *keys = Calloc(n, int);
  const double **XX = Calloc(d, const double *);
  for (int col = 0; col < d; ++col) {
    XX[col] = X + col * n;
  }

  double entropy = kdpee(XX, n, d, mins, maxs, z, keys);

  /* Free work memory */
  Free(XX);
  Free(keys);
  return ScalarReal(entropy);
}

static const R_CallMethodDef callMethods[] = {
    {"do_kdpee", (DL_FUNC)&do_kdpee, 4}, {NULL, NULL, 0}};

void R_init_kdpee(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
