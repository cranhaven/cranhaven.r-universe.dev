#ifndef CORRELATION_UTILS_H
#define CORRELATION_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cmath>

using namespace Rcpp;

// Forward declaration (to let spearman.cpp call it)
double pearson_corr_cpp(NumericVector x, NumericVector y);

// Check if a vector contains NA or NaN
inline bool has_na_or_nan(const NumericVector& v) {
  for (int i = 0; i < v.size(); ++i) {
    if (NumericVector::is_na(v[i]) || std::isnan(v[i]))
      return true;
  }
  return false;
}

// Check if all values in a vector are constant
inline bool is_constant(const NumericVector& v) {
  double ref = v[0];
  for (int i = 1; i < v.size(); ++i) {
    if (v[i] != ref)
      return false;
  }
  return true;
}

// Rank function (average ties)
inline NumericVector rank_cpp(const NumericVector& x) {
  int n = x.size();
  std::vector<int> indices(n);
  std::iota(indices.begin(), indices.end(), 0);

  std::sort(indices.begin(), indices.end(), [&](int a, int b) {
    return x[a] < x[b];
  });

  NumericVector ranks(n);
  for (int i = 0; i < n; ++i) ranks[indices[i]] = i + 1;

  for (int i = 0; i < n; ) {
    int j = i + 1;
    while (j < n && x[indices[j]] == x[indices[i]]) ++j;
    if (j - i > 1) {
      double avg = 0.0;
      for (int k = i; k < j; ++k) avg += ranks[indices[k]];
      avg /= (j - i);
      for (int k = i; k < j; ++k) ranks[indices[k]] = avg;
    }
    i = j;
  }

  return ranks;
}

#endif
