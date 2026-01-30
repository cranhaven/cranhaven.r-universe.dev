/*******************************************************************************
 *
 * Functions common to the phm function and the phmParallel functions
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#pragma once
#include <cstddef>
/*******************************************************************************
 * Utility Functions
 ******************************************************************************/
//Sum all values in a vector of length len
inline double sum_doubles(const double* values, size_t len) {
  double total = 0.0;
  for (size_t i = 0; i < len; ++i) total += values[i];
  return total;
}
/*******************************************************************************
 * Classes
 ******************************************************************************/
//Stores a sparse vector
struct SparseVec {
  const int* rows;
  const double* values;
  size_t size;
  double total;
};
/*******************************************************************************
 * Internal Function Declarations
 ******************************************************************************/
double dist_sparse_cpp(const SparseVec& x, const SparseVec& y, double zeroes);
