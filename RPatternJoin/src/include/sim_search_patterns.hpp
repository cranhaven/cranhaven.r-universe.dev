#ifndef SIM_SEARCH_PATTERNS_HPP
#define SIM_SEARCH_PATTERNS_HPP

#include <vector>
#include <string>
#include "map_patterns.hpp"
#include "file_io.hpp"
#include "patterns_generators.hpp"
#include "trim_strings.hpp"

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

void sim_search_patterns(
  const std::vector<std::string>& strings,
  int cutoff,
  char metric,
  str2int& str2idx,
  int_pair_set& out,
  ints* strings_subset = nullptr,
  bool include_eye = true
);

void sim_search_patterns(
  const std::vector<std::string>& strings,
  int cutoff,
  char metric,
  str2ints& str2idxs,
  int_pair_set& out
);


#endif // SIM_SEARCH_PATTERNS_HPP
