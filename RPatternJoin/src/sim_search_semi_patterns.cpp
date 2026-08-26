#include "include/sim_search_semi_patterns.hpp"

void sim_search_semi_patterns(
  const std::vector<std::string>& strings,
  int cutoff,
  char metric,
  str2ints& str2idxs,
  int_pair_set& out
) {
  str2int str2idx;
  countStrings(strings, str2idx, str2idxs);

  sim_search_semi_patterns_impl<TrimDirection::No>(strings, cutoff, metric, str2idx, out, nullptr, true);
}
