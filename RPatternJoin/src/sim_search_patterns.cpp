#include "include/sim_search_patterns.hpp"

void sim_search_patterns(
  const std::vector<std::string>& strings,
  int cutoff,
  char metric,
  str2int& str2idx,
  int_pair_set& out,
  ints* strings_subset,
  bool include_eye
) {
  str2ints pat2str;
  map_patterns<TrimDirection::No>(strings, cutoff, metric, str2idx, strings_subset, pat2str);

  for (auto entry = pat2str.begin(); entry != pat2str.end(); entry++) {
    if (entry->second.size() > 1) {
      Rcpp::checkUserInterrupt();
      for (auto str_idx1 = entry->second.begin(); str_idx1 != entry->second.end(); ++str_idx1) {
        for (auto str_idx2 = str_idx1 + 1; str_idx2 != entry->second.end(); ++str_idx2) {
          if (*str_idx1 > *str_idx2) {
            out.insert({*str_idx2, *str_idx1});
          } else {  
            out.insert({*str_idx1, *str_idx2});
          }
        }
      }
    }
  }

  if (include_eye)
    for (int i = 0; i < strings.size(); i++)
      out.insert({i, i});
}


void sim_search_patterns(
  const std::vector<std::string>& strings,
  int cutoff,
  char metric,
  str2ints& str2idxs,
  int_pair_set& out
) {
  str2int str2idx;
  countStrings(strings, str2idx, str2idxs);

  sim_search_patterns(strings, cutoff, metric, str2idx, out, nullptr, true);
}
