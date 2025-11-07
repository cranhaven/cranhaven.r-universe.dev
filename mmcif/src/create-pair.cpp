#include "Rcpp.h"
#include <vector>
#include <algorithm>
#include <numeric>

// [[Rcpp::export(rng = false)]]
Rcpp::List create_pair_indices
  (Rcpp::IntegerVector const cluster_id, Rcpp::IntegerVector const obs_idx){
  if(cluster_id.length() != obs_idx.length())
    throw std::invalid_argument("cluster_id.length() != obs_idx.length()");

  std::vector<R_len_t> cluster_id_order(cluster_id.length());
  std::iota(cluster_id_order.begin(), cluster_id_order.end(), R_len_t{0});

  std::sort
    (cluster_id_order.begin(), cluster_id_order.end(),
     [&](int const x, int const y) { return cluster_id[x] < cluster_id[y]; });

  std::vector<unsigned> cluster_sizes;
  cluster_sizes.reserve(cluster_id.length() / 2);
  size_t n_ele_out{};
  {
    auto head = cluster_id_order.begin();
    while(head != cluster_id_order.end()){
      auto const cluster{cluster_id[*head++]};
      unsigned n_members{1};
      while(head != cluster_id_order.end() && cluster_id[*head] == cluster){
        ++n_members;
        ++head;
      }

      n_ele_out += (n_members * (n_members - 1)) / 2;
      cluster_sizes.emplace_back(n_members);
    }
  }

  Rcpp::IntegerMatrix pair_indices(2, n_ele_out);
  Rcpp::IntegerVector pair_cluster_id(n_ele_out);

  auto head = cluster_id_order.begin();
  R_len_t pair_idx{};
  for(size_t idx_cluster = 0; idx_cluster < cluster_sizes.size();
      head += cluster_sizes[idx_cluster], ++idx_cluster){
    auto const cur_size = cluster_sizes[idx_cluster];
    int const cluster = cluster_id[*head];

    std::fill
      (pair_cluster_id.begin() + pair_idx,
       pair_cluster_id.begin() + pair_idx + (cur_size * (cur_size - 1)) / 2,
       cluster);

    for(unsigned j = 0; j < cur_size; ++j)
      for(unsigned i = j + 1; i < cur_size; ++i, ++pair_idx){
        pair_indices(0, pair_idx) = obs_idx[head[j]];
        pair_indices(1, pair_idx) = obs_idx[head[i]];
      }
  }

  return Rcpp::List::create(
    Rcpp::_("pair_indices") = pair_indices,
    Rcpp::_("pair_cluster_id") = pair_cluster_id);
}
