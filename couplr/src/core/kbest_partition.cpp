// src/core/kbest_partition.cpp
// [[Rcpp::plugins(cpp17)]]
#include "kbest_partition.h"

#include <queue>
#include <string>
#include <unordered_set>

#include "lap_utils_rcpp.h"

namespace {

// A subspace of the assignment space, and the best assignment in it.
//
// The subspace is stated by a forced prefix -- rows 1..prefix_len pinned to
// `forced` -- together with the columns already excluded at the first free
// row, prefix_len + 1. Those two pieces are the whole constraint set: every
// row past the branch row is unconstrained, and every row before it is pinned.
struct PartitionNode {
  std::vector<int> match;      // 1-based columns, length n
  double cost;                 // ordering key
  std::vector<int> forced;     // 1-based columns for rows 1..forced.size()
  std::vector<int> excluded;   // 1-based columns barred at row forced.size()+1
};

struct NodeGreater {
  bool operator()(const PartitionNode& a, const PartitionNode& b) const {
    return a.cost > b.cost;
  }
};

// The instance a subspace states: the forced prefix pinned, and every excluded
// column struck out at the branch row.
Rcpp::NumericMatrix constrain(const Rcpp::NumericMatrix& cost,
                              const std::vector<int>& forced,
                              const std::vector<int>& excluded) {
  Rcpp::NumericMatrix M = apply_constraints(cost, forced, 0, 0);
  const int branch_row = static_cast<int>(forced.size());  // 0-based
  if (branch_row < M.nrow()) {
    for (int col1 : excluded) {
      const int j = col1 - 1;
      if (j >= 0 && j < M.ncol()) M(branch_row, j) = NA_REAL;
    }
  }
  return M;
}

}  // namespace

std::vector<KBestSolution> kbest_by_partition(const Rcpp::NumericMatrix& cost,
                                              int k,
                                              const KBestOracle& solve_one) {
  std::vector<KBestSolution> out;
  const int n = cost.nrow();
  const int m = cost.ncol();
  if (k < 1 || n == 0 || m == 0) return out;

  std::pair<std::vector<int>, double> best = solve_one(cost);

  // The subspaces are disjoint, so a child's optimum cannot repeat a solution
  // already emitted from another branch. The key set stays as a guard against
  // an oracle that answers a tie differently on two calls.
  std::unordered_set<std::string> seen;
  seen.reserve(1024);
  seen.insert(match_to_key(best.first));

  out.reserve(static_cast<size_t>(k));
  out.push_back(KBestSolution{best.first, best.second});

  std::priority_queue<PartitionNode, std::vector<PartitionNode>, NodeGreater> pq;

  // Partition a node's subspace around its own solution `f`, minus `f` itself.
  //
  // Child i pins rows 1..i-1 to f and bars f_i at row i, for each row i from
  // the node's branch row to n. An assignment in the subspace other than f
  // first differs from f at exactly one row, and that row names the one child
  // it belongs to, so the children are disjoint and together they are
  // everything but f.
  //
  // The node's own excluded columns carry into its first child only: from the
  // second child on, row prefix_len + 1 is pinned to f's column there, which
  // is not one of the excluded ones, so the exclusion is already honoured.
  auto branch = [&](const PartitionNode& node) {
    const std::vector<int>& f = node.match;
    const int start = static_cast<int>(node.forced.size()) + 1;  // 1-based row

    for (int i = start; i <= n; ++i) {
      const int col_i = f[static_cast<size_t>(i - 1)];
      if (col_i == 0) continue;  // unmatched row, nothing to exclude

      std::vector<int> forced(f.begin(), f.begin() + (i - 1));
      std::vector<int> excluded;
      if (i == start) excluded = node.excluded;
      excluded.push_back(col_i);

      Rcpp::NumericMatrix Mi = constrain(cost, forced, excluded);
      if (!has_valid_matching(Mi)) continue;

      std::pair<std::vector<int>, double> child = solve_one(Mi);
      if (seen.insert(match_to_key(child.first)).second) {
        pq.push(PartitionNode{child.first, child.second,
                              std::move(forced), std::move(excluded)});
      }
    }
  };

  branch(PartitionNode{best.first, best.second, {}, {}});

  while (static_cast<int>(out.size()) < k && !pq.empty()) {
    PartitionNode node = pq.top();
    pq.pop();

    out.push_back(KBestSolution{node.match, node.cost});
    branch(node);
  }

  return out;
}
