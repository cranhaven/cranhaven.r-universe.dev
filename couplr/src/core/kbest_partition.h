// src/core/kbest_partition.h
// Murty/Lawler partitioning: the one k-best enumeration both k-best backends
// run.
#pragma once

#include <Rcpp.h>
#include <functional>
#include <utility>
#include <vector>

// One solution of the enumeration.
struct KBestSolution {
  std::vector<int> match;  // 1-based columns, length n; 0 = unmatched row
  double order_cost;       // the key the enumeration is ordered on
};

// Solves one constrained instance. The returned cost is the ordering key, so a
// maximizing caller negates it and the enumeration stays a min-heap.
using KBestOracle =
    std::function<std::pair<std::vector<int>, double>(Rcpp::NumericMatrix)>;

// Enumerate the k best assignments by partitioning the space around each
// solution as it is emitted.
//
// The children of a node with solution e_1..e_n are disjoint by construction:
// child i forces e_1..e_{i-1} and forbids e_i, so an assignment that agrees
// with the node up to i-1 and differs at i lies in exactly one of them, and
// the node's own solution in none. Appending a forbidden edge without forcing
// the prefix leaves the children overlapping, and a child dropped for
// reproducing an already-emitted solution then takes with it the assignments
// that are optimal only inside its own forbid set.
//
// A node's `next_i` is where its forced prefix ends, so its children branch
// only from there on: every assignment in the subspace already agrees on the
// positions before it. Forcing the prefix subsumes the parent's own forbidden
// edge, because position i is forced to a column the parent forbade there.
std::vector<KBestSolution> kbest_by_partition(const Rcpp::NumericMatrix& cost,
                                              int k,
                                              const KBestOracle& solve_one);
