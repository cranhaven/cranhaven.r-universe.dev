// src/core/lap_eps_repair.h
// Completes an epsilon-optimal assignment to an optimal one. Pure C++, no
// Rcpp.
//
// An epsilon-scaling solver stops on a complete assignment and column prices p
// under which every row holds a column within eps of its cheapest:
//     c_ik - p_k <= c_ij - p_j + eps     (k the column of row i, j allowed).
// On integer costs with n * eps < 1 that assignment is optimal. On real costs
// no terminal eps is enough, because two assignments can differ by less than
// any fixed n * eps, and the stopping rule says nothing about which of them
// was reached.
//
// The assignment is optimal exactly when some prices satisfy the condition
// with eps = 0. Written per edge that is
//     p_j <= p_k + (c_ij - c_ik),
// a shortest-path system on the columns: an arc k -> j of length c_ij - c_ik
// for the row i holding k and every other column j that row may take. Row
// potentials u_i = c_ik - p_k and column potentials p then satisfy
// complementary slackness, which is the optimality certificate of the linear
// assignment problem. The system is solvable if and only if the arc graph has
// no negative cycle, and a negative cycle is a set of rows each moving to the
// column its successor held, at a strictly lower total cost.
//
// So the repair is label-correcting shortest paths, started from the solver's
// own prices, with negative cycles cancelled as they are found. Starting labels
// violate each arc by at most eps, so on an assignment that is already optimal
// the corrections are small and propagate a short way. Every cancellation
// strictly lowers the objective, and between cancellations the labels only
// decrease, so the procedure ends.
//
// The arc length c_ij - c_ik is formed before it meets a label. A dummy row
// padding a rectangular problem has the same cost on every column, so its arcs
// are exactly zero, where adding the costs to a label one at a time would round
// at the scale of the dummy cost.
//
// Labels are updated with the arc length added rounding toward +infinity, so a
// label is never below the exact length of the walk that set it. That makes a
// cycle in the parent pointers a proof of a negative cycle in exact arithmetic:
// around the cycle every pointer satisfies label >= parent label + arc length,
// the arc that closed it strictly, and the labels telescope to zero, which
// leaves the exact arc lengths summing below zero. Rounding to nearest gives
// no such guarantee, and a cycle of exact length zero would then read as
// negative and be cancelled forever.
#pragma once

#include "lap_cost_source.h"
#include "lap_error.h"
#include "lap_exact.h"

#include <algorithm>
#include <cstdint>
#include <vector>

namespace lap {
namespace detail {

// A node on a cycle of the parent pointers, or -1 when they form a forest.
// Each walk marks its nodes with its start; meeting a node marked by an
// earlier walk joins a path already known to end at a root.
inline int find_parent_cycle(const std::vector<int>& parent, std::vector<int>& mark) {
    const int n = static_cast<int>(parent.size());
    std::fill(mark.begin(), mark.end(), -1);
    for (int s = 0; s < n; ++s) {
        int v = s;
        while (v != -1 && mark[v] == -1) {
            mark[v] = s;
            v = parent[v];
        }
        if (v != -1 && mark[v] == s) return v;
    }
    return -1;
}

// Makes a complete square assignment optimal.
//   work       : prepared, square cost source (minimization)
//   row_ptr    : CSR offsets of the allowed columns of each row
//   cols       : CSR column indices
//   row_of_col : column -> row, a permutation, updated in place
//   col_of_row : row -> column, its inverse, updated in place
//   price      : column prices the assignment is epsilon-optimal under; used
//                as the starting labels and left as feasible column potentials
template <typename CostSourceT>
void repair_eps_optimal(const CostSourceT& work,
                        const std::vector<int64_t>& row_ptr,
                        const std::vector<int>& cols,
                        std::vector<int>& row_of_col,
                        std::vector<int>& col_of_row,
                        std::vector<double>& price) {
    const int n = static_cast<int>(row_of_col.size());
    if (n < 2) return;

    // The arc system constrains price differences only, so a common shift
    // leaves it unchanged. Prices near zero keep each label's rounding at the
    // scale of the cost differences rather than of the prices' offset, which
    // bidding against large dummy-row costs can push many orders above them.
    const double offset = *std::max_element(price.begin(), price.end());
    for (double& p : price) p -= offset;

    std::vector<int> parent(n, -1);
    std::vector<int> mark(n, -1);
    std::vector<int> ring(n);
    std::vector<char> queued(n, 1);
    for (int j = 0; j < n; ++j) ring[j] = j;
    int head = 0;
    int count = n;

    auto push = [&](int j) {
        if (queued[j]) return;
        queued[j] = 1;
        ring[(head + count) % n] = j;
        ++count;
    };

    // Moves the row holding each cycle column's parent onto that column.
    auto cancel_cycle = [&](int start) {
        std::vector<int> cycle;
        int x = start;
        do {
            cycle.push_back(x);
            x = parent[x];
        } while (x != start);

        std::vector<int> incoming(cycle.size());
        for (size_t t = 0; t < cycle.size(); ++t) {
            incoming[t] = row_of_col[parent[cycle[t]]];
        }
        for (size_t t = 0; t < cycle.size(); ++t) {
            row_of_col[cycle[t]] = incoming[t];
            col_of_row[incoming[t]] = cycle[t];
        }
        std::fill(parent.begin(), parent.end(), -1);
        for (int c : cycle) push(c);
    };

    const long long max_scans = static_cast<long long>(row_ptr[n]) * 200 + 1000;
    long long scans = 0;
    int relaxed_since_check = 0;

    for (;;) {
        while (count > 0) {
            const int k = ring[head];
            head = (head + 1) % n;
            --count;
            queued[k] = 0;
            if (++scans > max_scans) {
                LAP_THROW_CONVERGENCE("Auction: optimality repair did not converge");
            }

            const int i = row_of_col[k];
            const double c_ik = work.at(i, k);
            for (int64_t e = row_ptr[i]; e < row_ptr[i + 1]; ++e) {
                const int j = cols[e];
                if (j == k) continue;
                const double arc = exact::sum_round_up(work.at(i, j), -c_ik);
                const double label = exact::sum_round_up(price[k], arc);
                if (label < price[j]) {
                    price[j] = label;
                    parent[j] = k;
                    push(j);
                    ++relaxed_since_check;
                }
            }

            // Checking the parent pointers once per n label decreases costs
            // O(1) amortized per decrease, and a negative cycle keeps
            // decreasing labels until the check runs.
            if (relaxed_since_check >= n) {
                relaxed_since_check = 0;
                const int on_cycle = find_parent_cycle(parent, mark);
                if (on_cycle >= 0) cancel_cycle(on_cycle);
            }
        }

        const int on_cycle = find_parent_cycle(parent, mark);
        if (on_cycle < 0) break;
        cancel_cycle(on_cycle);
    }
}

}  // namespace detail
}  // namespace lap
