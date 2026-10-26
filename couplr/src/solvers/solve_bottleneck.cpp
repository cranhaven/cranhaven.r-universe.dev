// src/solvers/solve_bottleneck.cpp
// Pure C++ Bottleneck Assignment Problem (BAP) solver - NO Rcpp dependencies

#include "solve_bottleneck.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <queue>
#include <algorithm>
#include <limits>
#include <cmath>

namespace lap {

namespace {

// Hopcroft-Karp for bipartite matching
// Returns size of maximum matching
class HopcroftKarp {
public:
    int nL, nR;
    std::vector<std::vector<int>> adj;
    std::vector<int> pairL, pairR, dist;

    HopcroftKarp(int left, int right)
        : nL(left), nR(right), adj(left),
          pairL(left, -1), pairR(right, -1), dist(left) {}

    void add_edge(int u, int v) {
        adj[u].push_back(v);
    }

    void clear_edges() {
        for (auto& a : adj) a.clear();
        std::fill(pairL.begin(), pairL.end(), -1);
        std::fill(pairR.begin(), pairR.end(), -1);
    }

    bool bfs() {
        std::queue<int> q;
        for (int u = 0; u < nL; ++u) {
            if (pairL[u] < 0) {
                dist[u] = 0;
                q.push(u);
            } else {
                dist[u] = -1;
            }
        }

        bool found = false;
        while (!q.empty()) {
            int u = q.front();
            q.pop();

            for (int v : adj[u]) {
                int w = pairR[v];
                if (w < 0) {
                    found = true;
                } else if (dist[w] < 0) {
                    dist[w] = dist[u] + 1;
                    q.push(w);
                }
            }
        }
        return found;
    }

    bool dfs(int u) {
        for (int v : adj[u]) {
            int w = pairR[v];
            if (w < 0 || (dist[w] == dist[u] + 1 && dfs(w))) {
                pairL[u] = v;
                pairR[v] = u;
                return true;
            }
        }
        dist[u] = -1;
        return false;
    }

    int max_matching() {
        int matching = 0;
        while (bfs()) {
            for (int u = 0; u < nL; ++u) {
                if (pairL[u] < 0 && dfs(u)) {
                    matching++;
                }
            }
        }
        return matching;
    }

    // Get current matching as vector (left -> right, -1 if unmatched)
    std::vector<int> get_matching() const {
        return pairL;
    }
};

}  // anonymous namespace

// Main Bottleneck Assignment solver
LapResult solve_bottleneck(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Collect all unique finite costs
    std::vector<double> unique_costs;
    unique_costs.reserve(n * m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (!cost.allowed(i, j)) continue;
            double c = cost.at(i, j);
            if (std::isfinite(c)) {
                unique_costs.push_back(c);
            }
        }
    }

    if (unique_costs.empty()) {
        LAP_THROW_INFEASIBLE("Infeasible: no finite costs in matrix");
    }

    // Sort and deduplicate
    std::sort(unique_costs.begin(), unique_costs.end());
    unique_costs.erase(std::unique(unique_costs.begin(), unique_costs.end()),
                       unique_costs.end());

    // For maximize: we want to maximize the minimum edge cost
    // Transform: reverse the sorted list and search from high to low
    if (maximize) {
        std::reverse(unique_costs.begin(), unique_costs.end());
    }

    // Binary search for minimum threshold that allows perfect matching
    HopcroftKarp hk(n, m);

    auto can_match = [&](double threshold) -> bool {
        hk.clear_edges();

        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (!cost.allowed(i, j)) continue;
                double c = cost.at(i, j);
                if (!std::isfinite(c)) continue;

                if (maximize) {
                    // For maximize: include edges with cost >= threshold
                    if (c >= threshold) {
                        hk.add_edge(i, j);
                    }
                } else {
                    // For minimize: include edges with cost <= threshold
                    if (c <= threshold) {
                        hk.add_edge(i, j);
                    }
                }
            }
        }

        return hk.max_matching() == n;
    };

    // Binary search on unique_costs
    int lo = 0, hi = static_cast<int>(unique_costs.size()) - 1;
    int best = -1;

    // First check if any matching is possible
    if (!can_match(unique_costs[hi])) {
        LAP_THROW_INFEASIBLE("Infeasible: no perfect matching possible");
    }

    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (can_match(unique_costs[mid])) {
            best = mid;
            hi = mid - 1;  // Try to find smaller threshold
        } else {
            lo = mid + 1;
        }
    }

    if (best < 0) {
        LAP_THROW_INFEASIBLE("No perfect matching found");
    }

    double bottleneck = unique_costs[best];

    // Reconstruct the matching at the optimal threshold
    can_match(bottleneck);  // This sets up hk with the matching
    std::vector<int> assignment = hk.get_matching();

    // Verify matching
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
    }

    // For bottleneck, we return the bottleneck value as total_cost
    // (This is different from LAP where total_cost is the sum)
    return LapResult(std::move(assignment), bottleneck, "optimal");
}

}  // namespace lap
