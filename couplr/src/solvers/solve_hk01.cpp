// src/solvers/solve_hk01.cpp
// Pure C++ Hopcroft-Karp solver for 0/1 costs - NO Rcpp dependencies

#include "solve_hk01.h"
#include "solve_csflow.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <queue>
#include <limits>
#include <cmath>
#include <algorithm>

namespace lap {

// Helper: Analyze cost palette to determine if matrix is uniform or binary {0,1}
struct Palette {
    bool all_equal = false;
    bool is_binary01 = false;
    double equal_value = 0.0;
};

static Palette analyze_palette(const CostMatrix& cost) {
    Palette p;
    double a = 0.0, b = 0.0;
    bool has_a = false, has_b = false;

    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (!cost.allowed(i, j)) continue;
            double c = cost.at(i, j);
            if (!std::isfinite(c)) continue;

            if (!has_a) {
                a = c;
                has_a = true;
                continue;
            }
            if (std::fabs(c - a) < TOL) continue;

            if (!has_b) {
                b = c;
                has_b = true;
                continue;
            }
            if (std::fabs(c - b) < TOL) continue;

            // Found >= 3 distinct values -> not 0/1 or all equal
            p.all_equal = false;
            p.is_binary01 = false;
            return p;
        }
    }

    if (!has_a) {
        // Empty or all forbidden -> treat as uniform
        p.all_equal = true;
        p.equal_value = 0.0;
        return p;
    }

    if (!has_b) {
        // Only one distinct value
        p.all_equal = true;
        p.equal_value = a;
        return p;
    }

    // Two distinct values: check if they are {0,1}
    double x = std::min(a, b);
    double y = std::max(a, b);
    p.is_binary01 = (std::fabs(x) < TOL) && (std::fabs(y - 1.0) < TOL);
    p.all_equal = false;

    return p;
}

// Hopcroft-Karp bipartite matching structure
struct HK {
    int nL, nR;
    std::vector<std::vector<int>> adj;  // adjacency: left -> right nodes (0-based)
    std::vector<int> pairU, pairV, dist;  // matching and distances

    HK(int nL, int nR)
        : nL(nL), nR(nR)
        , adj(nL)
        , pairU(nL + 1, 0)  // 1-based internally, 0 = unmatched
        , pairV(nR + 1, 0)
        , dist(nL + 1, 0) {}

    void add_edge(int u, int v) {
        adj[u].push_back(v);
    }

    // BFS to build layers from free left nodes
    bool bfs() {
        std::queue<int> q;
        const int INF = std::numeric_limits<int>::max();

        for (int u = 0; u < nL; ++u) {
            if (pairU[u + 1] == 0) {
                dist[u + 1] = 0;
                q.push(u + 1);
            } else {
                dist[u + 1] = INF;
            }
        }

        bool reached_free = false;
        while (!q.empty()) {
            int u1 = q.front();
            q.pop();
            int u = u1 - 1;

            for (int v : adj[u]) {
                int v1 = v + 1;
                int u1next = pairV[v1];  // matched partner on left (or 0)

                if (u1next == 0) {
                    reached_free = true;  // can reach a free right node
                } else if (dist[u1next] == INF) {
                    dist[u1next] = dist[u1] + 1;
                    q.push(u1next);
                }
            }
        }

        return reached_free;
    }

    // DFS to find augmenting path
    bool dfs(int u1) {
        int u = u1 - 1;
        for (int v : adj[u]) {
            int v1 = v + 1;
            int u1next = pairV[v1];

            if (u1next == 0 || (dist[u1next] == dist[u1] + 1 && dfs(u1next))) {
                pairU[u1] = v1;
                pairV[v1] = u1;
                return true;
            }
        }

        dist[u1] = std::numeric_limits<int>::max();
        return false;
    }

    // Find maximum matching
    int max_matching() {
        int matching = 0;
        while (bfs()) {
            for (int u = 0; u < nL; ++u) {
                if (pairU[u + 1] == 0 && dfs(u + 1)) {
                    matching++;
                }
            }
        }
        return matching;
    }
};

// Forward declaration of csflow solver (fallback for binary {0,1} without perfect zero matching)
// This will be linked from solve_csflow.cpp when available
// For now, we'll throw an error if we need it
LapResult solve_csflow(const CostMatrix& cost, bool maximize);

LapResult solve_hk01(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Prepare working costs. For maximize, flip finite allowed costs via
    // c' = cmax - c rather than negating: this keeps a {0,1} palette as {0,1}
    // (negation makes it {0,-1}, which the palette check below rejects), so the
    // binary and uniform fast paths still engage. Forbidden edges are read
    // through the mask and never scanned, so they need no transform; the total
    // is recomputed from the original matrix below, so the shift never leaks in.
    CostMatrix work = cost;
    if (maximize) {
        double cmax = -std::numeric_limits<double>::infinity();
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < m; ++j)
                if (work.allowed(i, j) && std::isfinite(work.at(i, j)))
                    cmax = std::max(cmax, work.at(i, j));
        if (std::isfinite(cmax)) {
            for (int i = 0; i < n; ++i)
                for (int j = 0; j < m; ++j)
                    if (work.allowed(i, j) && std::isfinite(work.at(i, j)))
                        work.at(i, j) = cmax - work.at(i, j);
        }
    }

    // Check feasibility
    ensure_each_row_has_option(work.mask, n, m);

    // Analyze cost palette
    Palette pal = analyze_palette(work);

    // Build HK graph
    HK hk(n, m);
    std::vector<int> assignment(n, -1);

    if (pal.all_equal) {
        // All allowed edges have equal cost -> any perfect matching is optimal
        // Build graph with all allowed edges
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (!work.allowed(i, j)) continue;
                double c = work.at(i, j);
                if (std::isfinite(c)) {
                    hk.add_edge(i, j);
                }
            }
        }

        int mm = hk.max_matching();
        if (mm < n) {
            LAP_THROW_INFEASIBLE("Could not find perfect matching");
        }

        // Extract matching (convert from 1-based to 0-based)
        for (int i = 0; i < n; ++i) {
            int v1 = hk.pairU[i + 1];
            if (v1 == 0) {
                LAP_THROW_INFEASIBLE("Row not matched");
            }
            assignment[i] = v1 - 1;  // Convert to 0-based
        }

    } else if (pal.is_binary01) {
        // Costs are {0, 1} -> try to find perfect matching using only zero-cost edges
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (!work.allowed(i, j)) continue;
                double c = work.at(i, j);
                if (std::isfinite(c) && std::fabs(c) < TOL) {
                    hk.add_edge(i, j);
                }
            }
        }

        int mm = hk.max_matching();
        if (mm == n) {
            // Perfect matching with zero cost!
            for (int i = 0; i < n; ++i) {
                int v1 = hk.pairU[i + 1];
                if (v1 == 0) {
                    LAP_THROW_INFEASIBLE("Row not matched");
                }
                assignment[i] = v1 - 1;
            }
        } else {
            // Optimum needs some 1-cost edges: the 0-cost subgraph has no
            // perfect matching. Fall back to the exact weighted solver on the
            // original costs (the rcpp twin does the same via lap_solve_csflow).
            return solve_csflow(cost, maximize);
        }

    } else {
        LAP_THROW("hk01: cost matrix is neither all-equal nor binary {0,1} after preparation");
    }

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Row not matched in result");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
