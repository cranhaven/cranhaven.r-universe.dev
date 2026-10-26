// src/solvers/solve_cycle_cancel.cpp
// Pure C++ Cycle Canceling LAP solver - NO Rcpp dependencies

#include "solve_cycle_cancel.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include "../flow/flow_assign.h"
#include <vector>
#include <algorithm>
#include <limits>
#include <cmath>
#include <cstddef>

namespace lap {

namespace {

constexpr double INF_DBL = std::numeric_limits<double>::infinity();

struct Edge {
    int to;
    int cap;
    double cost;
    int rev;
};

// Returns the index of the forward arc inside adj[u], so a flow found
// elsewhere can be pushed through it without searching for it again.
int add_edge(std::vector<std::vector<Edge>>& adj, int u, int v, int cap, double cost) {
    // The reverse index is read after the forward arc is in place, so that a
    // self-loop gets two distinct slots rather than two arcs pointing at one.
    int fwd_idx = adj[u].size();
    adj[u].push_back({v, cap, cost, -1});
    int rev_idx = adj[v].size();
    adj[v].push_back({u, 0, -cost, fwd_idx});
    adj[u][fwd_idx].rev = rev_idx;
    return fwd_idx;
}

// Move one unit through the arc at adj[u][idx], taking it out of the forward
// capacity and giving it to the reverse.
void push_unit(std::vector<std::vector<Edge>>& adj, int u, int idx) {
    Edge& e = adj[u][idx];
    e.cap -= 1;
    adj[e.to][e.rev].cap += 1;
}

bool karp_min_mean_cycle(const std::vector<std::vector<Edge>>& adj,
                         double& mean, std::vector<int>& cycle_nodes) {
    const int N = adj.size();

    std::vector<std::vector<std::pair<int,double>>> incoming(N);

    for (int u = 0; u < N; ++u) {
        for (const Edge& e : adj[u]) {
            if (e.cap > 0) {
                incoming[e.to].push_back({u, e.cost});
            }
        }
    }

    std::vector<std::vector<double>> dp(N + 1, std::vector<double>(N, INF_DBL));
    std::vector<std::vector<int>> par(N + 1, std::vector<int>(N, -1));

    for (int v = 0; v < N; ++v) {
        dp[0][v] = 0.0;
    }

    for (int k = 1; k <= N; ++k) {
        for (int v = 0; v < N; ++v) {
            double best = dp[k][v];
            int best_u = -1;

            for (const auto& [u, w] : incoming[v]) {
                if (dp[k-1][u] < INF_DBL) {
                    double cand = dp[k-1][u] + w;
                    if (cand < best) {
                        best = cand;
                        best_u = u;
                    }
                }
            }

            dp[k][v] = best;
            par[k][v] = best_u;
        }
    }

    double mu = INF_DBL;
    int arg_v = -1;

    for (int v = 0; v < N; ++v) {
        if (dp[N][v] == INF_DBL) continue;

        double max_ratio = -INF_DBL;

        for (int k = 0; k < N; ++k) {
            if (dp[k][v] == INF_DBL) continue;
            int denom = N - k;
            if (denom <= 0) continue;

            double ratio = (dp[N][v] - dp[k][v]) / denom;
            if (ratio > max_ratio) {
                max_ratio = ratio;
            }
        }

        if (max_ratio < mu) {
            mu = max_ratio;
            arg_v = v;
        }
    }

    if (!(mu < -1e-12) || arg_v == -1) {
        return false;
    }

    int x = arg_v;
    for (int i = 0; i < N; ++i) {
        x = par[N][x];
        if (x == -1) break;
    }

    if (x == -1) return false;

    std::vector<bool> seen(N, false);
    std::vector<int> path;
    int cur = x;

    while (!seen[cur]) {
        seen[cur] = true;
        path.push_back(cur);
        cur = par[N][cur];
        if (cur == -1) return false;
    }

    auto it = std::find(path.begin(), path.end(), cur);
    if (it == path.end()) return false;

    cycle_nodes.clear();
    cycle_nodes.insert(cycle_nodes.end(), it, path.end());
    cycle_nodes.push_back(cur);

    mean = mu;
    return true;
}

}  // anonymous namespace

LapResult solve_cycle_cancel(const CostMatrix& cost, bool maximize) {
    const int n0 = static_cast<int>(cost.nrow);
    const int m0 = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n0 == 0 || m0 == 0) {
        LAP_THROW_DIMENSION("Cost matrix cannot be empty");
    }

    // Handle transposition for rectangular matrices (n > m)
    bool transposed = false;
    CostMatrix C = cost;
    int n = n0, m = m0;

    if (n0 > m0) {
        // Transpose the matrix
        C = CostMatrix(m0, n0);
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                C.at(j, i) = cost.at(i, j);
                C.mask[static_cast<size_t>(flat_index(j, i, n0))] =
                    cost.mask[static_cast<size_t>(flat_index(i, j, m0))];
            }
        }
        n = m0;
        m = n0;
        transposed = true;
    }

    // Find maximum cost for transformation if maximizing
    double cmax = 0.0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double v = C.at(i, j);
            if (C.allowed(i, j) && std::isfinite(v) && v > cmax) {
                cmax = v;
            }
        }
    }

    // Build flow network
    // Nodes: 0..n-1 (left), n..n+m-1 (right), s=n+m, t=n+m+1
    const int s = n + m;
    const int t = n + m + 1;
    const int N = n + m + 2;
    std::vector<std::vector<Edge>> adj(N);

    // The costs the network carries: a maximization instance runs on cmax - v,
    // which keeps every arc cost non-negative so that a negative cycle means
    // the same thing in both directions.
    CostMatrix W(n, m);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double v = C.at(i, j);
            if (!C.allowed(i, j) || !std::isfinite(v)) {
                W.forbid(i, j);
                continue;
            }
            W.at(i, j) = maximize ? (cmax - v) : v;
        }
    }

    // Source to left nodes
    std::vector<int> src_arc(static_cast<size_t>(n), -1);
    for (int i = 0; i < n; ++i) {
        src_arc[static_cast<size_t>(i)] = add_edge(adj, s, i, 1, 0.0);
    }

    // Left to right edges (cost edges)
    std::vector<int> pair_arc(static_cast<size_t>(n) * static_cast<size_t>(m), -1);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (!W.allowed(i, j)) continue;
            pair_arc[static_cast<size_t>(flat_index(i, j, m))] =
                add_edge(adj, i, n + j, 1, W.at(i, j));
        }
    }

    // Right nodes to sink
    std::vector<int> sink_arc(static_cast<size_t>(m), -1);
    for (int j = 0; j < m; ++j) {
        sink_arc[static_cast<size_t>(j)] = add_edge(adj, n + j, t, 1, 0.0);
    }

    // Find the initial feasible flow: the assignment over the same arcs, solved
    // by the flow model, pushed into the residual graph the cancelling below
    // runs on. Residual capacities are a function of the net flow alone, so a
    // matching is all it takes to leave the graph in the state a
    // successive-shortest-path phase would have left it in.
    FlowOptions opts;
    opts.relax_eps = 0.0;
    opts.return_potentials = false;

    SourceOracle<CostMatrix> oracle(W);
    const AssignmentFlow flow = solve_assignment_flow(oracle, opts);

    if (flow.n_matched < n) {
        LAP_THROW_INFEASIBLE("Infeasible: forbidden edges block perfect matching");
    }

    for (int i = 0; i < n; ++i) {
        const int j = flow.match[static_cast<size_t>(i)];
        push_unit(adj, s, src_arc[static_cast<size_t>(i)]);
        push_unit(adj, i, pair_arc[static_cast<size_t>(flat_index(i, j, m))]);
        push_unit(adj, n + j, sink_arc[static_cast<size_t>(j)]);
    }

    // Iteratively cancel negative cost cycles using Karp's algorithm
    const long long max_iters = static_cast<long long>(n) * m * 10;
    long long iters = 0;

    while (iters < max_iters) {
        ++iters;

        double mu;
        std::vector<int> nodes;

        bool found = karp_min_mean_cycle(adj, mu, nodes);

        if (!found) break;

        // Find cycle edges and minimum capacity
        std::vector<Edge*> cyc_edges;
        int theta = 1000000;

        for (size_t k = 0; k < nodes.size() - 1; ++k) {
            int a = nodes[k];
            int b = nodes[k + 1];

            Edge* found_edge = nullptr;
            for (Edge& e : adj[a]) {
                if (e.cap > 0 && e.to == b) {
                    found_edge = &e;
                    break;
                }
            }

            if (found_edge) {
                cyc_edges.push_back(found_edge);
                if (found_edge->cap < theta) theta = found_edge->cap;
            }
        }

        if (theta <= 0 || cyc_edges.empty()) break;

        // Cancel the cycle
        for (Edge* e : cyc_edges) {
            e->cap -= theta;
            int rev_idx = e->rev;
            adj[e->to][rev_idx].cap += theta;
        }
    }

    // Extract assignment from flow
    std::vector<int> assignment(n0, -1);

    if (!transposed) {
        // Normal orientation
        for (int i = 0; i < n; ++i) {
            for (const Edge& e : adj[i]) {
                int j_node = e.to;
                if (j_node >= n && j_node < n + m) {
                    int j = j_node - n;
                    // Flow was sent if capacity is now 0 (started at 1)
                    if (e.cap == 0) {
                        assignment[i] = j;
                        break;
                    }
                }
            }
        }
    } else {
        // Transposed: original rows are now columns
        for (int i = 0; i < n; ++i) {
            for (const Edge& e : adj[i]) {
                int j_node = e.to;
                if (j_node >= n && j_node < n + m) {
                    int j = j_node - n;
                    if (e.cap == 0) {
                        assignment[j] = i;
                        break;
                    }
                }
            }
        }
    }

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n0; ++i) {
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
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
