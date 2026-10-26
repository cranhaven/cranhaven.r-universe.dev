// src/solvers/solve_ssap_bucket.cpp
// Pure C++ SSAP with Dial's bucket queue - NO Rcpp dependencies

#include "solve_ssap_bucket.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <algorithm>
#include <limits>
#include <cmath>

namespace lap {

namespace {

constexpr int INF_INT = 1000000000;

// Integrality tolerance in *scaled* units. It is deliberately fixed, not
// proportional to the scale: a scale-proportional tolerance grows to exceed
// half a quantum at large scales, at which point every real reads as "integral"
// (any value lies within half a unit of some integer) and the rounding bug
// returns silently. 1e-6 sits well above the ~1e-7 floating-point noise at the
// solver's largest usable scaled magnitude (< INF_INT) yet far below any
// genuine non-integer's scaled fractional part.
constexpr double SCALE_INT_TOL = 1e-6;

// Smallest power of ten that renders every cost integral to within
// SCALE_INT_TOL. Returns {scale, true} on success. Returns {scale, false} when
// no power of ten within the solver's representable range makes the costs
// integral: the caller then rejects the problem rather than silently rounding,
// which can flip which permutation is optimal.
std::pair<long long, bool> find_scale_factor(const std::vector<double>& vals) {
    if (vals.empty()) return {1, false};

    double max_abs = 0.0;
    for (double v : vals) max_abs = std::max(max_abs, std::abs(v));
    if (max_abs == 0.0) return {1, true};

    for (long long scale = 1; ; scale *= 10) {
        const double s = static_cast<double>(scale);
        bool all_int = true;
        for (double v : vals) {
            const double scaled = v * s;
            if (std::abs(scaled - std::round(scaled)) > SCALE_INT_TOL) {
                all_int = false;
                break;
            }
        }
        if (all_int) return {scale, true};

        // Stop before the next scale pushes the largest cost past the integer
        // range this solver uses; beyond it the magnitude guard rejects anyway.
        if (max_abs * s * 10.0 >= static_cast<double>(INF_INT)) {
            return {scale, false};
        }
    }
}

// Min-cost flow edge structure
struct MCFEdge {
    int to;
    int rev;
    int cap;
    int cost;
};

struct MinCostFlowBuckets {
    int N;
    std::vector<std::vector<MCFEdge>> g;

    explicit MinCostFlowBuckets(int N_) : N(N_), g(N_) {}

    void add_edge(int fr, int to, int cap, int cost) {
        MCFEdge fwd{to, (int)g[to].size(), cap,  cost};
        MCFEdge rev{fr, (int)g[fr].size(), 0,   -cost};
        g[fr].push_back(fwd);
        g[to].push_back(rev);
    }

    // Largest reduced edge cost over the current residual graph. Dial's queue
    // needs one bucket per distinct reduced cost, so this sizes the ring.
    int max_reduced_cost(const std::vector<int>& h) const {
        int maxc = 0;
        for (int v = 0; v < N; ++v) {
            for (const MCFEdge &e : g[v]) {
                if (e.cap <= 0) continue;
                int rcost = e.cost + h[v] - h[e.to];
                if (rcost > maxc) maxc = rcost;
            }
        }
        return maxc;
    }

    // Successive shortest path with bucket queue
    // Returns (flow, total_cost)
    std::pair<int, long long> min_cost_flow(int s, int t, int maxf) {
        const int INF = INF_INT;
        std::vector<int> h(N, 0);        // node potentials
        std::vector<int> dist(N);
        std::vector<int> prevv(N);       // previous vertex
        std::vector<int> preve(N);       // previous edge index

        // Dial's queue as a ring of intrusive lists. Entries live in a pool so a
        // node can sit in several buckets at once (decrease-key by re-insert,
        // resolved by the dist[] staleness check on pop).
        std::vector<int> head;           // bucket -> pool index, -1 when empty
        std::vector<int> pool_node;
        std::vector<int> pool_next;

        int flow = 0;
        long long total_cost = 0;

        while (flow < maxf) {
            // Dijkstra with Dial's bucket queue for reduced costs
            std::fill(dist.begin(), dist.end(), INF);
            dist[s] = 0;

            // Every key pushed lies in [dcur, dcur + maxC], so maxC + 1 buckets
            // indexed cyclically hold the whole live frontier. Sizing the ring by
            // the largest reduced cost instead of the largest distance is what
            // keeps this O(maxC) rather than O(N * maxC) in memory.
            const int ring = max_reduced_cost(h) + 1;
            head.assign(ring, -1);
            pool_node.clear();
            pool_next.clear();

            int maxd = 0;
            int bcur = 0;  // ring slot matching dcur, advanced in lockstep

            auto push_bucket = [&](int d, int v) {
                if (d < 0) d = 0;
                if (d >= INF) return;
                int b = d % ring;
                pool_node.push_back(v);
                pool_next.push_back(head[b]);
                head[b] = (int)pool_node.size() - 1;
                if (d > maxd) maxd = d;
            };

            push_bucket(0, s);
            int dcur = 0;

            // CRITICAL: Run FULL shortest path tree computation
            // Do NOT break when reaching t - need all distances for potential update
            while (dcur <= maxd) {
                // Find next non-empty bucket
                while (dcur <= maxd && head[bcur] == -1) {
                    ++dcur;
                    if (++bcur == ring) bcur = 0;
                }
                if (dcur > maxd) break;

                // Drain the slot. Relaxing at reduced cost 0 can refill it, so
                // keep draining until it stays empty.
                while (head[bcur] != -1) {
                    int idx = head[bcur];
                    head[bcur] = -1;

                    while (idx != -1) {
                        int v = pool_node[idx];
                        idx = pool_next[idx];

                        // Skip if stale. Live keys span at most one lap of the
                        // ring, so anything in this slot that is not at dcur has
                        // already been finalised at a smaller distance.
                        if (dist[v] != dcur) continue;

                        // Relax edges from v
                        for (int ei = 0; ei < (int)g[v].size(); ++ei) {
                            const MCFEdge &e = g[v][ei];
                            if (e.cap <= 0) continue;

                            // Reduced cost (should be >= 0 with correct potentials)
                            int rcost = e.cost + h[v] - h[e.to];
                            if (rcost < 0) rcost = 0;  // defensive clamp

                            int nd = dist[v] + rcost;
                            if (nd < dist[e.to] && nd < INF) {
                                dist[e.to] = nd;
                                prevv[e.to] = v;
                                preve[e.to] = ei;
                                push_bucket(nd, e.to);
                            }
                        }
                    }
                }
            }

            // Check if path to t exists
            if (dist[t] == INF) {
                break;  // No augmenting path
            }

            // Update potentials using computed distances
            for (int v = 0; v < N; ++v) {
                if (dist[v] < INF) {
                    h[v] += dist[v];
                }
            }

            // Find bottleneck capacity along path
            int d = maxf - flow;
            int v = t;
            while (v != s) {
                const MCFEdge &e = g[prevv[v]][preve[v]];
                if (e.cap < d) d = e.cap;
                v = prevv[v];
            }
            if (d <= 0) break;

            flow += d;

            // CRITICAL FIX: Cost of augmenting path is h[t] - h[s]
            // After potential updates, this equals the shortest path cost
            total_cost += (long long)d * (h[t] - h[s]);

            // Augment flow along path
            v = t;
            while (v != s) {
                MCFEdge &e = g[prevv[v]][preve[v]];
                e.cap -= d;
                g[v][e.rev].cap += d;
                v = prevv[v];
            }
        }

        return {flow, total_cost};
    }
};

}  // anonymous namespace


LapResult solve_ssap_bucket(const CostMatrix& cost, bool maximize) {
    const int n0 = static_cast<int>(cost.nrow);
    const int m0 = static_cast<int>(cost.ncol);

    // Handle empty matrix
    if (n0 == 0 || m0 == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Ensure n <= m by transposing if needed
    bool transposed = false;
    CostMatrix C = cost;
    int n = n0, m = m0;

    if (n0 > m0) {
        // Transpose: build new matrix
        C = CostMatrix(m0, n0);
        for (int i = 0; i < m0; ++i) {
            for (int j = 0; j < n0; ++j) {
                C.at(i, j) = cost.at(j, i);
                C.mask[static_cast<size_t>(flat_index(i, j, n0))] =
                    cost.mask[static_cast<size_t>(flat_index(j, i, m0))];
            }
        }
        n = m0;
        m = n0;
        transposed = true;
    }

    // Collect finite values for scaling
    std::vector<double> finite_vals;
    finite_vals.reserve(static_cast<size_t>(static_cast<int64_t>(n) * m));
    double min_val = 0.0;  // Track minimum for shifting
    bool has_finite = false;

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (C.allowed(i, j)) {
                double v = C.at(i, j);
                if (std::isfinite(v)) {
                    finite_vals.push_back(v);
                    if (!has_finite || v < min_val) {
                        min_val = v;
                        has_finite = true;
                    }
                }
            }
        }
    }

    if (finite_vals.empty()) {
        LAP_THROW_INFEASIBLE("No finite costs found");
    }

    // Shift to make all costs non-negative
    double shift = 0.0;
    if (min_val < 0.0) {
        shift = -min_val;  // Add this to make all >= 0
    }

    // Find appropriate scaling factor for absolute values
    std::vector<double> abs_vals;
    abs_vals.reserve(finite_vals.size());
    for (double v : finite_vals) {
        abs_vals.push_back(std::abs(v + shift));
    }

    auto [scale, exact] = find_scale_factor(abs_vals);
    if (!exact) {
        LAP_THROW("ssap_bucket: costs need finer fractional precision than the "
                  "integer bucket solver can represent exactly; use method = "
                  "'jv' or 'auction'");
    }

    // Guard against cost magnitudes this integer-bucket solver cannot handle:
    // the int cost matrix would overflow / collide with the INF_INT sentinel,
    // and Dial's bucket queue allocates one bucket per distance unit up to
    // ~n * max_cost, which blows up memory. Throw a clear error rather than
    // silently returning a wrong "optimal".
    {
        long long max_scaled = 0;
        for (double v : finite_vals) {
            long long s = std::llround((v + shift) * static_cast<double>(scale));
            if (s > max_scaled) max_scaled = s;
        }
        const long long BUCKET_BUDGET = 100000000LL;  // 1e8
        if (max_scaled >= INF_INT ||
            static_cast<long long>(n) * max_scaled > BUCKET_BUDGET) {
            LAP_THROW("ssap_bucket: cost magnitudes too large for the integer "
                      "bucket solver; use method = 'jv' or 'auction'");
        }
    }

    // Build integer cost matrix: shift then scale
    std::vector<std::vector<int>> CI(n, std::vector<int>(m, INF_INT));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (C.allowed(i, j)) {
                double v = C.at(i, j);
                if (std::isfinite(v)) {
                    // Shift to non-negative, then scale to integer
                    CI[i][j] = (int)std::llround((v + shift) * scale);
                }
            }
        }
    }

    // Transform for maximize
    if (maximize) {
        int cmax = 0;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (CI[i][j] < INF_INT && CI[i][j] > cmax) {
                    cmax = CI[i][j];
                }
            }
        }
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (CI[i][j] < INF_INT) {
                    CI[i][j] = cmax - CI[i][j];
                }
            }
        }
    }

    // Feasibility check
    for (int i = 0; i < n; ++i) {
        bool has_edge = false;
        for (int j = 0; j < m; ++j) {
            if (CI[i][j] < INF_INT) {
                has_edge = true;
                break;
            }
        }
        if (!has_edge) {
            LAP_THROW_INFEASIBLE("Infeasible: row " + std::to_string(i + 1) +
                                 " has no valid edges");
        }
    }

    // Build min-cost flow network
    // Nodes: 0..n-1 (rows), n..n+m-1 (cols), s=n+m, t=n+m+1
    int N = n + m + 2;
    int s = n + m;
    int t = n + m + 1;

    MinCostFlowBuckets mcf(N);

    // Source to rows
    for (int i = 0; i < n; ++i) {
        mcf.add_edge(s, i, 1, 0);
    }

    // Rows to columns (assignment edges)
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (CI[i][j] < INF_INT) {
                mcf.add_edge(i, n + j, 1, CI[i][j]);
            }
        }
    }

    // Columns to sink
    for (int j = 0; j < m; ++j) {
        mcf.add_edge(n + j, t, 1, 0);
    }

    // Solve min-cost flow
    auto [flow, total_cost_int] = mcf.min_cost_flow(s, t, n);
    (void)total_cost_int;  // We'll compute from original costs

    if (flow != n) {
        LAP_THROW_INFEASIBLE("Infeasible: could not find perfect matching");
    }

    // Extract matching from residual graph
    std::vector<int> row_match(n, -1);
    for (int i = 0; i < n; ++i) {
        for (const auto &e : mcf.g[i]) {
            int j = e.to - n;
            if (j >= 0 && j < m) {
                // Check if reverse edge has positive capacity (flow was sent)
                const MCFEdge &rev = mcf.g[e.to][e.rev];
                if (rev.cap > 0) {
                    row_match[i] = j;
                    break;
                }
            }
        }
    }

    // Build output with original cost (NOT shifted/scaled)
    std::vector<int> assignment(n0, -1);
    double total = 0.0;

    if (!transposed) {
        for (int i = 0; i < n; ++i) {
            if (row_match[i] < 0) {
                LAP_THROW("Internal error: unmatched row");
            }
            assignment[i] = row_match[i];  // 0-based

            // Use ORIGINAL cost
            if (!cost.allowed(i, row_match[i])) {
                LAP_THROW_INFEASIBLE("Chosen forbidden edge");
            }
            double c = cost.at(i, row_match[i]);
            if (!std::isfinite(c)) {
                LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
            }
            total += c;
        }
    } else {
        // Transposed: map back to original orientation
        for (int i = 0; i < n; ++i) {
            int j = row_match[i];
            if (j < 0) {
                LAP_THROW("Internal error: unmatched row (transposed)");
            }
            assignment[j] = i;  // 0-based

            // Use ORIGINAL cost
            if (!cost.allowed(j, i)) {
                LAP_THROW_INFEASIBLE("Chosen forbidden edge");
            }
            double c = cost.at(j, i);
            if (!std::isfinite(c)) {
                LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
            }
            total += c;
        }
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
