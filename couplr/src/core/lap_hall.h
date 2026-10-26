// src/core/lap_hall.h
// Infeasibility witness for the bipartite assignment problem - NO Rcpp dependencies
//
// A row-perfect assignment exists exactly when Hall's condition holds: every
// set S of rows reaches at least |S| distinct columns, |N(S)| >= |S|. When the
// condition fails, the reason is a particular deficient set of rows, and this
// header computes that set rather than reporting bare infeasibility.
//
// The construction is Koenig's. Take a maximum-cardinality matching M in the
// admissibility graph and let Z be the vertex set reachable by alternating
// paths starting from the unmatched rows: a row joins Z when it is unmatched
// or when a matched edge leads to it from a column already in Z, and a column
// joins Z when a non-matched edge leads to it from a row already in Z. Write
// S for the rows of Z and N(S) for the columns of Z.
//
// Every column of Z is matched, since an unmatched one would complete an
// augmenting path and contradict the maximality of M, and its partner row lies
// in Z by the rule above. Conversely a matched row of Z entered Z through its
// own partner column, so that column lies in Z too. Pairing each column of Z
// with its partner row is therefore a bijection between the columns of Z and
// the matched rows of Z, which leaves
//
//     |S| - |N(S)| = number of unmatched rows = nrow - |M|,
//
// positive exactly when M is not row-perfect. S also has no admissible edge
// leaving N(S): a non-matched edge out of S lands in N(S) by construction, and
// the matched edge of a row in S leads to the column that brought that row in.
// So S witnesses the violation, and the pair (S, N(S)) is a certificate that
// an independent checker confirms in O(|S| * ncol) without redoing the
// matching. hall_witness() runs that check on its own output and reports the
// result in `verified`.
//
// Feasibility reads allowed(i, j) only. Costs never enter: whether an
// assignment of a given cardinality exists is a property of the admissibility
// graph and not of the objective, so at() is never called and a maximize flag
// would have no effect here.
//
// Every neighbourhood is walked through for_each_allowed(), so a source that
// names where its pairs are -- the restricted arc set a pricing loop solves
// over -- is matched at the cost of its edges, and a source that does not keeps
// the grid scan in the same column order.
#pragma once

#include "lap_neighbours.h"

#include <cstdint>
#include <limits>
#include <queue>
#include <vector>

namespace lap {

struct DeficiencySet {
    std::vector<int64_t> rows;   // 0-based, the deficient row set S
    std::vector<int64_t> cols;   // 0-based, N(S)
    int64_t max_cardinality = 0; // size of a maximum matching
    int64_t deficiency = 0;      // nrow - max_cardinality
    bool    row_perfect = false; // max_cardinality == nrow
    bool    verified = false;    // recomputed: |S| > |N(S)| and S's edges stay inside N(S)
    std::vector<int> matching;   // 0-based col per row, -1 unmatched; the max-cardinality matching
};

namespace hall_detail {

// Hopcroft-Karp maximum-cardinality matching, templated on the cost source and
// reading its neighbourhoods through allowed(i, j) as it goes. Materializing an
// adjacency list would cost nrow * ncol memory, which is the cost a lazy source
// exists to avoid, so each scan of a row's neighbours re-tests the predicate.
//
// The algorithm alternates a layered breadth-first search from the free rows
// with a depth-first search restricted to that layering, augmenting along a
// maximal set of vertex-disjoint shortest augmenting paths per phase. The
// shortest augmenting path length strictly increases from phase to phase and a
// matching within sqrt(V) of maximum admits a short augmenting path, which caps
// the phase count at O(sqrt(V)) and the whole run at O(E sqrt(V)).
template <class Source>
struct HopcroftKarp {
    static constexpr int64_t INF = std::numeric_limits<int64_t>::max();

    const Source& src;
    int64_t n;                       // rows, the left side
    int64_t m;                       // columns, the right side
    std::vector<int64_t> match_row;  // column matched to each row, -1 when free
    std::vector<int64_t> match_col;  // row matched to each column, -1 when free
    std::vector<int64_t> dist;       // layer per row, plus a sink slot at index n

    explicit HopcroftKarp(const Source& source)
        : src(source)
        , n(source.nrow)
        , m(source.ncol)
        , match_row(static_cast<size_t>(source.nrow), -1)
        , match_col(static_cast<size_t>(source.ncol), -1)
        , dist(static_cast<size_t>(source.nrow) + 1, INF) {}

    // Layered search from the free rows. dist[i] is the layer of row i, and the
    // sink slot dist[n] takes the layer at which a free column is first
    // reached, which is the length of a shortest augmenting path in this phase.
    // Rows at or beyond that layer are never expanded, so the search below only
    // ever walks shortest paths. Returns false when no augmenting path remains,
    // which is the maximality condition.
    bool bfs() {
        std::queue<int64_t> q;
        for (int64_t i = 0; i < n; ++i) {
            if (match_row[static_cast<size_t>(i)] < 0) {
                dist[static_cast<size_t>(i)] = 0;
                q.push(i);
            } else {
                dist[static_cast<size_t>(i)] = INF;
            }
        }
        dist[static_cast<size_t>(n)] = INF;

        while (!q.empty()) {
            const int64_t i = q.front();
            q.pop();
            if (dist[static_cast<size_t>(i)] >= dist[static_cast<size_t>(n)]) continue;
            const int64_t next_layer = dist[static_cast<size_t>(i)] + 1;

            for_each_allowed(src, i, [&](int64_t j) {
                const int64_t k = match_col[static_cast<size_t>(j)];
                if (k < 0) {
                    if (dist[static_cast<size_t>(n)] == INF) {
                        dist[static_cast<size_t>(n)] = next_layer;
                    }
                } else if (dist[static_cast<size_t>(k)] == INF) {
                    dist[static_cast<size_t>(k)] = next_layer;
                    q.push(k);
                }
                return true;
            });
        }

        return dist[static_cast<size_t>(n)] != INF;
    }

    // Depth-first search along the layering, flipping the path on success.
    // Retiring a failed row by setting its layer to INF keeps every row to at
    // most one expansion per phase, which is what bounds a phase to a single
    // scan of the edge set. Recursion depth is the layer count, itself the
    // shortest augmenting path length, so it stays O(sqrt(V)).
    bool dfs(int64_t i) {
        if (dist[static_cast<size_t>(i)] == INF) return false;
        const int64_t next_layer = dist[static_cast<size_t>(i)] + 1;

        bool augmented = false;
        for_each_allowed(src, i, [&](int64_t j) {
            const int64_t k = match_col[static_cast<size_t>(j)];
            const int64_t layer = (k < 0) ? dist[static_cast<size_t>(n)]
                                          : dist[static_cast<size_t>(k)];
            if (layer != next_layer) return true;
            if (k < 0 || dfs(k)) {
                match_row[static_cast<size_t>(i)] = j;
                match_col[static_cast<size_t>(j)] = i;
                augmented = true;
                return false;
            }
            return true;
        });
        if (augmented) return true;

        dist[static_cast<size_t>(i)] = INF;
        return false;
    }

    int64_t run() {
        int64_t matched = 0;
        while (bfs()) {
            for (int64_t i = 0; i < n; ++i) {
                if (match_row[static_cast<size_t>(i)] < 0 && dfs(i)) ++matched;
            }
        }
        return matched;
    }
};

// Vertices reachable by alternating paths from the unmatched rows: rows enter
// through matched edges, columns through non-matched ones. The matched edge of
// a row is skipped in the forward direction because an alternating path leaves
// a row along an unused edge.
template <class Source>
void koenig_reachable(const Source& src,
                      const std::vector<int64_t>& match_row,
                      const std::vector<int64_t>& match_col,
                      std::vector<char>& in_rows,
                      std::vector<char>& in_cols) {
    const int64_t n = src.nrow;
    const int64_t m = src.ncol;

    in_rows.assign(static_cast<size_t>(n), static_cast<char>(0));
    in_cols.assign(static_cast<size_t>(m), static_cast<char>(0));

    std::vector<int64_t> stack;
    for (int64_t i = 0; i < n; ++i) {
        if (match_row[static_cast<size_t>(i)] < 0) {
            in_rows[static_cast<size_t>(i)] = 1;
            stack.push_back(i);
        }
    }

    while (!stack.empty()) {
        const int64_t i = stack.back();
        stack.pop_back();
        const int64_t skip = match_row[static_cast<size_t>(i)];

        for_each_allowed(src, i, [&](int64_t j) {
            if (j == skip) return true;
            if (in_cols[static_cast<size_t>(j)]) return true;
            in_cols[static_cast<size_t>(j)] = 1;
            const int64_t k = match_col[static_cast<size_t>(j)];
            if (k >= 0 && !in_rows[static_cast<size_t>(k)]) {
                in_rows[static_cast<size_t>(k)] = 1;
                stack.push_back(k);
            }
            return true;
        });
    }
}

// Independent re-check of the certificate. Membership is rebuilt from the
// returned index vectors rather than from the marks the search left behind, so
// a construction that silently went wrong cannot certify itself.
template <class Source>
bool verify_witness(const Source& src,
                    const std::vector<int64_t>& rows,
                    const std::vector<int64_t>& cols) {
    const int64_t n = src.nrow;
    const int64_t m = src.ncol;

    if (static_cast<int64_t>(rows.size()) <= static_cast<int64_t>(cols.size())) return false;

    std::vector<char> is_neighbour(static_cast<size_t>(m), static_cast<char>(0));
    for (size_t t = 0; t < cols.size(); ++t) {
        const int64_t j = cols[t];
        if (j < 0 || j >= m) return false;
        if (is_neighbour[static_cast<size_t>(j)]) return false;  // duplicate inflates |N(S)|
        is_neighbour[static_cast<size_t>(j)] = 1;
    }

    bool inside = true;
    for (size_t t = 0; t < rows.size() && inside; ++t) {
        const int64_t i = rows[t];
        if (i < 0 || i >= n) return false;
        for_each_allowed(src, i, [&](int64_t j) {
            if (is_neighbour[static_cast<size_t>(j)]) return true;
            inside = false;
            return false;
        });
    }

    return inside;
}

}  // namespace hall_detail

// Maximum-cardinality matching plus, when it leaves rows unmatched, the Koenig
// deficient set and the columns it reaches.
template <class Source>
DeficiencySet hall_witness(const Source& src) {
    DeficiencySet out;
    const int64_t n = src.nrow;
    const int64_t m = src.ncol;

    hall_detail::HopcroftKarp<Source> hk(src);
    out.max_cardinality = hk.run();
    out.deficiency = n - out.max_cardinality;
    out.row_perfect = (out.max_cardinality == n);

    out.matching.assign(static_cast<size_t>(n), -1);
    for (int64_t i = 0; i < n; ++i) {
        const int64_t j = hk.match_row[static_cast<size_t>(i)];
        out.matching[static_cast<size_t>(i)] = (j >= 0) ? static_cast<int>(j) : -1;
    }

    if (out.row_perfect) {
        // Every row is matched, so no set of rows violates Hall's condition and
        // there is nothing to certify. `verified` is true vacuously: the two
        // conditions it reports on are conditions on a witness, and the empty
        // rows/cols carry no claim for a checker to refute.
        out.verified = true;
        return out;
    }

    std::vector<char> in_rows;
    std::vector<char> in_cols;
    hall_detail::koenig_reachable(src, hk.match_row, hk.match_col, in_rows, in_cols);

    for (int64_t i = 0; i < n; ++i) {
        if (in_rows[static_cast<size_t>(i)]) out.rows.push_back(i);
    }
    for (int64_t j = 0; j < m; ++j) {
        if (in_cols[static_cast<size_t>(j)]) out.cols.push_back(j);
    }

    out.verified = hall_detail::verify_witness(src, out.rows, out.cols);
    return out;
}

}  // namespace lap
