// utils_gabow_tarjan.h
// Header for Gabow-Tarjan utilities

#ifndef UTILS_GABOW_TARJAN_H
#define UTILS_GABOW_TARJAN_H

#include <vector>

// ============================================================================
// Constants and Type Definitions
// ============================================================================

constexpr int NIL = -1;
constexpr long long BIG_INT = 1000000000000000LL; // 1e15

using CostMatrix = std::vector<std::vector<long long>>;
using MatchVec   = std::vector<int>;
using DualVec    = std::vector<long long>;

// ============================================================================
// Row capacity
// ============================================================================
//
// Every routine below takes a trailing `last_row_cap`: the number of columns
// the last row of the instance may hold at once. One is an ordinary row and
// the default, so a square or plainly rectangular instance passes nothing.
//
// A wide instance needs the dummy side of the padded square, and its m - n
// dummy rows are copies of one node: same zero cost to every column, nothing
// to tell them apart. The solver therefore carries them as a single row of
// capacity m - n, which keeps the instance n + 1 rows tall instead of m.
//
// `col_match` is the matching: col_match[j] is the row holding column j, or
// NIL. `row_match` mirrors it for the ordinary rows, which hold one column
// each; a capacitated row holds several, so col_match alone speaks for it.

// Index of the capacitated row, or NIL when every row holds one column.
inline int capacitated_row(int n_rows, int last_row_cap) {
    return (last_row_cap > 1 && n_rows > 0) ? (n_rows - 1) : NIL;
}

// Columns the row may hold.
inline int row_capacity(int i, int n_rows, int last_row_cap) {
    return (i == capacitated_row(n_rows, last_row_cap)) ? last_row_cap : 1;
}

// Columns each row currently holds, read off the matching.
std::vector<int> row_degrees(const MatchVec& col_match, int n_rows);

// ============================================================================
// Module A: Cost-length & 1-feasibility utilities
// ============================================================================

long long cost_length(long long c_ij, bool in_matching);

bool is_eligible(long long c_ij, bool in_matching,
                 long long yu, long long yv);

bool check_one_feasible(const CostMatrix& cost,
                        const MatchVec& col_match,
                        const DualVec& y_u,
                        const DualVec& y_v);

// ============================================================================
// Module B: Equality graph construction
// ============================================================================

std::vector<std::vector<int>>
build_equality_graph(const CostMatrix& cost,
                     const MatchVec& col_match,
                     const DualVec& y_u,
                     const DualVec& y_v);

void update_equality_graph_incremental(std::vector<std::vector<int>>& eq_graph,
                                        const CostMatrix& cost,
                                        const MatchVec& col_match,
                                        const DualVec& y_u,
                                        const DualVec& y_v,
                                        const std::vector<int>& affected_rows,
                                        const std::vector<int>& affected_cols);

// ============================================================================
// Module C: Augment matching along a path
// ============================================================================

void augment_along_path(const std::vector<std::pair<int,int>>& edges,
                        MatchVec& row_match,
                        MatchVec& col_match,
                        int last_row_cap = 1);

// ============================================================================
// Module D: Maximal set of augmenting paths on equality graph
// ============================================================================

std::vector<std::vector<std::pair<int,int>>>
find_maximal_augmenting_paths(const std::vector<std::vector<int>>& eq_graph,
                              const MatchVec& col_match,
                              int last_row_cap = 1);

// ============================================================================
// Module E: Hungarian-style search on cost-length (Step 2 core)
// ============================================================================

bool hungarian_search_cl(const CostMatrix& cost,
                        MatchVec& row_match,
                        MatchVec& col_match,
                        DualVec& y_u,
                        DualVec& y_v,
                        std::vector<int>* affected_rows_out = nullptr,
                        std::vector<int>* affected_cols_out = nullptr,
                        int last_row_cap = 1);

bool hungarian_step_one_feasible(const CostMatrix& cost,
                                 MatchVec& row_match,
                                 MatchVec& col_match,
                                 DualVec& y_u,
                                 DualVec& y_v,
                                 std::vector<int>* affected_rows_out = nullptr,
                                 std::vector<int>* affected_cols_out = nullptr,
                                 int last_row_cap = 1);

// ============================================================================
// Module F: match_gt - Inner Gabow-Tarjan matching algorithm
// ============================================================================

// A saturated instance: every row holds as many columns as its capacity allows.
bool is_perfect(const MatchVec& col_match, int n_rows, int last_row_cap = 1);

// New signature that supports incremental updates
bool apply_step1(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v,
                std::vector<std::vector<int>>* eq_graph = nullptr,
                std::vector<int>* affected_cols_out = nullptr,
                int last_row_cap = 1);

void match_gt(const CostMatrix& cost,
             MatchVec& row_match,
             MatchVec& col_match,
             DualVec& y_u,
             DualVec& y_v,
             int max_iters = 1000,
             bool check_feasible = false,
             int last_row_cap = 1);

// ============================================================================
// Module G: scale_match - Wrapper for bit-scaling outer loop
// ============================================================================

void scale_match(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v,
                bool enable_6n_prune = false,
                int last_row_cap = 1);

// ============================================================================
// Module H: Gabow-Tarjan bit-scaling outer loop
// ============================================================================

long long find_max_cost(const CostMatrix& cost);

void solve_gabow_tarjan_inner(const CostMatrix& cost,
                              MatchVec& row_match,
                              MatchVec& col_match,
                              DualVec& y_u,
                              DualVec& y_v);

#endif // UTILS_GABOW_TARJAN_H
