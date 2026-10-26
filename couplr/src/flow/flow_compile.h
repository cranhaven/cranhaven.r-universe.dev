// src/flow/flow_compile.h
// One compiler per matching design: the design goes in as a cost source plus
// its parameters, a FlowProblem comes out together with the index metadata a
// caller needs to read a solved flow back out in its own terms.
//
// Every design is the same three-layer network and differs only in the bounds
// on its three arc sets:
//
//     source --[row bounds]--> row --[0..1 per pair]--> column --[column
//     capacity]--> sink
//
// so the compilers share one builder and each one is the bounds it chooses:
//
//   design           row throughput            column capacity   flow value
//   ---------------------------------------------------------------------------
//   1:1              supply 1 per row node     1                 n_rows
//   k:1 fixed ratio  supply 1 per row node,    1                 k * n_rows
//                    rows replicated k times
//   replacement      supply k per row node     n_rows            k * n_rows
//   variable ratio   source arc [r_min,r_max]  1                 min(n_rows *
//                                                                r_max, n_cols)
//   k-cardinality    source arc [0, 1]         1                 k
//   full matching    source arc [c_min,c_max]  1                 n_units
//   exact / blocked  supply 1 per row node,    1                 n_rows
//                    one block per stratum
//
// A design whose row throughput is a range, or whose flow value is smaller than
// what the rows could carry, needs the auxiliary source to hold the slack; a
// design whose rows each carry a fixed amount puts that amount on the row node
// and leaves the source unused. The distinction is what the lowering predicate
// at the bottom of this file reads.
//
// Node layout is flow_problem.h's, unchanged:
//
//     0                        auxiliary source
//     1                        auxiliary sink
//     2 .. 2+nr-1              row nodes
//     2+nr .. 2+nr+nc-1        column nodes
//     2+nr+nc ..               category nodes (section F), stratum nodes
//
// Lower bounds are emitted as lower bounds. The transformation that turns them
// into supplies and auxiliary arcs belongs to the solver, so a compiler here
// states what the design means and never what a residual graph needs.
#pragma once

#include "flow_oracle.h"
#include "flow_problem.h"

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace lap {

// Fine balance and cardinality balance (section F): the listed units send their
// flow through a shared node admitting between `lower` and `upper` of them,
// instead of reaching the sink directly. The node ids above the column block
// are reserved for those shared nodes.
//
// Declared so the constraint has a place to arrive and the compiler signatures
// do not change when it does. No compiler emits one: a non-empty vector is an
// error, because a constraint silently dropped is a wrong answer that validates
// and solves.
struct CategoryConstraint {
    enum class Side { Row, Column };

    Side                 side  = Side::Column;
    std::vector<int32_t> members;   // indices into that side's unit list
    int64_t              lower = 0;
    int64_t              upper = FLOW_INF_CAP;
};

// Row-replicated view of a cost source: row node e reads the base source's row
// e / times. This is the k:1 design's expansion, and it is the same expansion
// R/matching_core.R:383-384 writes as cost[rep(seq_len(n), each = k), ], down to
// the order of the replicas, so a compiled k:1 problem lowers to the matrix the
// package already solves.
class RowReplicatedOracle final : public CostOracle {
public:
    RowReplicatedOracle(const CostOracle& base, int64_t times)
        : base_(base), times_(times) {}

    double  at(int64_t i, int64_t j) const override { return base_.at(i / times_, j); }
    bool    allowed(int64_t i, int64_t j) const override { return base_.allowed(i / times_, j); }
    int64_t nrow() const override { return base_.nrow() * times_; }
    int64_t ncol() const override { return base_.ncol(); }

    bool admissible(int64_t i, int64_t j, double& cost) const override {
        return base_.admissible(i / times_, j, cost);
    }

private:
    const CostOracle& base_;
    int64_t           times_;
};

// A design's shape, with no costs behind it. Which network a design compiles
// to, and which of the caller's units each node stands for, are decided by the
// row and column counts alone, so a caller that only needs the routing decision
// and the index maps can state the shape and keep its prices where they are.
//
// Reading a cell throws. A problem built on this source can be lowered or
// tested for separability; expanding one would ask for prices it does not hold.
class ShapeOracle final : public CostOracle {
public:
    ShapeOracle(int64_t nrow, int64_t ncol) : nrow_(nrow), ncol_(ncol) {}

    double  at(int64_t i, int64_t j) const override;
    bool    allowed(int64_t i, int64_t j) const override;
    int64_t nrow() const override { return nrow_; }
    int64_t ncol() const override { return ncol_; }

private:
    int64_t nrow_;
    int64_t ncol_;
};

// A compiled design: the problem, where its row and column blocks sit, and
// which of the caller's units each node stands for.
//
// row_unit and col_unit are indexed by node offset, not by node id: row node
// row_base + r belongs to the caller's row row_unit[r]. They are the identity
// for every design that does not reshape its input; k:1 replication puts the
// same unit on `ratio` consecutive nodes, a transposed full matching puts right
// units on row nodes, and blocking concatenates the strata.
struct CompiledDesign {
    FlowProblem problem;

    int32_t row_base = FLOW_FIRST_ROW;
    int32_t n_rows   = 0;
    int32_t col_base = FLOW_FIRST_ROW;
    int32_t n_cols   = 0;

    std::vector<int32_t> row_unit;
    std::vector<int32_t> col_unit;

    // Flow value a complete solve places. A solve that falls short of it is the
    // design's "partial", and one that places nothing is its "infeasible".
    int64_t flow_required = 0;

    // Cost sources the problem's blocks point at and nobody else owns. Moving a
    // CompiledDesign keeps every pointee address, so the blocks stay valid.
    std::vector<std::unique_ptr<CostOracle>> owned;
};

// One stratum of an exact / blocked design: its own cost source and the
// caller's indices for its rows and columns.
struct Stratum {
    const CostOracle*    costs = nullptr;   // not owned
    std::vector<int32_t> row_unit;
    std::vector<int32_t> col_unit;
};

// Where one stratum landed. The networks share only the auxiliary source and
// sink, and no arc crosses a stratum boundary, so a solve over the compiled
// problem is a solve of each stratum independently.
struct StratumRange {
    int32_t row_base   = 0;
    int32_t n_rows     = 0;
    int32_t col_base   = 0;
    int32_t n_cols     = 0;
    int64_t block      = 0;   // index into problem.blocks
};

struct CompiledBlocked {
    CompiledDesign            design;
    std::vector<StratumRange> strata;
};

// A compiled full matching, with the orientation and the effective bounds the
// network was built on.
//
// bounds_feasible is false when the requested bounds cannot be met by any
// assignment of these units, which is decided from the counts alone and before
// any arc exists: an empty side, an upper bound below the lower one once
// clamped to the unit count, or fewer units than the centres' lower bounds
// require. The problem is then left empty and `reason` says which. Everything
// else is a solver status.
//
// A solve that places every unit is a complete full matching. A solve that
// falls short leaves centres holding fewer than min_controls units, and a
// centre below its bound holds no group: the reading step drops it and its
// units, which is what separates a partial full matching from an infeasible
// one -- some group survived, or none did.
struct CompiledFullMatch {
    CompiledDesign design;

    bool        bounds_feasible = true;
    std::string reason;

    // True when both group shapes are admissible, which is min_controls == 1.
    // The network is then an edge cover over the pairs rather than one centre
    // per group, and the reading step recovers the groups as the components of
    // the arcs the solve placed.
    bool    symmetric    = false;
    int64_t n_centres    = 0;
    int64_t n_units      = 0;
    int64_t min_controls = 0;
    int64_t max_capacity = 0;   // max_controls clamped to n_units
};

// Unit-capacity bipartite assignment: every row is matched, every column at
// most once. The LP is lap_certify.h's, so a compiled 1:1 problem is the
// package's existing assignment problem written as a flow.
CompiledDesign compile_one_to_one(const CostOracle&                      costs,
                                  const std::vector<CategoryConstraint>& categories);

// k:1 fixed ratio: every row takes exactly `ratio` distinct columns, every
// column at most one row. Compiled as the replicated assignment R already
// solves, which is what lets it lower.
CompiledDesign compile_fixed_ratio(const CostOracle&                      costs,
                                   int64_t                                ratio,
                                   const std::vector<CategoryConstraint>& categories);

// Matching with replacement: every row takes min(ratio, n_cols) distinct
// columns and columns are shared freely, so the rows do not compete and the
// optimum is each row's own cheapest choices. Column capacity is n_rows, the
// most any column could be asked to carry.
CompiledDesign compile_with_replacement(const CostOracle&                      costs,
                                        int64_t                                ratio,
                                        const std::vector<CategoryConstraint>& categories);

// Variable ratio: every row takes between r_min and r_max distinct columns,
// every column at most one row, and as many pairs are formed as the bounds
// allow -- min(n_rows * r_max, n_cols) -- at minimum cost among matchings of
// that size. Pass FLOW_INF_CAP for an unbounded r_max.
CompiledDesign compile_variable_ratio(const CostOracle&                      costs,
                                      int64_t                                r_min,
                                      int64_t                                r_max,
                                      const std::vector<CategoryConstraint>& categories);

// k-cardinality subset: exactly k pairs, each row and each column in at most
// one, at minimum cost. The source carries k and every row arc is unit
// capacity, so k is the only thing bounding the number of pairs.
CompiledDesign compile_k_cardinality(const CostOracle&                      costs,
                                     int64_t                                k,
                                     const std::vector<CategoryConstraint>& categories);

// Full matching: every unit joins a group built around a centre absorbing
// between min_controls and max_controls units, at minimum total distance. The
// centres are the left units, and their lower bound is emitted as an arc lower
// bound for the solver to transform.
CompiledFullMatch compile_full_matching(const CostOracle&                      costs,
                                        int64_t                                min_controls,
                                        int64_t                                max_controls,
                                        const std::vector<CategoryConstraint>& categories);

// Exact / blocked matching: one unit-capacity bipartite network per stratum,
// laid out in one problem. Rows are the strata's rows concatenated in the order
// given, then columns likewise, so every stratum owns a contiguous node range
// and no two overlap.
CompiledBlocked compile_blocked(const std::vector<Stratum>&            strata,
                                const std::vector<CategoryConstraint>& categories);

// The block's cost source and the maps from node offset to the caller's units.
// This is everything R/lap_solve.R's assignment() needs: it takes the matrix
// the source describes, and its answer is read back through the maps.
struct LoweredAssignment {
    bool              valid = false;
    const CostOracle* costs = nullptr;   // not owned; the block's source

    int32_t row_base = FLOW_FIRST_ROW;
    int32_t n_rows   = 0;
    int32_t col_base = FLOW_FIRST_ROW;
    int32_t n_cols   = 0;

    std::vector<int32_t> row_unit;
    std::vector<int32_t> col_unit;
};

// True when the problem is the assignment problem R/lap_solve.R already solves:
// one block of unit-capacity pair arcs, supply 1 on every row node, a single
// unit-capacity arc from every column to the sink, nothing on the auxiliary
// source, and no node above the column block.
//
// All five conditions are load-bearing and each one excludes a design that
// would otherwise be solved as something it is not. Unit capacity on the pair
// arcs alone does not make an assignment: matching with replacement has unit
// pair arcs and column capacity n_rows, and solving it as an assignment would
// impose a column disjointness R/matching_core.R:313-339 does not impose.
//
// False on an expanded problem. Lowering is the routing decision taken before
// expansion, and taking it after has already paid for the arc list it exists to
// avoid.
bool is_unit_capacity_assignment(const FlowProblem& prob);

// True when no column can be made to compete: one block of pair arcs a row uses
// at most once, every row carrying a fixed amount of its own, and a column
// admitting a unit from every row. The columns then bind nothing, so the
// cheapest way to place a row's flow is that row's own cheapest columns and the
// problem decomposes into one independent choice per row.
//
// This is matching with replacement, and the column capacity is what separates
// it from an assignment: the same pair arcs under a unit column capacity are a
// problem in which the rows compete for columns and no per-row choice solves.
//
// False on an expanded problem, for the reason lowering is: routing is decided
// before the arc list is paid for.
bool is_row_separable(const FlowProblem& prob);

// The lowered form, or a LoweredAssignment with valid = false when the problem
// is not one. The FlowProblem overload knows only the block's own row and
// column order, so its maps are the identity; the CompiledDesign overload
// carries the design's maps, which is where k:1 replication lives.
LoweredAssignment lower_to_assignment(const FlowProblem& prob);
LoweredAssignment lower_to_assignment(const CompiledDesign& design);

}  // namespace lap
