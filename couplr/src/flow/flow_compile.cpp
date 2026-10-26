// src/flow/flow_compile.cpp
// The design compilers. Every one of them allocates the same node layout,
// attaches one bipartite block per stratum, and then chooses three things: what
// the rows may carry, what a column may carry, and how much flow the problem
// asks for. Nothing else separates the designs, so nothing else is written more
// than once here.

#include "flow_compile.h"

#include "../core/lap_error.h"

#include <algorithm>
#include <limits>
#include <utility>

namespace lap {

namespace {

// Node ids are int32, so a cost source wider than that cannot be laid out.
constexpr int64_t FLOW_MAX_NODES =
    static_cast<int64_t>(std::numeric_limits<int32_t>::max());

// Product of two non-negative counts, clamped at FLOW_INF_CAP. Used where the
// result bounds a capacity, so saturating is the right answer: a bound above
// every reachable flow value bounds nothing.
int64_t capped_mul(int64_t a, int64_t b) {
    if (a <= 0 || b <= 0) return 0;
    if (a > FLOW_INF_CAP / b) return FLOW_INF_CAP;
    return a * b;
}

// Product of two non-negative counts, exact. Used for flow values and supplies,
// where a clamped result would break the supplies-sum-to-zero invariant instead
// of merely being loose.
int64_t exact_mul(int64_t a, int64_t b) {
    if (a <= 0 || b <= 0) return 0;
    if (a > std::numeric_limits<int64_t>::max() / b) {
        LAP_THROW_DIMENSION("flow value exceeds the representable range");
    }
    return a * b;
}

int32_t node_count(int64_t n) {
    if (n < 0 || n > FLOW_MAX_NODES) {
        LAP_THROW_DIMENSION("cost source dimension does not fit a node index");
    }
    return static_cast<int32_t>(n);
}

std::vector<int32_t> identity_map(int32_t n) {
    std::vector<int32_t> map(static_cast<std::size_t>(n));
    for (int32_t i = 0; i < n; ++i) map[static_cast<std::size_t>(i)] = i;
    return map;
}

void reject_categories(const std::vector<CategoryConstraint>& categories) {
    if (!categories.empty()) {
        LAP_THROW("category constraints are not compiled; pass an empty vector");
    }
}

// Node allocation and the three arc sets. Rows are allocated before columns and
// columns before anything else, which is the layout flow_problem.h fixes; the
// builder refuses any other order rather than producing a problem whose blocks
// happen to sit somewhere a dual mapping does not expect.
class Builder {
public:
    Builder() : supply_(2, 0) {}

    int32_t add_rows(int32_t n) {
        if (columns_open_) {
            LAP_THROW_DIMENSION("row nodes must be allocated before column nodes");
        }
        return allocate(n);
    }

    int32_t add_cols(int32_t n) {
        columns_open_ = true;
        return allocate(n);
    }

    void add_source_arcs(int32_t base, int32_t n, int64_t lower, int64_t upper) {
        for (int32_t i = 0; i < n; ++i) {
            arcs_.emplace_back(FLOW_SOURCE, base + i, lower, upper, 0.0);
        }
    }

    void add_sink_arcs(int32_t base, int32_t n, int64_t lower, int64_t upper) {
        for (int32_t j = 0; j < n; ++j) {
            arcs_.emplace_back(base + j, FLOW_SINK, lower, upper, 0.0);
        }
    }

    void add_block(int32_t row_base, int32_t col_base, const CostOracle& costs,
                   int64_t lower, int64_t upper) {
        BipartiteBlock blk;
        blk.row_base = row_base;
        blk.col_base = col_base;
        blk.lower    = lower;
        blk.upper    = upper;
        blk.costs    = &costs;
        blocks_.push_back(blk);
    }

    // The source's own outlet to the sink, for a design whose flow value is not
    // fixed in advance. The injected total is a constant the network must
    // absorb, and a design that chooses how many pair arcs to use sends the
    // remainder straight down this arc at no cost, so the value the bipartite
    // part carries is free while the problem stays a fixed-value flow.
    void add_bypass_arc(int64_t lower, int64_t upper) {
        arcs_.emplace_back(FLOW_SOURCE, FLOW_SINK, lower, upper, 0.0);
    }

    // Flow the auxiliary source injects, absorbed at the auxiliary sink.
    void inject_at_source(int64_t flow) {
        supply_[static_cast<std::size_t>(FLOW_SOURCE)] += flow;
        supply_[static_cast<std::size_t>(FLOW_SINK)]   -= flow;
    }

    // Flow each row node injects itself, absorbed at the auxiliary sink. A
    // design whose rows carry a fixed amount states it here and leaves the
    // source out of the network entirely.
    void inject_at_rows(int32_t base, int32_t n, int64_t per_row) {
        for (int32_t i = 0; i < n; ++i) {
            supply_[static_cast<std::size_t>(base + i)] += per_row;
        }
        supply_[static_cast<std::size_t>(FLOW_SINK)] -= exact_mul(n, per_row);
    }

    FlowProblem finish() {
        FlowProblem prob;
        prob.n_nodes = static_cast<int32_t>(supply_.size());
        prob.supply  = std::move(supply_);
        prob.arcs    = std::move(arcs_);
        prob.blocks  = std::move(blocks_);
        return prob;
    }

private:
    int32_t allocate(int32_t n) {
        if (n < 0) LAP_THROW_DIMENSION("node count must not be negative");
        const int64_t base = static_cast<int64_t>(supply_.size());
        if (base + n > FLOW_MAX_NODES) {
            LAP_THROW_DIMENSION("problem needs more nodes than a node index holds");
        }
        supply_.resize(static_cast<std::size_t>(base + n), 0);
        return static_cast<int32_t>(base);
    }

    std::vector<int64_t>        supply_;
    std::vector<FlowArc>        arcs_;
    std::vector<BipartiteBlock> blocks_;
    bool                        columns_open_ = false;
};

// One bipartite layer: row nodes, column nodes, and the block of unit-capacity
// pair arcs between them. Each design finishes it by saying where the flow
// comes from and what a column may carry.
struct Skeleton {
    Builder b;
    int32_t row_base = FLOW_FIRST_ROW;
    int32_t n_rows   = 0;
    int32_t col_base = FLOW_FIRST_ROW;
    int32_t n_cols   = 0;
};

Skeleton open_bipartite(const CostOracle& costs) {
    Skeleton s;
    s.n_rows   = node_count(costs.nrow());
    s.n_cols   = node_count(costs.ncol());
    s.row_base = s.b.add_rows(s.n_rows);
    s.col_base = s.b.add_cols(s.n_cols);
    s.b.add_block(s.row_base, s.col_base, costs, 0, 1);
    return s;
}

CompiledDesign close_bipartite(Skeleton& s, int64_t flow_required) {
    CompiledDesign out;
    out.problem       = s.b.finish();
    out.row_base      = s.row_base;
    out.n_rows        = s.n_rows;
    out.col_base      = s.col_base;
    out.n_cols        = s.n_cols;
    out.row_unit      = identity_map(s.n_rows);
    out.col_unit      = identity_map(s.n_cols);
    out.flow_required = flow_required;
    return out;
}

}  // namespace

CompiledDesign compile_one_to_one(const CostOracle&                      costs,
                                  const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);

    Skeleton s = open_bipartite(costs);
    s.b.inject_at_rows(s.row_base, s.n_rows, 1);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    return close_bipartite(s, s.n_rows);
}

CompiledDesign compile_fixed_ratio(const CostOracle&                      costs,
                                   int64_t                                ratio,
                                   const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (ratio < 1) LAP_THROW_DIMENSION("ratio must be at least 1");

    // The k replicas of a row sit on consecutive nodes, so replica e belongs to
    // row e / ratio and the design is the assignment R already solves.
    auto replicated = std::unique_ptr<CostOracle>(new RowReplicatedOracle(costs, ratio));

    Skeleton s = open_bipartite(*replicated);
    s.b.inject_at_rows(s.row_base, s.n_rows, 1);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);

    CompiledDesign out = close_bipartite(s, s.n_rows);
    for (int32_t e = 0; e < out.n_rows; ++e) {
        out.row_unit[static_cast<std::size_t>(e)] = static_cast<int32_t>(e / ratio);
    }
    out.owned.push_back(std::move(replicated));
    return out;
}

CompiledDesign compile_with_replacement(const CostOracle&                      costs,
                                        int64_t                                ratio,
                                        const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (ratio < 1) LAP_THROW_DIMENSION("ratio must be at least 1");

    Skeleton s = open_bipartite(costs);

    // A row cannot take more distinct columns than exist, and columns are
    // shared, so the column capacity is the number of rows: the most any column
    // could be asked to carry.
    const int64_t per_row = std::min<int64_t>(ratio, s.n_cols);
    s.b.inject_at_rows(s.row_base, s.n_rows, per_row);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, s.n_rows);
    return close_bipartite(s, exact_mul(s.n_rows, per_row));
}

CompiledDesign compile_variable_ratio(const CostOracle&                      costs,
                                      int64_t                                r_min,
                                      int64_t                                r_max,
                                      const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (r_min < 0)     LAP_THROW_DIMENSION("r_min must not be negative");
    if (r_max < r_min) LAP_THROW_DIMENSION("r_max must be at least r_min");

    Skeleton s = open_bipartite(costs);
    if (r_min > s.n_cols) {
        LAP_THROW_DIMENSION("r_min exceeds the number of columns, so no row bound is expressible");
    }

    // As many pairs as the bounds allow, at minimum cost among matchings of
    // that size: every column is used when the rows can absorb them all, every
    // row reaches r_max when they cannot.
    const int64_t cap   = std::min<int64_t>(r_max, s.n_cols);
    const int64_t total = std::min<int64_t>(capped_mul(s.n_rows, cap), s.n_cols);

    s.b.add_source_arcs(s.row_base, s.n_rows, r_min, cap);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    s.b.inject_at_source(total);
    return close_bipartite(s, total);
}

CompiledDesign compile_k_cardinality(const CostOracle&                      costs,
                                     int64_t                                k,
                                     const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (k < 0) LAP_THROW_DIMENSION("k must not be negative");

    Skeleton s = open_bipartite(costs);
    if (k > std::min<int64_t>(s.n_rows, s.n_cols)) {
        LAP_THROW_DIMENSION("k exceeds the number of pairs the problem can hold");
    }

    s.b.add_source_arcs(s.row_base, s.n_rows, 0, 1);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    s.b.inject_at_source(k);
    return close_bipartite(s, k);
}

// Full matching with both group shapes admissible. A full matching is a set of
// disjoint stars covering every unit, so the arcs it uses are an edge cover of
// the admissible pairs: every unit meets at least one chosen arc. Conversely an
// inclusion-minimal edge cover has no path of three arcs, so each of its
// components is a star, and with non-negative distances a cheapest cover is
// minimal. Minimising the total distance over edge covers is therefore exactly
// minimising it over full matchings.
//
// As a flow that is a lower bound of one on both sides, a unit capacity on each
// pair arc, and no fixed value: the number of arcs a cover uses depends on how
// many groups it forms, so the source injects a constant that bounds it and the
// bypass arc absorbs whatever the network does not need. A minimal cover uses
// at most n_left + n_right - 1 arcs, so that total is always enough.
//
// max_controls bounds how many units a group holds on its many side, whichever
// side that is, which is the same reading as the one-to-many design for a group
// of one left unit and several right ones.
void compile_full_matching_symmetric(const CostOracle& costs,
                                     int64_t           max_controls,
                                     CompiledFullMatch& out) {
    const int64_t n_left  = costs.nrow();
    const int64_t n_right = costs.ncol();

    const int64_t cap = std::min<int64_t>(max_controls,
                                          std::max<int64_t>(n_left, n_right));
    out.n_centres    = n_left;
    out.n_units      = n_right;
    out.max_capacity = cap;

    // Every unit meets at least one chosen arc and no unit meets more than cap
    // of them, so one side's capacity has to reach the other side's count.
    if (capped_mul(cap, n_left) < n_right || capped_mul(cap, n_right) < n_left) {
        out.bounds_feasible = false;
        out.reason = "max_controls is too small to cover the larger side";
        return;
    }

    Skeleton s = open_bipartite(costs);
    s.b.add_source_arcs(s.row_base, s.n_rows, 1, cap);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 1, cap);
    const int64_t total = n_left + n_right;
    s.b.add_bypass_arc(0, total);
    s.b.inject_at_source(total);

    out.design = close_bipartite(s, total);
}

CompiledFullMatch compile_full_matching(const CostOracle&                      costs,
                                        int64_t                                min_controls,
                                        int64_t                                max_controls,
                                        const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (min_controls < 1) LAP_THROW_DIMENSION("min_controls must be at least 1");

    CompiledFullMatch out;
    out.min_controls = min_controls;

    const int64_t n_left  = costs.nrow();
    const int64_t n_right = costs.ncol();
    if (n_left == 0 || n_right == 0) {
        out.bounds_feasible = false;
        out.reason = "one side has no units";
        return out;
    }

    // min_controls is the least number of right units a group may hold, and a
    // many-to-one group holds exactly one. So min_controls above one forbids
    // that shape outright, and every group is one left unit with several right
    // ones: the centres are the smaller side and the design below is the whole
    // feasible set. At min_controls == 1 both shapes are admissible, which is
    // full matching in the sense of Hansen and Klopfer, and fixing the centres
    // to one side would drop solutions that can be arbitrarily cheaper.
    out.symmetric = (min_controls == 1);
    if (out.symmetric) {
        compile_full_matching_symmetric(costs, max_controls, out);
        return out;
    }

    // min_controls and max_controls count right units, so the centres are the
    // left side whichever side holds more. Making the smaller side the centres
    // turns the bounds into bounds on left units instead: on six left units,
    // two right ones and min_controls = 2 that returns two groups holding one
    // right unit each and reports them optimal. An orientation that cannot
    // meet the bound is refused below rather than answered in the other one.
    out.n_centres = n_left;
    out.n_units   = n_right;

    // A centre cannot absorb more units than exist, so the requested upper
    // bound is clamped before it is compared with the lower one.
    out.max_capacity = std::min<int64_t>(max_controls, out.n_units);
    if (out.max_capacity < min_controls) {
        out.bounds_feasible = false;
        out.reason = "max_controls falls below min_controls once clamped to the unit count";
        return out;
    }
    if (out.n_units < capped_mul(out.n_centres, min_controls)) {
        out.bounds_feasible = false;
        out.reason = "fewer units than min_controls for every centre";
        return out;
    }

    // Every unit is placed, which is what makes this full matching rather than
    // a subset selection, so the flow the source injects is the unit count.
    Skeleton s = open_bipartite(costs);
    s.b.add_source_arcs(s.row_base, s.n_rows, min_controls, out.max_capacity);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    s.b.inject_at_source(out.n_units);

    out.design = close_bipartite(s, out.n_units);
    return out;
}

CompiledBlocked compile_blocked(const std::vector<Stratum>&            strata,
                                const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);

    CompiledBlocked out;
    out.strata.resize(strata.size());

    Builder b;

    for (std::size_t s = 0; s < strata.size(); ++s) {
        if (strata[s].costs == nullptr) {
            LAP_THROW_DIMENSION("stratum has no cost source");
        }
        const int32_t nr = node_count(strata[s].costs->nrow());
        if (strata[s].row_unit.size() != static_cast<std::size_t>(nr)) {
            LAP_THROW_DIMENSION("stratum row_unit length does not match its cost source");
        }
        out.strata[s].n_rows   = nr;
        out.strata[s].row_base = b.add_rows(nr);
    }

    for (std::size_t s = 0; s < strata.size(); ++s) {
        const int32_t nc = node_count(strata[s].costs->ncol());
        if (strata[s].col_unit.size() != static_cast<std::size_t>(nc)) {
            LAP_THROW_DIMENSION("stratum col_unit length does not match its cost source");
        }
        out.strata[s].n_cols   = nc;
        out.strata[s].col_base = b.add_cols(nc);
    }

    int64_t              flow_required = 0;
    std::vector<int32_t> row_unit;
    std::vector<int32_t> col_unit;

    for (std::size_t s = 0; s < strata.size(); ++s) {
        StratumRange& range = out.strata[s];
        range.block = static_cast<int64_t>(s);
        b.add_block(range.row_base, range.col_base, *strata[s].costs, 0, 1);
        b.inject_at_rows(range.row_base, range.n_rows, 1);
        b.add_sink_arcs(range.col_base, range.n_cols, 0, 1);
        flow_required += range.n_rows;
        row_unit.insert(row_unit.end(), strata[s].row_unit.begin(), strata[s].row_unit.end());
        col_unit.insert(col_unit.end(), strata[s].col_unit.begin(), strata[s].col_unit.end());
    }

    out.design.problem       = b.finish();
    out.design.row_base      = FLOW_FIRST_ROW;
    out.design.n_rows        = static_cast<int32_t>(row_unit.size());
    out.design.col_base      = FLOW_FIRST_ROW + out.design.n_rows;
    out.design.n_cols        = static_cast<int32_t>(col_unit.size());
    out.design.row_unit      = std::move(row_unit);
    out.design.col_unit      = std::move(col_unit);
    out.design.flow_required = flow_required;
    return out;
}

double ShapeOracle::at(int64_t, int64_t) const {
    LAP_THROW("a shape carries no costs; compile against the cost source to read one");
}

bool ShapeOracle::allowed(int64_t, int64_t) const {
    LAP_THROW("a shape carries no costs; compile against the cost source to read one");
}

namespace {

// The layout both routing predicates read: one block of pair arcs a row uses at
// most once, sitting where the node convention puts it, with nothing above the
// column block. A category or stratum node there would carry a constraint no
// cost matrix expresses and no per-row choice respects.
//
// After expansion the block arcs are in `arcs` and the arc scans below can no
// longer separate a column's sink arc from a pair arc. Routing is decided
// before the arc list is paid for, which is the point of it.
const BipartiteBlock* single_pair_block(const FlowProblem& prob) {
    if (prob.expanded) return nullptr;
    if (prob.blocks.size() != 1) return nullptr;

    const BipartiteBlock& blk = prob.blocks.front();
    if (blk.costs == nullptr) return nullptr;
    if (blk.lower != 0 || blk.upper != 1) return nullptr;

    const int64_t nr = blk.costs->nrow();
    const int64_t nc = blk.costs->ncol();
    if (nr < 0 || nc < 0) return nullptr;
    if (blk.row_base != FLOW_FIRST_ROW) return nullptr;
    if (static_cast<int64_t>(blk.col_base) != FLOW_FIRST_ROW + nr) return nullptr;
    if (static_cast<int64_t>(prob.n_nodes) != FLOW_FIRST_ROW + nr + nc) return nullptr;
    if (static_cast<int64_t>(prob.supply.size()) != prob.n_nodes) return nullptr;
    return &blk;
}

// Total flow the rows inject, or -1 when something other than the rows meters
// it: flow held at the source, supply on a column, a sink absorbing a different
// amount, or a row carrying other than `per_row` when a fixed amount is asked
// for. Pass -1 for `per_row` to accept any fixed non-negative amount.
int64_t rows_carry_the_flow(const FlowProblem& prob, const BipartiteBlock& blk,
                            int64_t nr, int64_t nc, int64_t per_row) {
    if (prob.supply[static_cast<std::size_t>(FLOW_SOURCE)] != 0) return -1;

    int64_t total = 0;
    for (int64_t i = 0; i < nr; ++i) {
        const int64_t s = prob.supply[static_cast<std::size_t>(blk.row_base) +
                                      static_cast<std::size_t>(i)];
        if (s < 0) return -1;
        if (per_row >= 0 && s != per_row) return -1;
        total += s;
    }
    if (prob.supply[static_cast<std::size_t>(FLOW_SINK)] != -total) return -1;
    for (int64_t j = 0; j < nc; ++j) {
        if (prob.supply[static_cast<std::size_t>(blk.col_base) +
                        static_cast<std::size_t>(j)] != 0) return -1;
    }
    return total;
}

// Exactly one free arc per column and no other explicit arc, each admitting
// between `min_upper` and `max_upper` units. What a column admits is the whole
// difference between the two designs, and the pair arcs' own capacity does not
// state it.
bool columns_admit(const FlowProblem& prob, const BipartiteBlock& blk, int64_t nc,
                   int64_t min_upper, int64_t max_upper) {
    if (static_cast<int64_t>(prob.arcs.size()) != nc) return false;

    std::vector<bool> reaches_sink(static_cast<std::size_t>(nc), false);
    for (const FlowArc& arc : prob.arcs) {
        if (arc.head != FLOW_SINK) return false;
        const int64_t j = static_cast<int64_t>(arc.tail) - static_cast<int64_t>(blk.col_base);
        if (j < 0 || j >= nc) return false;
        if (reaches_sink[static_cast<std::size_t>(j)]) return false;
        reaches_sink[static_cast<std::size_t>(j)] = true;
        if (arc.lower != 0) return false;
        if (arc.upper < min_upper || arc.upper > max_upper) return false;
        if (arc.cost != 0.0) return false;
    }
    return true;
}

}  // namespace

bool is_unit_capacity_assignment(const FlowProblem& prob) {
    const BipartiteBlock* blk = single_pair_block(prob);
    if (blk == nullptr) return false;

    const int64_t nr = blk->costs->nrow();
    const int64_t nc = blk->costs->ncol();

    // Supply 1 on every row is what says every row must be matched, and an
    // empty source is what says nothing else meters the flow.
    if (rows_carry_the_flow(prob, *blk, nr, nc, 1) != nr) return false;

    // Unit capacity on the column arcs is what forbids a column being reused.
    return columns_admit(prob, *blk, nc, 1, 1);
}

bool is_row_separable(const FlowProblem& prob) {
    const BipartiteBlock* blk = single_pair_block(prob);
    if (blk == nullptr) return false;

    const int64_t nr = blk->costs->nrow();
    const int64_t nc = blk->costs->ncol();

    if (rows_carry_the_flow(prob, *blk, nr, nc, -1) < 0) return false;

    // A row sends a column at most one unit, so a column admitting nr of them
    // admits every row at once and no set of rows can exhaust it.
    return columns_admit(prob, *blk, nc, nr, FLOW_INF_CAP);
}

LoweredAssignment lower_to_assignment(const FlowProblem& prob) {
    LoweredAssignment out;
    if (!is_unit_capacity_assignment(prob)) return out;

    const BipartiteBlock& blk = prob.blocks.front();
    out.valid    = true;
    out.costs    = blk.costs;
    out.row_base = blk.row_base;
    out.col_base = blk.col_base;
    out.n_rows   = node_count(blk.costs->nrow());
    out.n_cols   = node_count(blk.costs->ncol());
    out.row_unit = identity_map(out.n_rows);
    out.col_unit = identity_map(out.n_cols);
    return out;
}

LoweredAssignment lower_to_assignment(const CompiledDesign& design) {
    LoweredAssignment out = lower_to_assignment(design.problem);
    if (!out.valid) return out;

    if (design.row_unit.size() != static_cast<std::size_t>(out.n_rows) ||
        design.col_unit.size() != static_cast<std::size_t>(out.n_cols)) {
        LAP_THROW_DIMENSION("compiled design index maps do not match its block");
    }
    out.row_unit = design.row_unit;
    out.col_unit = design.col_unit;
    return out;
}

}  // namespace lap
