// src/flow/flow_oracle.h
// Type-erased view over a cost source, for the flow model only.
//
// The rest of the package templates on the cost-source concept: a type exposing
// at(int64_t, int64_t) const, allowed(int64_t, int64_t) const, and the public
// members nrow and ncol. lap::CostMatrix, lap::LazyCostMatrix and
// lap::PaddedCostView<Base> all satisfy it, and lap_certify.h reaches every one
// of them through a template.
//
// FlowProblem cannot. A flow problem holds a mixed set of arcs -- structural
// arcs with no cost source behind them, and one or more bipartite blocks that
// may each be backed by a different source type -- so the source type is not a
// property of the problem and cannot be a template parameter of it without
// pushing that parameter through every compiler, the solver, the certificate
// and the Rcpp layer.
//
// The cost of erasure is one virtual call per block arc, paid once, at
// expansion time. Nothing in the solver's inner loop touches the oracle:
// expand_blocks() reads every admissible (i, j) into the explicit arc array
// before the first augmentation, and the residual-graph search from then on
// sees only doubles. The templated path is still the one phase 3's pricer
// takes, which is why scan_reduced_costs() stays a template on the concrete
// type.
#pragma once

#include "../core/lap_cost_source.h"

#include <cstdint>

namespace lap {

class CostOracle {
public:
    virtual ~CostOracle() = default;

    virtual double  at(int64_t i, int64_t j) const = 0;
    virtual bool    allowed(int64_t i, int64_t j) const = 0;
    virtual int64_t nrow() const = 0;
    virtual int64_t ncol() const = 0;

    // Both questions for one virtual call, and for one evaluation where the
    // source behind the erasure can give both at once. Expansion reads every
    // pair of a block, so a source whose admissibility test is a distance --
    // LazyCostMatrix under a finite max_distance -- is the difference between
    // one distance a pair and three. The default is the two-call form the
    // concept guarantees; an oracle that can do better overrides it.
    virtual bool admissible(int64_t i, int64_t j, double& cost) const {
        if (!allowed(i, j)) return false;
        cost = at(i, j);
        return true;
    }
};

// Adapter for any type satisfying the cost-source concept. Holds a reference,
// so the source must outlive the oracle, which must in turn outlive the
// FlowProblem pointing at it.
template <class Source>
class SourceOracle final : public CostOracle {
public:
    explicit SourceOracle(const Source& src) : src_(src) {}

    double  at(int64_t i, int64_t j) const override { return src_.at(i, j); }
    bool    allowed(int64_t i, int64_t j) const override { return src_.allowed(i, j); }
    int64_t nrow() const override { return src_.nrow; }
    int64_t ncol() const override { return src_.ncol; }

    bool admissible(int64_t i, int64_t j, double& cost) const override {
        return cost_if_allowed(src_, i, j, cost);
    }

private:
    const Source& src_;
};

}  // namespace lap
