// src/core/lap_lazy_types.h
// Lazy (on-demand) cost source - NO Rcpp dependencies
//
// Computes C(i,j) on demand from the underlying feature rows instead of
// materializing an n_left x n_right dense matrix. Trades compute for RAM:
// a 100k x 100k dense CostMatrix needs ~80GB, but the underlying 100k x 100
// feature matrices need ~80MB. Exposes the same at()/allowed()/empty()
// interface as CostMatrix, and bakes in "prepared" semantics (forbidden ->
// BIG, negated if maximize) at construction time, matching what
// prepare_for_solve() does for a dense CostMatrix -- so solver bodies that
// only call .at()/.allowed() work unmodified when templated on this type.
#pragma once

#include "lap_types.h"
#include <vector>
#include <cstdint>
#include <cmath>
#include <algorithm>

namespace lap {

enum class DistanceMetric {
    Euclidean,
    Manhattan,
    SquaredEuclidean,
    Chebyshev,
    Mahalanobis
};

// Per-variable caliper: pairs with |left[i] - right[j]| exceeding threshold
// are forbidden, independent of the chosen distance metric. The values are
// the variable as the caller supplied it, held apart from the coordinates the
// distance is computed in: those are scaled and weighted, while a caliper is
// stated on the raw variable, as R's apply_calipers() checks it on the dense
// path.
struct CaliperSpec {
    double threshold;
    std::vector<double> left;
    std::vector<double> right;

    bool passes(int64_t i, int64_t j) const {
        return !(std::abs(left[static_cast<std::size_t>(i)] -
                          right[static_cast<std::size_t>(j)]) > threshold);
    }

    CaliperSpec swapped() const { return CaliperSpec{threshold, right, left}; }
};

inline bool passes_all(const std::vector<CaliperSpec>& calipers,
                       int64_t i, int64_t j) {
    for (const CaliperSpec& cal : calipers) {
        if (!cal.passes(i, j)) return false;
    }
    return true;
}

inline std::vector<CaliperSpec> swapped_all(const std::vector<CaliperSpec>& calipers) {
    std::vector<CaliperSpec> out;
    out.reserve(calipers.size());
    for (const CaliperSpec& cal : calipers) out.push_back(cal.swapped());
    return out;
}

class LazyCostMatrix {
public:
    int64_t nrow = 0;  // public, matching CostMatrix's convention (solvers
    int64_t ncol = 0;  // read work.nrow/work.ncol directly)

    // left_rowmajor: nrow * n_vars, row-major (row i = feature vector for left unit i)
    // right_rowmajor: ncol * n_vars, row-major
    // inv_cov: n_vars * n_vars, row-major; only read when metric == Mahalanobis
    // max_distance: Inf when unconstrained
    LazyCostMatrix(std::vector<double> left_rowmajor,
                   std::vector<double> right_rowmajor,
                   int64_t n_vars,
                   DistanceMetric metric,
                   std::vector<double> inv_cov,
                   double max_distance,
                   std::vector<CaliperSpec> calipers,
                   bool negate)
        : nrow(static_cast<int64_t>(left_rowmajor.size()) / (n_vars > 0 ? n_vars : 1))
        , ncol(static_cast<int64_t>(right_rowmajor.size()) / (n_vars > 0 ? n_vars : 1))
        , left_(std::move(left_rowmajor))
        , right_(std::move(right_rowmajor))
        , n_vars_(n_vars)
        , metric_(metric)
        , inv_cov_(std::move(inv_cov))
        , max_distance_(max_distance)
        , calipers_(std::move(calipers))
        , negate_(negate) {}

    // Whether the pair is admissible, and its cost when it is, for one
    // evaluation of the distance. Under a finite max_distance the
    // admissibility test *is* a distance, so a caller that asks allowed() and
    // then at() pays for three evaluations of it and this pays for one.
    // Templated callers reach it through cost_if_allowed().
    bool admissible(int64_t i, int64_t j, double& cost) const {
        if (!passes_calipers(i, j)) return false;
        const double d = raw_distance(i, j);
        // Inf max_distance admits every finite d, and the comparison is the
        // same one either way.
        if (std::isfinite(max_distance_) && d > max_distance_) return false;
        cost = negate_ ? -d : d;
        return true;
    }

    // Composed from the same two helpers as admissible() rather than called
    // through it: returning the cost directly, with no out-parameter and no
    // bool to test, is 12% faster on an unconstrained source, and at() alone is
    // what the auction's bidding loop and the JV augmentation read through.
    double at(int64_t i, int64_t j) const {
        if (!passes_calipers(i, j)) return BIG;
        const double d = raw_distance(i, j);
        if (std::isfinite(max_distance_) && d > max_distance_) return BIG;
        return negate_ ? -d : d;
    }

    // Kept separate from admissible() rather than written in terms of it,
    // because an unconstrained source answers this from the calipers alone and
    // a caller that only asks admissibility -- hall_witness(), build_allowed()
    // -- should not be charged a distance it never reads.
    bool allowed(int64_t i, int64_t j) const {
        if (!passes_calipers(i, j)) return false;
        if (std::isfinite(max_distance_)) {
            if (raw_distance(i, j) > max_distance_) return false;
        }
        return true;
    }

    bool empty() const { return nrow == 0 || ncol == 0; }

    // Whether at() reports negated distances (maximize mode). Callers that
    // need the true, unnegated distance for a chosen (allowed) pair -- e.g.
    // to report a "total cost" using original costs, matching the dense
    // CostMatrix convention of reporting from the pre-negation matrix --
    // can undo the sign via this flag rather than needing a second,
    // un-negated copy of the source.
    bool is_negated() const { return negate_; }

    // The geometry behind the distance, for a caller that prunes pairs rather
    // than evaluating them. A tree over the columns bounds a subtree from the
    // coordinates, the metric and the covariance, and reading them from here
    // is what keeps the distance itself written once, in raw_distance().
    int64_t n_vars() const { return n_vars_; }
    DistanceMetric metric() const { return metric_; }
    double max_distance() const { return max_distance_; }
    const std::vector<CaliperSpec>& calipers() const { return calipers_; }
    const std::vector<double>& inv_cov() const { return inv_cov_; }

    // Move the distance cut without moving the coordinates. A path over caliper
    // values states one problem per value, and the values differ in this field
    // alone: the geometry, the metric and the calipers are the same, so a tree
    // built over the columns and a flow placed over the pairs both stay valid
    // across a move. Widening admits pairs and leaves every cost already
    // reported unchanged; narrowing withdraws pairs a flow may be standing on,
    // and the caller that sweeps is the one that knows which it is doing.
    void set_max_distance(double d) { max_distance_ = d; }

    // The same problem with its rows and columns exchanged. Every distance the
    // source measures is a function of the difference of two points, and
    // negating that difference changes none of them, calipers included, so
    // transposed().at(j, i) is at(i, j) to the bit.
    LazyCostMatrix transposed() const {
        return LazyCostMatrix(right_, left_, n_vars_, metric_, inv_cov_, max_distance_,
                              swapped_all(calipers_), negate_);
    }

    const double* left_row(int64_t i) const {
        return &left_[static_cast<size_t>(i * n_vars_)];
    }
    const double* right_row(int64_t j) const {
        return &right_[static_cast<size_t>(j * n_vars_)];
    }

private:
    bool passes_calipers(int64_t i, int64_t j) const {
        return passes_all(calipers_, i, j);
    }

    double raw_distance(int64_t i, int64_t j) const {
        const double* li = &left_[static_cast<size_t>(i * n_vars_)];
        const double* rj = &right_[static_cast<size_t>(j * n_vars_)];

        switch (metric_) {
            case DistanceMetric::Euclidean: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    double d = li[k] - rj[k];
                    s += d * d;
                }
                return std::sqrt(s);
            }
            case DistanceMetric::Manhattan: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    s += std::abs(li[k] - rj[k]);
                }
                return s;
            }
            case DistanceMetric::SquaredEuclidean: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    double d = li[k] - rj[k];
                    s += d * d;
                }
                return s;
            }
            case DistanceMetric::Chebyshev: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    s = std::max(s, std::abs(li[k] - rj[k]));
                }
                return s;
            }
            case DistanceMetric::Mahalanobis: {
                // diff' * inv_cov * diff, inv_cov row-major n_vars x n_vars
                double s = 0.0;
                for (int64_t a = 0; a < n_vars_; ++a) {
                    double diff_a = li[a] - rj[a];
                    if (diff_a == 0.0) continue;
                    double row_sum = 0.0;
                    const double* inv_row = &inv_cov_[static_cast<size_t>(a * n_vars_)];
                    for (int64_t b = 0; b < n_vars_; ++b) {
                        row_sum += inv_row[b] * (li[b] - rj[b]);
                    }
                    s += diff_a * row_sum;
                }
                return std::sqrt(std::max(s, 0.0));
            }
        }
        return 0.0;  // unreachable
    }

    std::vector<double> left_;
    std::vector<double> right_;
    int64_t n_vars_;
    DistanceMetric metric_;
    std::vector<double> inv_cov_;
    double max_distance_;
    std::vector<CaliperSpec> calipers_;
    bool negate_;
};

}  // namespace lap
