// src/core/lap_callback_source.h
// A cost source whose distance is a user's R function of two covariate matrices.
//
// The function's contract is the one compute_distance_matrix() already assumes
// in R/matching_distance.R: called on a block of left rows and a block of right
// rows, it returns their distances as a matrix. Calling it once per pair would
// pay an R call per pair; calling it on every row at once would build the matrix
// the lazy and implicit paths exist to avoid. So the source asks for a block of
// left rows against every right unit, sized to a cell budget, and keeps a few of
// those blocks. A solver reading row after row, which is how a pricing scan and
// a row search read, hits the block it asked for; a solver revisiting rows in
// another order refills blocks as it goes.
//
// A block is aligned to a multiple of its row count, so a row always belongs to
// the same block and reading it twice evaluates it once while that block is
// kept. The function runs on the R main thread, which every solver taking this
// source runs on.
//
// Everything the built-in lazy source applies after its distance applies here:
// per-variable calipers on the raw variables, the distance cut, the maximize
// negation. A value that is not a finite number is no pair, as NA and Inf are
// in a materialized matrix.
#pragma once

#include <Rcpp.h>

#include "lap_error.h"
#include "lap_lazy_types.h"
#include "lap_types.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <vector>

namespace lap {

class CallbackCostSource {
public:
    int64_t nrow = 0;
    int64_t ncol = 0;

    CallbackCostSource(Rcpp::Function fn, Rcpp::NumericMatrix left,
                       Rcpp::NumericMatrix right, double max_distance,
                       std::vector<CaliperSpec> calipers, bool negate,
                       int64_t block_cells = int64_t{1} << 20, int max_blocks = 8)
        : nrow(left.nrow()), ncol(right.nrow()), fn_(fn), left_(left), right_(right),
          max_distance_(max_distance), calipers_(std::move(calipers)), negate_(negate),
          max_blocks_(max_blocks > 0 ? max_blocks : 1) {
        if (left.ncol() != right.ncol()) {
            LAP_THROW_DIMENSION("distance function source: the left and right "
                                "covariates have different column counts");
        }
        const int64_t per = ncol > 0 ? block_cells / ncol : 1;
        rows_per_block_ = per > 0 ? per : 1;
        left_rows_ = std::make_shared<std::vector<double>>(
            static_cast<std::size_t>(left.nrow() * left.ncol()));
        right_rows_ = std::make_shared<std::vector<double>>(
            static_cast<std::size_t>(right.nrow() * right.ncol()));
        n_vars_ = left.ncol();
        for (int64_t i = 0; i < left.nrow(); ++i) {
            for (int64_t k = 0; k < n_vars_; ++k) {
                (*left_rows_)[static_cast<std::size_t>(i * n_vars_ + k)] = left(i, k);
            }
        }
        for (int64_t j = 0; j < right.nrow(); ++j) {
            for (int64_t k = 0; k < n_vars_; ++k) {
                (*right_rows_)[static_cast<std::size_t>(j * n_vars_ + k)] = right(j, k);
            }
        }
    }

    bool admissible(int64_t i, int64_t j, double& cost) const {
        if (!passes_calipers(i, j)) return false;
        const double d = value(i, j);
        if (!std::isfinite(d)) return false;
        if (std::isfinite(max_distance_) && d > max_distance_) return false;
        if (d < min_seen_) min_seen_ = d;
        cost = negate_ ? -d : d;
        return true;
    }

    double at(int64_t i, int64_t j) const {
        double c = 0.0;
        return admissible(i, j, c) ? c : BIG;
    }

    bool allowed(int64_t i, int64_t j) const {
        double c = 0.0;
        return admissible(i, j, c);
    }

    bool empty() const { return nrow == 0 || ncol == 0; }
    bool is_negated() const { return negate_; }
    double max_distance() const { return max_distance_; }
    void set_max_distance(double d) { max_distance_ = d; }

    // How many times the function was called, and the smallest admissible
    // distance it has returned: a design that needs non-negative distances reads
    // the second, which covers every pair a solver evaluated.
    int64_t n_calls() const { return n_calls_; }
    double min_distance_seen() const { return min_seen_; }

private:
    struct Block {
        int64_t first = -1;
        int64_t rows = 0;
        std::vector<double> values;   // rows * ncol, row-major
    };

    bool passes_calipers(int64_t i, int64_t j) const {
        return passes_all(calipers_, i, j);
    }

    double value(int64_t i, int64_t j) const {
        const int64_t first = (i / rows_per_block_) * rows_per_block_;
        for (std::size_t t = 0; t < blocks_.size(); ++t) {
            if (blocks_[t].first == first) {
                if (t != 0) std::rotate(blocks_.begin(), blocks_.begin() + static_cast<std::ptrdiff_t>(t),
                                        blocks_.begin() + static_cast<std::ptrdiff_t>(t) + 1);
                return blocks_[0].values[static_cast<std::size_t>((i - first) * ncol + j)];
            }
        }
        load(first);
        return blocks_[0].values[static_cast<std::size_t>((i - first) * ncol + j)];
    }

    void load(int64_t first) const {
        const int64_t rows = std::min<int64_t>(rows_per_block_, nrow - first);
        Rcpp::NumericMatrix sub(static_cast<int>(rows), static_cast<int>(n_vars_));
        for (int64_t r = 0; r < rows; ++r) {
            for (int64_t k = 0; k < n_vars_; ++k) {
                sub(static_cast<int>(r), static_cast<int>(k)) =
                    (*left_rows_)[static_cast<std::size_t>((first + r) * n_vars_ + k)];
            }
        }
        Rcpp::NumericMatrix out;
        {
            SEXP res = fn_(sub, right_);
            ++n_calls_;
            if (!Rf_isMatrix(res) || !Rf_isNumeric(res)) {
                LAP_THROW_DIMENSION("the distance function returned something other "
                                    "than a numeric matrix");
            }
            out = Rcpp::NumericMatrix(res);
        }
        if (out.nrow() != rows || out.ncol() != ncol) {
            LAP_THROW_DIMENSION("the distance function returned a " +
                                std::to_string(out.nrow()) + " by " +
                                std::to_string(out.ncol()) + " matrix for " +
                                std::to_string(rows) + " left and " +
                                std::to_string(ncol) + " right units");
        }
        Block b;
        b.first = first;
        b.rows = rows;
        b.values.resize(static_cast<std::size_t>(rows * ncol));
        for (int64_t r = 0; r < rows; ++r) {
            for (int64_t c = 0; c < ncol; ++c) {
                b.values[static_cast<std::size_t>(r * ncol + c)] =
                    out(static_cast<int>(r), static_cast<int>(c));
            }
        }
        blocks_.insert(blocks_.begin(), std::move(b));
        if (static_cast<int>(blocks_.size()) > max_blocks_) blocks_.pop_back();
    }

    Rcpp::Function fn_;
    Rcpp::NumericMatrix left_;
    Rcpp::NumericMatrix right_;
    std::shared_ptr<std::vector<double>> left_rows_;
    std::shared_ptr<std::vector<double>> right_rows_;
    int64_t n_vars_ = 0;
    double max_distance_ = std::numeric_limits<double>::infinity();
    std::vector<CaliperSpec> calipers_;
    bool negate_ = false;
    int max_blocks_ = 8;
    int64_t rows_per_block_ = 1;

    mutable std::vector<Block> blocks_;
    mutable int64_t n_calls_ = 0;
    mutable double min_seen_ = std::numeric_limits<double>::infinity();
};

// The smallest distance a source has handed a solver. A built-in metric is
// non-negative by construction; a user's function is whatever it returned.
inline double min_distance_seen(const LazyCostMatrix&) { return 0.0; }
inline double min_distance_seen(const CallbackCostSource& src) {
    return src.min_distance_seen();
}

}  // namespace lap
