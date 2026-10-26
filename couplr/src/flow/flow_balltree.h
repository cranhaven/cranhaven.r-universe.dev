// src/flow/flow_balltree.h
// A ball tree over the control units, and the bounds that let a pricer skip a
// subtree instead of reading it.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// price_block() reads every omitted pair, and the seed reads every pair again.
// Both questions are "which columns of row i are cheap", and a structure over
// the columns answers them by bounding a whole subtree at once: if the
// cheapest pair the subtree could hold still prices at or above zero, none of
// its columns is a violator and none of them is read.
//
// The bound needs two structures per node, not one, because the two things
// that make a pair inadmissible live in different coordinates:
//
//   - a whitened centre and radius, for the distance. Mahalanobis is Euclidean
//     after whitening by the Cholesky factor of the inverse covariance, so one
//     ball serves both metrics and the factor is computed once here. The
//     source holds inv_cov itself rather than a factor.
//   - an axis-aligned box in the *original* covariates, for the distance's
//     per-variable reach, and a range over each caliper's raw values, for the
//     caliper. Whitening is a rotation and a scaling, so it destroys the axis
//     alignment a per-variable caliper is stated in, and the covariates are
//     themselves scaled and weighted before they reach the source, so the
//     caliper is bounded on the values it is stated on.
//
// The tree holds only what the source cannot answer for itself: the leaf
// ordering, the two structures, the whitening factor, and the per-node largest
// column dual. Calipers, max_distance and the maximize flag stay on the source
// and are read from it at query time.
//
// Manhattan and Chebyshev get no tree. A whitened ball bounds neither, and
// build_ball_tree() returns an empty tree for them rather than a wrong one,
// which routes those metrics to price_block().
#pragma once

#include "../core/lap_error.h"
#include "../core/lap_lazy_types.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <vector>

namespace lap {

// Whether a ball in whitened coordinates bounds the metric at all.
inline bool metric_has_ball_bound(DistanceMetric metric) {
    switch (metric) {
        case DistanceMetric::Euclidean:
        case DistanceMetric::SquaredEuclidean:
        case DistanceMetric::Mahalanobis:
            return true;
        case DistanceMetric::Manhattan:
        case DistanceMetric::Chebyshev:
            return false;
    }
    return false;
}

// The distance the source reports for a pair whose whitened separation is `d`,
// before the maximize flag is applied. Euclidean and Mahalanobis are that
// separation; SquaredEuclidean is its square. Non-decreasing in d either way,
// which is what makes a bound on d a bound on the cost.
inline double metric_cost_of(DistanceMetric metric, double d) {
    return metric == DistanceMetric::SquaredEuclidean ? d * d : d;
}

// Lower-triangular L with A = L L', A the symmetric part of `a`. False when A
// is not positive definite, which is the source saying its Mahalanobis
// distance has no whitening and no ball can bound it.
inline bool cholesky_lower(const std::vector<double>& a, int64_t n,
                           std::vector<double>& l) {
    l.assign(static_cast<std::size_t>(n * n), 0.0);
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j <= i; ++j) {
            // raw_distance() reads diff' A diff, which is diff' ((A + A')/2)
            // diff for any A at all, so the symmetric part is the matrix being
            // factored and an inv_cov off by a rounding error in its symmetry
            // is not a different problem.
            double s = 0.5 * (a[static_cast<std::size_t>(i * n + j)] +
                              a[static_cast<std::size_t>(j * n + i)]);
            for (int64_t k = 0; k < j; ++k) {
                s -= l[static_cast<std::size_t>(i * n + k)] *
                     l[static_cast<std::size_t>(j * n + k)];
            }
            if (i == j) {
                if (!(s > 0.0)) return false;
                l[static_cast<std::size_t>(i * n + i)] = std::sqrt(s);
            } else {
                l[static_cast<std::size_t>(i * n + j)] =
                    s / l[static_cast<std::size_t>(j * n + j)];
            }
        }
    }
    return true;
}

struct BallTree {
    int64_t n_vars = 0;
    int32_t n_units = 0;
    int32_t leaf_size = 16;

    // The control units in leaf order. Node `id` owns perm[lo[id] .. hi[id]).
    std::vector<int32_t> perm;

    std::vector<int32_t> lo, hi, left, right;

    std::vector<double> centre;          // n_nodes * n_vars, whitened
    std::vector<double> radius;          // n_nodes
    std::vector<double> box_lo, box_hi;  // n_nodes * n_vars, original
    // The range of each caliper's raw values over the node's columns,
    // n_nodes * n_cal. A caliper is stated on its variable as supplied, which
    // the coordinates above hold scaled and weighted.
    int64_t n_cal = 0;
    std::vector<double> cal_lo, cal_hi;
    std::vector<double> max_v;           // n_nodes, refreshed per pricing round

    // The point every coordinate is measured from, in the original covariates.
    // Whitening maps x to L'(x - origin) rather than L'x, so a translation
    // shared by the whole sample cancels before the factor is applied and the
    // separations the tree works in stay the size of the sample's own spread.
    std::vector<double> origin;

    // The control coordinates after whitening, n_units * n_vars, indexed by
    // unit rather than by leaf position so that a permutation of `perm` costs
    // nothing to follow.
    std::vector<double> whitened;

    // L' of the Cholesky factor, n_vars * n_vars row-major and upper
    // triangular. Empty when whitening is the identity.
    std::vector<double> factor;

    // The allowance on the bound's own arithmetic. Every bound the tree reports
    // is widened by it before anyone reads it, so a prune fires only where the
    // bound clears its threshold by more than the bound can be wrong.
    // Descending where it does not clear costs a leaf evaluation, which is the
    // cost source's own answer, so an allowance set too high loses time and an
    // allowance set too low would lose an edge.
    //
    // It has a relative part and an absolute part, and it needs both. The
    // centre distance and the radius are each a sum of n_vars squares and a
    // square root, whose relative error is bounded by gamma_{n_vars + 3} in the
    // standard sense, with gamma_k = k*eps/(1 - k*eps). Under Mahalanobis the
    // tree bounds ||L' d|| while the source evaluates d' A d for the same A the
    // factor came from; in exact arithmetic the two differ by the residual of
    // the factorization, whose effect on the ratio is bounded by
    // ||L L' - A||_F * ||A^-1||_2, since d'(L L' - A)d over d' A d is at most
    // ||L L' - A||_2 / lambda_min(A). Measuring that residual against ||A||_F
    // would understate it by the conditioning of A. That residual is the
    // algebraic gap only, and it is frequently zero because L L' reconstructs
    // the stored A exactly; what the source's own evaluation rounds by is
    // charged separately, by src_quad_coef below.
    double bound_rel = 0.0;

    // Whitening a point is an absolute-error operation: the coordinates are
    // formed before the difference the distance is taken over, so their
    // rounding does not shrink as two points approach each other and no
    // relative allowance can cover it. The error in one whitened coordinate of
    // x is bounded by gamma_{n_vars + 2} * sum_m |L'_km| |x_m - origin_m|, so
    // the displacement's norm is bounded by bound_abs_coef * g(x), with g the
    // value whiten_point() returns. g_max is the largest g over the controls,
    // which stands for the node geometry built from them.
    double bound_abs_coef = 0.0;
    double g_max = 0.0;

    // What the source's own evaluation can be wrong by. bound_rel covers the
    // tree's arithmetic and the algebraic gap between ||L' d||^2 and d' A d,
    // but a prune is read against the number raw_distance() returns, and that
    // number carries its own rounding. Under Mahalanobis the source sums
    // d_a * sum_b A_ab d_b, whose terms cancel when d runs along a direction A
    // is small in, so its error is bounded relative to |d|' |A| |d| rather
    // than to d' A d and no relative allowance on the distance can hold it.
    // The error is at most gamma_{2 n_vars + 2} * |d|' |A| |d|, counted off the
    // loop the source runs rather than off the tree's: the inner row product
    // recomputes its own differences, so each of its terms carries one
    // subtraction and the length n_vars accumulation,
    // (1 + delta)(1 + theta_n) = (1 + theta_{n+1}); the outer sum multiplies by
    // d_a, itself one subtraction, and accumulates over n_vars terms again, so
    // a term of the double sum carries
    // (1 + delta)(1 + theta_{n+1})(1 + theta_n) = (1 + theta_{2n+2}).
    //
    // Over a node, |d_k| is at most the query's reach r_k to the node's box
    // along variable k, and |d|' |A| |d| <= r' |A| r <= rho(|A|) ||r||^2, since
    // |A| is non-negative and symmetric. rho(|A|) is at most the largest row
    // sum of |A|, which does not depend on the query, so the bound a node visit
    // pays for is src_quad_coef * ||r||^2, one pass over the variables.
    // src_quad_coef carries gamma_{2n+2} and that row sum, both widened by
    // (1 + gamma_{2n+6}) for the rounding in the row sum (n + 1 operations),
    // in ||r||^2 (n + 2), in the product with it (1) and in forming the
    // coefficient itself (2). Zero for the metrics the source sums non-negative
    // terms for, where no cancellation arises.
    double src_quad_coef = 0.0;

    bool empty() const { return lo.empty(); }
    int32_t n_nodes() const { return static_cast<int32_t>(lo.size()); }
    bool is_leaf(int32_t id) const {
        return left[static_cast<std::size_t>(id)] < 0;
    }

    const double* node_centre(int32_t id) const {
        return &centre[static_cast<std::size_t>(id) * static_cast<std::size_t>(n_vars)];
    }
    const double* node_box_lo(int32_t id) const {
        return &box_lo[static_cast<std::size_t>(id) * static_cast<std::size_t>(n_vars)];
    }
    const double* node_box_hi(int32_t id) const {
        return &box_hi[static_cast<std::size_t>(id) * static_cast<std::size_t>(n_vars)];
    }
    const double* unit_whitened(int32_t unit) const {
        return &whitened[static_cast<std::size_t>(unit) * static_cast<std::size_t>(n_vars)];
    }
};

namespace detail {

// Split perm[a, b) recursively, recording the node structure and nothing else.
// A node is appended before its children, so the geometry pass that follows
// can fill a parent from its children in one reverse sweep.
inline int32_t ball_split(BallTree& tree, int32_t a, int32_t b) {
    const int32_t id = static_cast<int32_t>(tree.lo.size());
    tree.lo.push_back(a);
    tree.hi.push_back(b);
    tree.left.push_back(-1);
    tree.right.push_back(-1);

    if (b - a <= tree.leaf_size) return id;

    const int64_t n_vars = tree.n_vars;
    const double* w = tree.whitened.data();
    const int32_t* p = tree.perm.data();

    int64_t split_dim = 0;
    double best = -1.0;
    for (int64_t k = 0; k < n_vars; ++k) {
        double mn = std::numeric_limits<double>::infinity();
        double mx = -std::numeric_limits<double>::infinity();
        for (int32_t t = a; t < b; ++t) {
            const double x = w[static_cast<std::size_t>(p[t]) *
                                   static_cast<std::size_t>(n_vars) +
                               static_cast<std::size_t>(k)];
            if (x < mn) mn = x;
            if (x > mx) mx = x;
        }
        if (mx - mn > best) {
            best = mx - mn;
            split_dim = k;
        }
    }
    // Every member sits at the same whitened point, so there is no dimension
    // to cut on and the node stays a leaf however many units it holds. Its
    // radius is zero, which makes it the tightest bound in the tree anyway.
    if (!(best > 0.0)) return id;

    const int32_t mid = a + (b - a) / 2;
    std::nth_element(tree.perm.begin() + a, tree.perm.begin() + mid,
                     tree.perm.begin() + b,
                     [w, n_vars, split_dim](int32_t lhs, int32_t rhs) {
                         return w[static_cast<std::size_t>(lhs) *
                                      static_cast<std::size_t>(n_vars) +
                                  static_cast<std::size_t>(split_dim)] <
                                w[static_cast<std::size_t>(rhs) *
                                      static_cast<std::size_t>(n_vars) +
                                  static_cast<std::size_t>(split_dim)];
                     });

    const int32_t l = ball_split(tree, a, mid);
    const int32_t r = ball_split(tree, mid, b);
    tree.left[static_cast<std::size_t>(id)] = l;
    tree.right[static_cast<std::size_t>(id)] = r;
    return id;
}

}  // namespace detail

// Write the whitened image of one original-coordinate point into `out`, which
// must hold n_vars doubles, and return the magnitude g(x) the rounding of those
// coordinates is bounded against. This is the query side of the same map the
// tree's centres were built under, origin included.
inline double whiten_point(const BallTree& tree, const double* x, double* out) {
    const int64_t n = tree.n_vars;
    const double* o = tree.origin.data();
    if (tree.factor.empty()) {
        double g = 0.0;
        for (int64_t k = 0; k < n; ++k) {
            const double d = x[k] - o[k];
            out[k] = d;
            g += d * d;
        }
        return std::sqrt(g);
    }
    double g = 0.0;
    for (int64_t k = 0; k < n; ++k) {
        const double* row = &tree.factor[static_cast<std::size_t>(k * n)];
        double s = 0.0;
        double t = 0.0;
        for (int64_t m = k; m < n; ++m) {
            const double d = x[m] - o[m];
            s += row[m] * d;
            t += std::fabs(row[m]) * std::fabs(d);
        }
        out[k] = s;
        g += t * t;
    }
    return std::sqrt(g);
}

namespace detail {

// The relative allowance the tree lowers every bound by.
//
// The geometric part is gamma_{n_vars + 3}: a sum of n_vars squares, the
// square root over it, and the centre and radius that were built the same way.
//
// The Mahalanobis part is the factorization residual. The tree measures
// ||L' d||^2 where the source measures d' A d, and the difference is d' E d
// with E = L L' - A, L the factor as stored. Over every direction that ratio is
// bounded by ||E||_2 / lambda_min(A), which is at most ||E||_F * ||L^-1||_F^2
// because A^-1 = L^-T L^-1. Taking that ratio on the squared quantity and
// applying it to the distance is conservative, since a relative error r on a
// square is a relative error below r on its root for r below one.
//
// Both factors are themselves computed, and each is bounded from above rather
// than estimated. An entry of E is formed by an inner product and a
// symmetrization, whose rounding is added to the entry's computed magnitude.
// ||L^-1||_F is read off X, the computed inverse, through the residual
// R = I - L X: L^-1 = X (I - R)^-1, so ||L^-1||_F <= ||X||_F / (1 - ||R||_F)
// whenever ||R||_F < 1. A factor whose inverse cannot be verified that way
// gets an infinite allowance, which is no tree.
inline double bound_allowance(const LazyCostMatrix& src,
                              const std::vector<double>& factor,
                              int64_t n_vars) {
    constexpr double kInf = std::numeric_limits<double>::infinity();
    double rel = gamma_of(n_vars + 3);
    if (src.metric() != DistanceMetric::Mahalanobis || factor.empty()) {
        return rel;
    }
    const auto at = [&factor, n_vars](int64_t row, int64_t col) {
        return factor[static_cast<std::size_t>(row * n_vars + col)];
    };

    const std::vector<double>& a = src.inv_cov();
    const double g_dot = gamma_of(n_vars + 1);
    const double g_sym = gamma_of(2);
    double num = 0.0;
    for (int64_t i = 0; i < n_vars; ++i) {
        for (int64_t j = 0; j < n_vars; ++j) {
            // factor holds L' upper triangular, so (L L')_ij is the inner
            // product of columns i and j of L', over the rows both reach.
            double lij = 0.0;
            double lij_abs = 0.0;
            const int64_t stop = i < j ? i : j;
            for (int64_t k = 0; k <= stop; ++k) {
                lij += at(k, i) * at(k, j);
                lij_abs += std::fabs(at(k, i)) * std::fabs(at(k, j));
            }
            const double aij = 0.5 * (a[static_cast<std::size_t>(i * n_vars + j)] +
                                      a[static_cast<std::size_t>(j * n_vars + i)]);
            double d = next_up(std::fabs(lij - aij));
            d = next_up(d + next_up(g_dot * next_up(lij_abs)));
            d = next_up(d + next_up(g_sym * std::fabs(aij)));
            num = next_up(num + next_up(d * d));
        }
    }

    // X = L^-1 by forward substitution. L is the lower factor, held transposed
    // in `factor`, so L_ik = at(k, i).
    std::vector<double> inv(static_cast<std::size_t>(n_vars * n_vars), 0.0);
    for (int64_t j = 0; j < n_vars; ++j) {
        for (int64_t i = j; i < n_vars; ++i) {
            const double dii = at(i, i);
            if (!(std::fabs(dii) > 0.0)) return kInf;
            if (i == j) {
                inv[static_cast<std::size_t>(i * n_vars + j)] = 1.0 / dii;
                continue;
            }
            double s = 0.0;
            for (int64_t k = j; k < i; ++k) {
                s += at(k, i) * inv[static_cast<std::size_t>(k * n_vars + j)];
            }
            inv[static_cast<std::size_t>(i * n_vars + j)] = -s / dii;
        }
    }

    double r_sq = 0.0;
    double x_sq = 0.0;
    for (int64_t i = 0; i < n_vars; ++i) {
        for (int64_t j = 0; j < n_vars; ++j) {
            const double xij = inv[static_cast<std::size_t>(i * n_vars + j)];
            x_sq = next_up(x_sq + next_up(xij * xij));
            double lx = 0.0;
            double lx_abs = 0.0;
            for (int64_t k = j; k <= i; ++k) {
                const double term = at(k, i) * inv[static_cast<std::size_t>(k * n_vars + j)];
                lx += term;
                lx_abs += std::fabs(term);
            }
            const double rij = (i == j ? 1.0 : 0.0) - lx;
            double e = next_up(std::fabs(rij));
            e = next_up(e + next_up(gamma_of(n_vars + 2) * next_up(lx_abs + 1.0)));
            r_sq = next_up(r_sq + next_up(e * e));
        }
    }
    const double r_norm = next_up(std::sqrt(r_sq));
    if (!(r_norm < 1.0)) return kInf;
    const double one_less = next_down(1.0 - r_norm);
    const double inv_sq = next_up(next_up(x_sq / one_less) / one_less);

    const double resid = next_up(next_up(std::sqrt(num)) * inv_sq);
    if (!std::isfinite(resid)) return kInf;
    return next_up(rel + resid);
}

}  // namespace detail

// The tree over `src`'s columns, or an empty tree when the metric or the
// covariance leaves no bound. An empty tree is a routing answer rather than an
// error: it says this source is price_block()'s to scan.
inline BallTree build_ball_tree(const LazyCostMatrix& src, int32_t leaf_size = 16) {
    BallTree tree;
    if (!metric_has_ball_bound(src.metric())) return tree;
    if (src.ncol <= 0 || src.n_vars() <= 0) return tree;

    const int64_t n_vars = src.n_vars();
    if (src.metric() == DistanceMetric::Mahalanobis) {
        if (static_cast<int64_t>(src.inv_cov().size()) != n_vars * n_vars) return tree;
        std::vector<double> l;
        if (!cholesky_lower(src.inv_cov(), n_vars, l)) return tree;
        tree.factor.assign(static_cast<std::size_t>(n_vars * n_vars), 0.0);
        for (int64_t m = 0; m < n_vars; ++m) {
            for (int64_t k = 0; k <= m; ++k) {
                tree.factor[static_cast<std::size_t>(k * n_vars + m)] =
                    l[static_cast<std::size_t>(m * n_vars + k)];
            }
        }
        // The largest row sum of |sym(inv_cov)|, the matrix the source's row
        // sums are taken over, entrywise absolute.
        const std::vector<double>& a = src.inv_cov();
        double row_max = 0.0;
        for (int64_t i = 0; i < n_vars; ++i) {
            double row = 0.0;
            for (int64_t j = 0; j < n_vars; ++j) {
                row += std::fabs(0.5 * (a[static_cast<std::size_t>(i * n_vars + j)] +
                                        a[static_cast<std::size_t>(j * n_vars + i)]));
            }
            if (!(row <= row_max)) row_max = row;
        }
        tree.src_quad_coef = detail::gamma_of(2 * n_vars + 2) * row_max *
                             (1.0 + detail::gamma_of(2 * n_vars + 6));
    }

    tree.n_vars = n_vars;
    tree.n_units = static_cast<int32_t>(src.ncol);
    tree.leaf_size = leaf_size > 0 ? leaf_size : 1;
    // Both parts also carry the rounding of the sums node_ball_bounds() forms
    // them into, so a node visit needs no directed rounding of its own. Those
    // sums take at most eight rounded operations over the centre distance, the
    // radius and the absolute slack, so their error is at most gamma_8 times
    // the sum of those magnitudes, and gamma_8 is added to the relative part
    // and multiplied into the absolute one. The coefficient on g(x) is also
    // widened by g's own rounding, a sum of products of absolute values over
    // the factor's rows, squared and summed again under one square root.
    const double g_combine = detail::gamma_of(8);
    tree.bound_rel = detail::next_up(
        detail::bound_allowance(src, tree.factor, n_vars) + g_combine);
    tree.bound_abs_coef = detail::next_up(
        detail::gamma_of(n_vars + 2) * (1.0 + detail::gamma_of(3 * n_vars + 4)) *
        (1.0 + g_combine));

    // The midpoint of the controls' own bounding box, which is the origin that
    // makes the largest |x - origin| over them as small as it can be.
    tree.origin.assign(static_cast<std::size_t>(n_vars), 0.0);
    {
        std::vector<double> box_min(static_cast<std::size_t>(n_vars),
                                    std::numeric_limits<double>::infinity());
        std::vector<double> box_max(static_cast<std::size_t>(n_vars),
                                    -std::numeric_limits<double>::infinity());
        for (int64_t j = 0; j < src.ncol; ++j) {
            const double* px = src.right_row(j);
            for (int64_t k = 0; k < n_vars; ++k) {
                const std::size_t s = static_cast<std::size_t>(k);
                if (px[k] < box_min[s]) box_min[s] = px[k];
                if (px[k] > box_max[s]) box_max[s] = px[k];
            }
        }
        for (int64_t k = 0; k < n_vars; ++k) {
            const std::size_t s = static_cast<std::size_t>(k);
            const double mid = 0.5 * (box_min[s] + box_max[s]);
            tree.origin[s] = std::isfinite(mid) ? mid : 0.0;
        }
    }

    tree.whitened.assign(static_cast<std::size_t>(src.ncol) *
                             static_cast<std::size_t>(n_vars), 0.0);
    double g_max = 0.0;
    for (int64_t j = 0; j < src.ncol; ++j) {
        const double g = whiten_point(
            tree, src.right_row(j),
            &tree.whitened[static_cast<std::size_t>(j) *
                           static_cast<std::size_t>(n_vars)]);
        if (g > g_max) g_max = g;
    }
    tree.g_max = g_max;

    tree.perm.resize(static_cast<std::size_t>(src.ncol));
    for (int32_t j = 0; j < tree.n_units; ++j) {
        tree.perm[static_cast<std::size_t>(j)] = j;
    }
    detail::ball_split(tree, 0, tree.n_units);

    const std::size_t n_nodes = tree.lo.size();
    const std::size_t span = static_cast<std::size_t>(n_vars);
    tree.centre.assign(n_nodes * span, 0.0);
    tree.box_lo.assign(n_nodes * span, 0.0);
    tree.box_hi.assign(n_nodes * span, 0.0);
    tree.radius.assign(n_nodes, 0.0);
    tree.max_v.assign(n_nodes, 0.0);
    const std::vector<CaliperSpec>& cals = src.calipers();
    tree.n_cal = static_cast<int64_t>(cals.size());
    const std::size_t n_cal = cals.size();
    tree.cal_lo.assign(n_nodes * n_cal, std::numeric_limits<double>::infinity());
    tree.cal_hi.assign(n_nodes * n_cal, -std::numeric_limits<double>::infinity());

    for (int32_t id = static_cast<int32_t>(n_nodes) - 1; id >= 0; --id) {
        const std::size_t base = static_cast<std::size_t>(id) * span;
        const int32_t a = tree.lo[static_cast<std::size_t>(id)];
        const int32_t b = tree.hi[static_cast<std::size_t>(id)];

        double* ctr = &tree.centre[base];
        for (int32_t t = a; t < b; ++t) {
            const double* pw = tree.unit_whitened(tree.perm[static_cast<std::size_t>(t)]);
            for (int64_t k = 0; k < n_vars; ++k) ctr[k] += pw[k];
        }
        const double inv = 1.0 / static_cast<double>(b - a);
        for (int64_t k = 0; k < n_vars; ++k) ctr[k] *= inv;

        double worst = 0.0;
        for (int32_t t = a; t < b; ++t) {
            const double* pw = tree.unit_whitened(tree.perm[static_cast<std::size_t>(t)]);
            double s = 0.0;
            for (int64_t k = 0; k < n_vars; ++k) {
                const double d = pw[k] - ctr[k];
                s += d * d;
            }
            if (s > worst) worst = s;
        }
        tree.radius[static_cast<std::size_t>(id)] = std::sqrt(worst);

        double* blo = &tree.box_lo[base];
        double* bhi = &tree.box_hi[base];
        if (tree.is_leaf(id)) {
            for (int64_t k = 0; k < n_vars; ++k) {
                blo[k] = std::numeric_limits<double>::infinity();
                bhi[k] = -std::numeric_limits<double>::infinity();
            }
            for (int32_t t = a; t < b; ++t) {
                const double* px = src.right_row(
                    static_cast<int64_t>(tree.perm[static_cast<std::size_t>(t)]));
                for (int64_t k = 0; k < n_vars; ++k) {
                    if (px[k] < blo[k]) blo[k] = px[k];
                    if (px[k] > bhi[k]) bhi[k] = px[k];
                }
            }
        } else {
            const double* llo = tree.node_box_lo(tree.left[static_cast<std::size_t>(id)]);
            const double* lhi = tree.node_box_hi(tree.left[static_cast<std::size_t>(id)]);
            const double* rlo = tree.node_box_lo(tree.right[static_cast<std::size_t>(id)]);
            const double* rhi = tree.node_box_hi(tree.right[static_cast<std::size_t>(id)]);
            for (int64_t k = 0; k < n_vars; ++k) {
                blo[k] = llo[k] < rlo[k] ? llo[k] : rlo[k];
                bhi[k] = lhi[k] > rhi[k] ? lhi[k] : rhi[k];
            }
        }

        double* clo = &tree.cal_lo[static_cast<std::size_t>(id) * n_cal];
        double* chi = &tree.cal_hi[static_cast<std::size_t>(id) * n_cal];
        if (tree.is_leaf(id)) {
            for (int32_t t = a; t < b; ++t) {
                const std::size_t col =
                    static_cast<std::size_t>(tree.perm[static_cast<std::size_t>(t)]);
                for (std::size_t c = 0; c < n_cal; ++c) {
                    const double x = cals[c].right[col];
                    if (x < clo[c]) clo[c] = x;
                    if (x > chi[c]) chi[c] = x;
                }
            }
        } else {
            const std::size_t l =
                static_cast<std::size_t>(tree.left[static_cast<std::size_t>(id)]) * n_cal;
            const std::size_t r =
                static_cast<std::size_t>(tree.right[static_cast<std::size_t>(id)]) * n_cal;
            for (std::size_t c = 0; c < n_cal; ++c) {
                clo[c] = std::min(tree.cal_lo[l + c], tree.cal_lo[r + c]);
                chi[c] = std::max(tree.cal_hi[l + c], tree.cal_hi[r + c]);
            }
        }
    }

    return tree;
}

// The whitened separation between a query point and the nearest and furthest
// members a node could hold. Both are bounds over the ball rather than
// distances any member attains.
struct BallBounds {
    double d_lo = 0.0;
    double d_hi = 0.0;
};

inline BallBounds node_ball_bounds(const BallTree& tree, const double* q_whitened,
                                   double q_g, int32_t id) {
    const int64_t n = tree.n_vars;
    const double* ctr = tree.node_centre(id);
    double s = 0.0;
    for (int64_t k = 0; k < n; ++k) {
        const double d = q_whitened[k] - ctr[k];
        s += d * d;
    }
    const double dc = std::sqrt(s);
    const double r = tree.radius[static_cast<std::size_t>(id)];
    // The near side of the ball is what a prune rests on, so the centre
    // distance is read low and the radius high; the far side, which bounds a
    // maximised cost, is read the other way. Both are moved by the tree's own
    // allowance, so neither side can be tighter than the arithmetic that
    // produced it. The relative part covers the sum of squares and the factor
    // residual; the absolute part covers the whitening of the query and of the
    // controls the node's geometry was built from, whose rounding does not
    // shrink as the two points approach each other.
    // The rounding of the sums below is already in both allowances; see
    // build_ball_tree().
    const double rel = tree.bound_rel;
    const double abs_slack = tree.bound_abs_coef * (q_g + 2.0 * tree.g_max);
    const double dc_lo = dc - rel * dc;
    const double r_hi = r + rel * r;
    BallBounds out;
    const double near = dc_lo - r_hi - abs_slack;
    out.d_lo = near > 0.0 ? near : 0.0;
    const double far = dc + r;
    out.d_hi = far + rel * far + abs_slack;
    return out;
}

// The query's largest reach to the node's box along one variable, which bounds
// |d_k| over every column the node holds.
inline double box_reach(const double* q_original, const double* blo,
                        const double* bhi, int64_t k) {
    const double a = std::fabs(q_original[k] - blo[k]);
    const double b = std::fabs(q_original[k] - bhi[k]);
    return a > b ? a : b;
}

// A bound on |fl(d' A d) - d' A d| over every column the node holds, which is
// what the source's row-sum evaluation can be wrong by. Zero for a metric whose
// terms are non-negative, where the sum cannot cancel.
inline double source_quadform_slack(const BallTree& tree, const double* q_original,
                                    int32_t id) {
    if (!(tree.src_quad_coef > 0.0)) return 0.0;
    const int64_t n = tree.n_vars;
    const double* blo = tree.node_box_lo(id);
    const double* bhi = tree.node_box_hi(id);
    double r2 = 0.0;
    for (int64_t k = 0; k < n; ++k) {
        const double r = box_reach(q_original, blo, bhi, k);
        r2 += r * r;
    }
    return tree.src_quad_coef * r2;
}

// The ball's bounds carried across to the distance the SOURCE reports. The
// slack is absolute on the squared distance, so it is taken there and the root
// re-applied, each step rounded away from the bound it is widening.
inline BallBounds widen_for_source(const BallBounds& b, double slack) {
    if (!(slack > 0.0)) return b;
    BallBounds out = b;
    double lo2 = detail::next_down(b.d_lo * b.d_lo);
    lo2 = detail::next_down(lo2 - slack);
    out.d_lo = lo2 > 0.0 ? detail::next_down(std::sqrt(lo2)) : 0.0;
    double hi2 = detail::next_up(b.d_hi * b.d_hi);
    hi2 = detail::next_up(hi2 + slack);
    out.d_hi = detail::next_up(std::sqrt(hi2));
    return out;
}

// The bounds every cost-level question is answered from: the ball's geometry,
// then the source's own evaluation error. One place, so no caller can ask the
// geometric bound a question that is decided against the source's arithmetic.
inline BallBounds node_bounds_for_source(const BallTree& tree,
                                         const double* q_whitened, double q_g,
                                         const double* q_original, int32_t id) {
    return widen_for_source(node_ball_bounds(tree, q_whitened, q_g, id),
                            source_quadform_slack(tree, q_original, id));
}

// A lower bound on src.at(i, j) over every column the node holds. Under
// maximize the cost falls as the distance grows, so the bound comes off the far
// side of the ball.
inline double cost_lo_of(const LazyCostMatrix& src, const BallBounds& b) {
    // metric_cost_of is monotone in the distance, so a bound on the distance
    // carries to a bound on the cost, and its own rounding is one step.
    const double c = src.is_negated() ? -metric_cost_of(src.metric(), b.d_hi)
                                      : metric_cost_of(src.metric(), b.d_lo);
    return detail::next_down(c);
}

// Whether max_distance forbids every column the node holds. The comparison is
// against the distance the source measures, before the maximize flag, which is
// the order raw_distance() is tested in.
inline bool distance_out_of(const LazyCostMatrix& src, const BallBounds& b) {
    const double limit = src.max_distance();
    if (!std::isfinite(limit)) return false;
    // metric_cost_of is one more rounded operation on a bound already read as
    // low as it goes, so it is stepped down once more before a comparison whose
    // true answer would rule the node out.
    return detail::next_down(metric_cost_of(src.metric(), b.d_lo)) > limit;
}

// The same two questions from a node rather than from a ball already measured.
// A caller asking both about one node measures the ball once and reads them
// off it; these are for a caller asking one.
inline double node_cost_lo(const BallTree& tree, const LazyCostMatrix& src,
                           const double* q_whitened, double q_g,
                           const double* q_original, int32_t id) {
    return cost_lo_of(src,
                      node_bounds_for_source(tree, q_whitened, q_g, q_original, id));
}

inline bool node_distance_out(const BallTree& tree, const LazyCostMatrix& src,
                              const double* q_whitened, double q_g,
                              const double* q_original, int32_t id) {
    if (!std::isfinite(src.max_distance())) return false;
    return distance_out_of(
        src, node_bounds_for_source(tree, q_whitened, q_g, q_original, id));
}

// Whether the calipers forbid every column the node holds, for left unit `qi`.
// Read in each caliper's raw values, where the per-variable window is stated.
inline bool node_caliper_out(const BallTree& tree, const LazyCostMatrix& src,
                             int64_t qi, int32_t id) {
    const std::vector<CaliperSpec>& cals = src.calipers();
    if (cals.empty()) return false;
    const std::size_t base = static_cast<std::size_t>(id) *
                             static_cast<std::size_t>(tree.n_cal);
    for (std::size_t c = 0; c < cals.size(); ++c) {
        const double x = cals[c].left[static_cast<std::size_t>(qi)];
        // The window's two endpoints are each one rounded addition, and the
        // box holds values copied rather than computed. Widening the window by
        // one representable step on each side puts the rounding on the side
        // that declines to prune.
        const double hi_edge = detail::next_up(x + cals[c].threshold);
        const double lo_edge = detail::next_down(x - cals[c].threshold);
        if (tree.cal_lo[base + c] > hi_edge) return true;
        if (tree.cal_hi[base + c] < lo_edge) return true;
    }
    return false;
}

// A lower bound on the cost of every column the node holds for left unit `qi`,
// whose coordinates are `q_original`, infinite when the node's box or its ball
// puts them all out of reach. This is the one question a descent asks of a
// node, and both the caliper and the distance limit answer it by ruling the
// node out rather than by bounding it.
inline double node_cost_floor(const BallTree& tree, const LazyCostMatrix& src,
                              const double* q_whitened, double q_g,
                              const double* q_original, int64_t qi, int32_t id) {
    if (node_caliper_out(tree, src, qi, id)) {
        return std::numeric_limits<double>::infinity();
    }
    const BallBounds b =
        node_bounds_for_source(tree, q_whitened, q_g, q_original, id);
    if (distance_out_of(src, b)) return std::numeric_limits<double>::infinity();
    const double c = cost_lo_of(src, b);
    // Infinity here is the node being out of reach, which is a prune every
    // caller is entitled to make. A floor that is not a number is the opposite:
    // it bounds nothing, so it is reported as no bound at all and the node is
    // read rather than skipped. Answered here so that no descent has to test
    // for it separately.
    if (std::isnan(c)) return -std::numeric_limits<double>::infinity();
    return c;
}

// One bottom-up pass setting each node's largest column dual, which is the
// half of the pricing bound that moves. Children are appended after their
// parent, so a reverse sweep over the nodes fills every one after both of its
// children and the pass is O(n_units + n_nodes).
inline void refresh_max_v(BallTree& tree, const std::vector<double>& v) {
    if (tree.empty()) return;
    if (static_cast<int32_t>(v.size()) != tree.n_units) {
        LAP_THROW_DIMENSION("refresh_max_v: " + std::to_string(v.size()) +
                            " column duals for " + std::to_string(tree.n_units) +
                            " columns");
    }
    for (int32_t id = tree.n_nodes() - 1; id >= 0; --id) {
        if (tree.is_leaf(id)) {
            double m = -std::numeric_limits<double>::infinity();
            const int32_t b = tree.hi[static_cast<std::size_t>(id)];
            for (int32_t t = tree.lo[static_cast<std::size_t>(id)]; t < b; ++t) {
                const double x = v[static_cast<std::size_t>(
                    tree.perm[static_cast<std::size_t>(t)])];
                if (x > m) m = x;
            }
            tree.max_v[static_cast<std::size_t>(id)] = m;
        } else {
            const double l = tree.max_v[static_cast<std::size_t>(
                tree.left[static_cast<std::size_t>(id)])];
            const double r = tree.max_v[static_cast<std::size_t>(
                tree.right[static_cast<std::size_t>(id)])];
            tree.max_v[static_cast<std::size_t>(id)] = l > r ? l : r;
        }
    }
}

}  // namespace lap
