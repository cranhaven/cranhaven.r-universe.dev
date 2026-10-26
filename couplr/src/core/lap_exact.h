// src/core/lap_exact.h
// Exact sign of c - u - v over IEEE doubles, with no tolerance anywhere.
// Pure C++, no Rcpp, so the C++ test harness reaches it as well as the
// certificate does.
//
// Every condition the optimality certificate checks reduces to one question
// asked of three numbers: is c_ij - u_i - v_j positive, zero, or negative. A
// double is a rational number, p / 2^k with p and k integers, so that question
// has an exact answer. A verifier deciding it within a band around zero is
// making a statement about a neighbourhood of the instance rather than about
// the instance: at tolerance eps it accepts potentials that violate dual
// feasibility by eps on any pair and miss tightness by eps on any matched arc,
// which bounds the matching's excess over the optimum by about
// 2 (nrow + ncol) eps rather than pinning it at zero.
//
// Setting the band to zero and reading the sign of the double evaluation is
// not the fix, because that sign can be wrong. With c = 2^-60, u = 1 and
// v = -1 the first subtraction rounds to -1 and the expression returns 0 on a
// pair whose reduced cost is positive; at c = -2^-60 it returns 0 on a pair
// whose reduced cost is negative, which is a dual infeasibility a zero-band
// check would accept.
//
// The band goes away once the expression is evaluated exactly. Knuth's two-sum
// splits a rounded addition into the sum plus the part that rounding lost, both
// doubles, and both together equal to the true value. Applying it twice writes
// c - u - v as a non-overlapping expansion of three doubles whose sum is the
// exact difference; the sign of that sum is the sign of its most significant
// non-zero component. The technique and the non-overlap property are
// Shewchuk's (1997, "Adaptive Precision Floating-Point Arithmetic and Fast
// Robust Geometric Predicates").
//
// The expansion costs about ten flops against two, so it runs behind a filter:
// a rounding-error bound on the double evaluation decides the pairs that are
// clear of zero, and only pairs inside the bound are expanded. On a dense scan
// almost every pair is clear, and the pairs that are not are the tight ones,
// which are the ones worth the arithmetic.
#pragma once

#include <cfloat>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace lap {
namespace exact {

// Knuth's two-sum. Returns fl(a + b) and writes into `err` the part the
// rounding lost, so that a + b == result + err holds exactly in the reals. No
// assumption about the relative magnitude of a and b, and no branch. Exact
// whenever the addition does not overflow.
inline double two_sum(double a, double b, double& err) {
    const double s = a + b;
    const double b_virtual = s - a;
    err = (a - (s - b_virtual)) + (b - b_virtual);
    return s;
}

// a + b rounded toward +infinity: the smallest double not below the exact sum.
// Two-sum returns the part that rounding to nearest lost, which is at most half
// an ulp of the rounded sum, so a positive remainder is covered by one step up.
// An overflowed sum is +/-infinity with a NaN remainder and is returned as is.
inline double sum_round_up(double a, double b) {
    double err = 0.0;
    const double s = two_sum(a, b, err);
    return err > 0.0 ? std::nextafter(s, std::numeric_limits<double>::infinity()) : s;
}

// Sign of c - u - v, exactly: -1, 0 or +1.
//
// The filter is a bound on the error of the two roundings in fl(fl(c - u) - v).
// Each is relative to the magnitude of its own operands, so their sum is
// bounded by a small multiple of the unit roundoff times |c| + |u| + |v|; the
// multiple below is generous, which costs a few expansions on pairs that did
// not need one and never returns a sign the expansion would disagree with.
//
// Values large enough to overflow the double evaluation take the sign of the
// overflowed sum, which is the sign of the dominant term and is the answer the
// expansion would reach if it could represent it.
inline int sign_reduced_cost(double c, double u, double v) {
    const double approx = (c - u) - v;
    const double magnitude = std::fabs(c) + std::fabs(u) + std::fabs(v);
    const double bound = 4.0 * DBL_EPSILON * magnitude;

    if (approx > bound) return 1;
    if (approx < -bound) return -1;
    if (!std::isfinite(approx)) return approx > 0.0 ? 1 : -1;

    // Grow the one-component expansion [c] by -u, then the two-component
    // result by -v. Shewchuk's grow_expansion: the running sum absorbs each
    // term and the piece rounding lost is set aside as a component of lower
    // magnitude. What comes out is non-overlapping and ordered, so the first
    // non-zero component from the top carries the sign of the whole.
    double low_u = 0.0;
    const double high_u = two_sum(-u, c, low_u);

    double low_a = 0.0;
    double running = two_sum(-v, low_u, low_a);
    double low_b = 0.0;
    running = two_sum(running, high_u, low_b);

    if (running != 0.0) return running > 0.0 ? 1 : -1;
    if (low_b != 0.0) return low_b > 0.0 ? 1 : -1;
    if (low_a != 0.0) return low_a > 0.0 ? 1 : -1;
    return 0;
}

// ---------------------------------------------------------------------------
// Expansions
// ---------------------------------------------------------------------------
//
// A value that is a sum of several doubles -- a shortest-path distance over
// cost entries, a potential derived from one -- is generally not a double
// itself, and rounding it to one is exactly what costs a certificate its
// exactness. Such a value is held as a non-overlapping expansion: doubles in
// increasing order of magnitude, none zero, whose exact sum is the value. The
// empty expansion is zero. Everything below is Shewchuk's (1997), with zero
// elimination throughout, so an expansion's length is the number of pieces it
// needs rather than the number of terms that went into it.
//
// The operations are exact whenever no intermediate sum overflows, which at the
// magnitudes a cost matrix carries is always.
using Expansion = std::vector<double>;

// fl(a + b) and the part rounding lost, given |a| >= |b| or a == 0. Three flops
// against two-sum's six, and the precondition is what compress() arranges.
inline double fast_two_sum(double a, double b, double& err) {
    const double s = a + b;
    const double b_virtual = s - a;
    err = b - b_virtual;
    return s;
}

// e + b into `out`. Shewchuk's GROW-EXPANSION: the running sum absorbs each
// component in turn and every piece rounding lost is set aside, smallest first,
// so the result is again non-overlapping and increasing. `out` must not alias
// `e`.
inline void grow_expansion(const Expansion& e, double b, Expansion& out) {
    out.clear();
    double q = b;
    for (double component : e) {
        double h = 0.0;
        q = two_sum(q, component, h);
        if (h != 0.0) out.push_back(h);
    }
    if (q != 0.0) out.push_back(q);
}

// Shewchuk's COMPRESS: the same value in as few components as it needs. Two
// passes of fast two-sum, top down and then bottom up, leave a non-adjacent
// expansion whose largest component approximates the whole to within one unit
// in its last place. Growing an expansion term by term lengthens it by one per
// term whatever the value, so a distance summed along a long path is compressed
// as it goes, which is what keeps its length at the handful of components its
// value actually spans.
inline void compress(Expansion& e) {
    const std::size_t m = e.size();
    if (m < 2) return;
    std::vector<double> g(m);
    std::size_t bottom = m - 1;
    double q = e[m - 1];
    for (std::size_t k = m - 1; k-- > 0;) {
        double small = 0.0;
        const double big = fast_two_sum(q, e[k], small);
        if (small != 0.0) {
            g[bottom--] = big;
            q = small;
        } else {
            q = big;
        }
    }
    g[bottom] = q;
    std::size_t top = 0;
    for (std::size_t k = bottom + 1; k < m; ++k) {
        double small = 0.0;
        const double big = fast_two_sum(g[k], q, small);
        if (small != 0.0) e[top++] = small;
        q = big;
    }
    e[top++] = q;
    e.resize(top);
    if (e.size() == 1 && e[0] == 0.0) e.clear();
}

// e + f, exactly and compressed.
inline Expansion expansion_sum(const Expansion& e, const Expansion& f) {
    Expansion acc = e;
    Expansion next;
    for (double component : f) {
        grow_expansion(acc, component, next);
        acc.swap(next);
    }
    compress(acc);
    return acc;
}

// e + a - b for doubles a and b, exactly and compressed. The shape every
// potential and every relaxation below takes: a distance plus one cost entry
// less another.
inline Expansion add_difference(const Expansion& e, double a, double b) {
    Expansion once;
    grow_expansion(e, a, once);
    Expansion twice;
    grow_expansion(once, -b, twice);
    compress(twice);
    return twice;
}

inline Expansion negated(const Expansion& e) {
    Expansion out(e.size());
    for (std::size_t k = 0; k < e.size(); ++k) out[k] = -e[k];
    return out;
}

// The sign of the value, exactly: the largest component carries it, since every
// smaller one is below half a unit in its last place.
inline int sign(const Expansion& e) {
    if (e.empty()) return 0;
    return e.back() > 0.0 ? 1 : -1;
}

// The value rounded to a double, and a bound on how far that rounding sits
// from it. Summing the components smallest first commits one rounding per
// addition, each at most half a unit in the last place of a partial sum no
// larger than the sum of the magnitudes, so that sum times the component count
// times epsilon covers all of them. The bound is what lets a comparison between
// two expansions be settled in doubles whenever the doubles are clear of each
// other, and taken exactly only when they are not.
struct Approximation {
    double value = 0.0;
    double error = 0.0;
};

inline Approximation approximate(const Expansion& e) {
    Approximation out;
    double magnitude = 0.0;
    for (double component : e) {
        out.value += component;
        magnitude += std::fabs(component);
    }
    if (e.size() > 1) {
        out.error = static_cast<double>(e.size()) * DBL_EPSILON * magnitude;
    }
    return out;
}

// Dekker's two-product through a fused multiply-add: returns fl(a * b) and
// writes into `err` the part the rounding lost, so that a * b == result + err
// holds exactly. std::fma rounds once, which is what makes the remainder exact.
inline double two_product(double a, double b, double& err) {
    const double p = a * b;
    err = std::fma(a, b, -p);
    return p;
}

// a * b as an expansion, exactly.
inline Expansion product(double a, double b) {
    double lo = 0.0;
    const double hi = two_product(a, b, lo);
    Expansion out;
    if (lo != 0.0) out.push_back(lo);
    if (hi != 0.0) out.push_back(hi);
    return out;
}

// e * b, exactly and compressed. Shewchuk's SCALE-EXPANSION: each component's
// product splits into a high and a low part, and the running sum absorbs them
// with every piece rounding lost set aside, as grow_expansion() does for a sum.
inline Expansion scale_expansion(const Expansion& e, double b) {
    Expansion out;
    if (e.empty() || b == 0.0) return out;
    out.reserve(2 * e.size());
    double lo = 0.0;
    double q = two_product(e[0], b, lo);
    if (lo != 0.0) out.push_back(lo);
    for (std::size_t k = 1; k < e.size(); ++k) {
        double p_lo = 0.0;
        const double p_hi = two_product(e[k], b, p_lo);
        double s_lo = 0.0;
        const double s = two_sum(q, p_lo, s_lo);
        if (s_lo != 0.0) out.push_back(s_lo);
        double t_lo = 0.0;
        q = two_sum(p_hi, s, t_lo);
        if (t_lo != 0.0) out.push_back(t_lo);
    }
    if (q != 0.0) out.push_back(q);
    compress(out);
    return out;
}

// Sign of e - y, exactly.
inline int compare(const Expansion& e, double y) {
    Expansion diff;
    grow_expansion(e, -y, diff);
    return sign(diff);
}

// Sign of e - f, exactly.
inline int compare(const Expansion& e, const Expansion& f) {
    return sign(expansion_sum(e, negated(f)));
}

// The largest double not above the value, and the smallest not below it. A
// compressed expansion's top component is within one unit in its last place of
// the whole, so each loop below takes at most a step or two.
inline double round_down(const Expansion& e) {
    Expansion c = e;
    compress(c);
    double x = c.empty() ? 0.0 : c.back();
    const double down = -std::numeric_limits<double>::infinity();
    const double up = std::numeric_limits<double>::infinity();
    while (compare(c, x) < 0) x = std::nextafter(x, down);
    for (;;) {
        const double y = std::nextafter(x, up);
        if (compare(c, y) < 0) break;
        x = y;
    }
    return x;
}

inline double round_up(const Expansion& e) {
    return -round_down(negated(e));
}

// The smallest integer q with q * d >= e + a, for d > 0: the ceiling of the
// quotient, decided exactly. The double quotient starts the search, and q * d
// is held exactly as a two-component product, so each test is one exact sign.
// Every q a caller passes through here is far below 2^53, where a double counts
// exactly.
inline double ceil_quotient(const Expansion& e, double a, double d) {
    Expansion t;
    grow_expansion(e, a, t);
    compress(t);
    const auto above = [&](double q) {
        return sign(expansion_sum(t, negated(product(q, d)))) > 0;
    };
    double q = std::ceil((t.empty() ? 0.0 : t.back()) / d);
    while (above(q)) q += 1.0;
    while (!above(q - 1.0)) q -= 1.0;
    return q;
}

// Sign of c - U - V for a double c and expansions U and V, exactly. The
// certificate's question once the potentials are expansions rather than
// doubles; the same filter as sign_reduced_cost() decides the pairs clear of
// zero from the rounded potentials and their error bounds, and only the rest
// are expanded.
inline int sign_reduced_cost(double c, const Expansion& u, const Approximation& ua,
                             const Expansion& v, const Approximation& va) {
    const double approx = (c - ua.value) - va.value;
    const double magnitude = std::fabs(c) + std::fabs(ua.value) + std::fabs(va.value);
    const double bound = 4.0 * DBL_EPSILON * magnitude + ua.error + va.error;
    if (approx > bound) return 1;
    if (approx < -bound) return -1;
    if (!std::isfinite(approx)) return approx > 0.0 ? 1 : -1;

    Expansion acc;
    if (c != 0.0) acc.push_back(c);
    const Expansion total = expansion_sum(expansion_sum(acc, negated(u)), negated(v));
    return sign(total);
}

}  // namespace exact
}  // namespace lap
