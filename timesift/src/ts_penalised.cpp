#include "ts_penalised.h"

#include <algorithm>
#include <atomic>
#include <cmath>
#include <functional>
#include <limits>
#include <system_error>
#include <thread>

namespace timesift {
namespace {

// glmnet's own control constants, which are part of what is being matched rather than settings of
// ours: the probability a fitted case is pinned at, the linear predictor's clamp, the floor under
// the mixing when the largest penalty is derived, and the probability the held-out deviance is
// read at.
constexpr double kProbFloor = 1e-9;
constexpr double kEtaClamp = 250.0;
constexpr double kAlphaFloor = 1e-3;
constexpr double kCVProbFloor = 1e-5;



// The inner products the descent lives on, summed into four accumulators. A single accumulator
// chains every addition on the one before it, and the descent then runs at the latency of a
// floating-point add rather than at the throughput of one; the package is compiled at the
// optimisation R sets, which neither vectorises this nor reassociates it. The order is fixed, so
// the sum is the same number on every platform.
double dot(const double* a, const double* b, std::size_t n) {
  double s0 = 0.0, s1 = 0.0, s2 = 0.0, s3 = 0.0;
  std::size_t i = 0;
  for (; i + 4 <= n; i += 4) {
    s0 += a[i] * b[i];
    s1 += a[i + 1] * b[i + 1];
    s2 += a[i + 2] * b[i + 2];
    s3 += a[i + 3] * b[i + 3];
  }
  double s = (s0 + s1) + (s2 + s3);
  for (; i < n; ++i) s += a[i] * b[i];
  return s;
}

double dot3(const double* a, const double* b, const double* c, std::size_t n) {
  double s0 = 0.0, s1 = 0.0, s2 = 0.0, s3 = 0.0;
  std::size_t i = 0;
  for (; i + 4 <= n; i += 4) {
    s0 += a[i] * b[i] * c[i];
    s1 += a[i + 1] * b[i + 1] * c[i + 1];
    s2 += a[i + 2] * b[i + 2] * c[i + 2];
    s3 += a[i + 3] * b[i + 3] * c[i + 3];
  }
  double s = (s0 + s1) + (s2 + s3);
  for (; i < n; ++i) s += a[i] * b[i] * c[i];
  return s;
}

double total(const double* a, std::size_t n) {
  double s0 = 0.0, s1 = 0.0, s2 = 0.0, s3 = 0.0;
  std::size_t i = 0;
  for (; i + 4 <= n; i += 4) {
    s0 += a[i];
    s1 += a[i + 1];
    s2 += a[i + 2];
    s3 += a[i + 3];
  }
  double s = (s0 + s1) + (s2 + s3);
  for (; i < n; ++i) s += a[i];
  return s;
}

// The maps the descent lives on beside the inner products: a case's own arithmetic, over every
// case. Two cases are written per turn of the loop, which is what lets the compiler issue the two
// as one at the optimisation R builds a package at; each case is still the expression it was, in
// the order it was, so the numbers are the ones a case-at-a-time loop gives.
void add_scaled(double* out, const double* a, double b, std::size_t n) {
  std::size_t i = 0;
  for (; i + 2 <= n; i += 2) {
    out[i] += b * a[i];
    out[i + 1] += b * a[i + 1];
  }
  for (; i < n; ++i) out[i] += b * a[i];
}

void add_scaled3(double* out, const double* a, const double* c, double b, std::size_t n) {
  std::size_t i = 0;
  for (; i + 2 <= n; i += 2) {
    out[i] += b * a[i] * c[i];
    out[i + 1] += b * a[i + 1] * c[i + 1];
  }
  for (; i < n; ++i) out[i] += b * a[i] * c[i];
}

double log1pexp(double x) {
  if (x > 0.0) return x + std::log1p(std::exp(-x));
  return std::log1p(std::exp(x));
}

// The design the descent runs over: every column centred on its weighted mean and divided by its
// weighted standard deviation, so a penalty that is one number over every column costs a column
// the same whatever it was recorded on. A column holding one value has no spread to divide by and
// is carried through at zero.
//
// A Gaussian fit's quadratic weight is the case weight, which does not move along the path, so its
// root is folded into every column and into the intercept's own column: the quadratic then has
// unit weight, and a coordinate's step reads the column and the residual and nothing beside them.
// A binomial fit's weight moves with every reweighting, and its columns are the standardised ones
// with an intercept column of ones.
struct Design {
  std::size_t n = 0;
  std::size_t p = 0;
  bool folded = false;
  std::vector<double> xt;            // [n, p] column-major
  std::vector<double> centre, scale;
  std::vector<std::uint8_t> usable;
  std::vector<double> vp;            // penalty factor, rescaled to sum to p
  std::vector<double> w;             // case weights, summing to one
  std::vector<double> lead;          // the intercept's column

  const double* column(std::size_t j) const { return xt.data() + j * n; }
};

Design build_design(const double* x, const double* w_in, std::size_t n, std::size_t p,
                    const PenaltySpec& spec, bool folded) {
  Design d;
  d.n = n;
  d.p = p;
  d.w.assign(n, 0.0);
  double total = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double wi = w_in == nullptr ? 1.0 : w_in[i];
    if (!(wi >= 0.0) || !std::isfinite(wi)) {
      throw Error("a case weight of a penalised fit is negative or not a number.");
    }
    d.w[i] = wi;
    total += wi;
  }
  if (!(total > 0.0)) throw Error("a penalised fit was handed case weights that sum to zero.");
  for (std::size_t i = 0; i < n; ++i) d.w[i] /= total;
  d.folded = folded;
  d.lead.assign(n, 1.0);
  if (folded) {
    for (std::size_t i = 0; i < n; ++i) d.lead[i] = std::sqrt(d.w[i]);
  }

  d.xt.assign(n * p, 0.0);
  d.centre.assign(p, 0.0);
  d.scale.assign(p, 1.0);
  d.usable.assign(p, 1);
  for (std::size_t j = 0; j < p; ++j) {
    const double* col = x + j * n;
    double centre = 0.0;
    if (spec.intercept) {
      for (std::size_t i = 0; i < n; ++i) centre += d.w[i] * col[i];
    }
    double spread = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double z = col[i] - centre;
      spread += d.w[i] * z * z;
    }
    spread = std::sqrt(spread);
    if (!(spread > 0.0) || !std::isfinite(spread)) {
      d.usable[j] = 0;
      d.centre[j] = centre;
      d.scale[j] = 1.0;
      continue;
    }
    d.centre[j] = centre;
    d.scale[j] = spec.standardize ? spread : 1.0;
    double* out = d.xt.data() + j * n;
    for (std::size_t i = 0; i < n; ++i) out[i] = (col[i] - centre) / d.scale[j];
    if (folded) {
      for (std::size_t i = 0; i < n; ++i) out[i] *= d.lead[i];
    }
  }

  d.vp.assign(p, 1.0);
  if (!spec.penalty_factor.empty()) {
    if (spec.penalty_factor.size() != p) {
      throw Error("a penalised fit takes one penalty factor per column.");
    }
    double sum = 0.0;
    for (std::size_t j = 0; j < p; ++j) {
      const double f = spec.penalty_factor[j];
      if (!(f >= 0.0) || !std::isfinite(f)) {
        throw Error("a penalty factor is negative or not a number.");
      }
      d.vp[j] = f;
      sum += f;
    }
    // glmnet rescales the factors to sum to the column count, so the path a factor of one gives
    // is the path no factor gives.
    if (sum > 0.0) {
      for (std::size_t j = 0; j < p; ++j) d.vp[j] *= static_cast<double>(p) / sum;
    }
  }
  return d;
}

// The state a warm start carries from one penalty to the next: the coefficients on the
// standardised scale, which columns the descent has been offered, and which have ever left zero.
// Both sets only grow along the path, so a column offered at one penalty is offered at every
// penalty below it.
struct Coefs {
  double a0 = 0.0;
  std::vector<double> b;
  std::vector<std::uint8_t> ever;
  std::vector<std::size_t> active;
  std::vector<std::uint8_t> offered;
  std::vector<std::size_t> candidates;

  void offer(std::size_t j) {
    if (offered[j]) return;
    offered[j] = 1;
    candidates.push_back(j);
  }
};

// Anderson extrapolation of the cycle over the columns that have left zero (Bertrand and Massias,
// 2021). A cyclic descent over correlated columns converges along a few slow directions, and the
// last few iterates of the cycle say what they are: the affine combination of those iterates whose
// successive moves come closest to cancelling is where the cycle is heading. It is taken only where
// the penalised quadratic is lower than at the cycle's own iterate, so the objective still only
// falls, and the descent still stops only on a cycle that moved nothing.
struct Extrapolation {
  static constexpr std::size_t depth = 5;
  std::size_t m = 0;       // the intercept, then the columns that have left zero
  std::size_t filled = 0;  // iterates held
  std::vector<double> iterates;  // [depth + 1, m]
  std::vector<double> target;    // the extrapolated iterate
  std::vector<double> move;      // the move to it, on the cases

  void reset(std::size_t size) {
    m = size;
    filled = 0;
    iterates.resize((depth + 1) * m);
  }

  void record(const Coefs& fit) {
    double* at = iterates.data() + filled * m;
    at[0] = fit.a0;
    for (std::size_t k = 0; k < fit.active.size(); ++k) at[k + 1] = fit.b[fit.active[k]];
    ++filled;
  }
};

// The extrapolated point from the iterates held, and whether it was taken.
bool extrapolate(const Design& d, double lambda, double alpha, bool intercept,
                 const std::vector<double>& v, std::vector<double>& r, Coefs& fit,
                 Extrapolation& ex) {
  constexpr std::size_t K = Extrapolation::depth;
  const std::size_t m = ex.m;
  const std::size_t n = d.n;
  auto iterate = [&](std::size_t i) { return ex.iterates.data() + i * m; };

  // The weights of the combination are the ones minimising the length of the combined move under
  // weights summing to one: the Gram matrix of the moves against a vector of ones, normalised.
  double gram[K][K];
  for (std::size_t a = 0; a < K; ++a) {
    for (std::size_t b = 0; b <= a; ++b) {
      double s = 0.0;
      const double* a0 = iterate(a);
      const double* a1 = iterate(a + 1);
      const double* b0 = iterate(b);
      const double* b1 = iterate(b + 1);
      for (std::size_t k = 0; k < m; ++k) s += (a1[k] - a0[k]) * (b1[k] - b0[k]);
      gram[a][b] = s;
      gram[b][a] = s;
    }
  }
  double chol[K][K] = {};
  for (std::size_t a = 0; a < K; ++a) {
    for (std::size_t b = 0; b <= a; ++b) {
      double s = gram[a][b];
      for (std::size_t k = 0; k < b; ++k) s -= chol[a][k] * chol[b][k];
      if (a == b) {
        if (!(s > 0.0) || !std::isfinite(s)) return false;
        chol[a][a] = std::sqrt(s);
      } else {
        chol[a][b] = s / chol[b][b];
      }
    }
  }
  double z[K];
  for (std::size_t a = 0; a < K; ++a) {
    double s = 1.0;
    for (std::size_t k = 0; k < a; ++k) s -= chol[a][k] * z[k];
    z[a] = s / chol[a][a];
  }
  for (std::size_t a = K; a-- > 0;) {
    double s = z[a];
    for (std::size_t k = a + 1; k < K; ++k) s -= chol[k][a] * z[k];
    z[a] = s / chol[a][a];
  }
  double sum = 0.0;
  for (std::size_t a = 0; a < K; ++a) sum += z[a];
  if (!(std::fabs(sum) > 0.0) || !std::isfinite(sum)) return false;

  // The move from the cycle's own iterate to the extrapolated one, on the cases, and what it does to
  // the penalised quadratic: the smooth part falls by the residual's reading of the move less half
  // its curvature, and the penalty is read off the coefficients directly.
  const double* current = iterate(K);
  std::vector<double>& q = ex.move;
  q.assign(n, 0.0);
  double penalty = 0.0;
  std::vector<double>& target = ex.target;
  target.resize(m);
  for (std::size_t k = 0; k < m; ++k) {
    double s = 0.0;
    for (std::size_t a = 0; a < K; ++a) s += z[a] * iterate(a + 1)[k];
    target[k] = s / sum;
  }
  const double shift = intercept ? target[0] - current[0] : 0.0;
  if (shift != 0.0) add_scaled(q.data(), d.lead.data(), shift, n);
  for (std::size_t k = 0; k + 1 < m; ++k) {
    const std::size_t j = fit.active[k];
    const double from = current[k + 1];
    const double to = target[k + 1];
    if (to == from) continue;
    add_scaled(q.data(), d.column(j), to - from, n);
    const double pen = lambda * d.vp[j];
    penalty += pen * (alpha * (std::fabs(to) - std::fabs(from)) +
                      0.5 * (1.0 - alpha) * (to * to - from * from));
  }
  const double curve = d.folded ? dot(q.data(), q.data(), n) : dot3(v.data(), q.data(), q.data(), n);
  const double change = -dot(q.data(), r.data(), n) + 0.5 * curve + penalty;
  if (!(change < 0.0)) return false;

  if (intercept) fit.a0 = target[0];
  for (std::size_t k = 0; k + 1 < m; ++k) fit.b[fit.active[k]] = target[k + 1];
  if (d.folded) {
    add_scaled(r.data(), q.data(), -1.0, n);
  } else {
    add_scaled3(r.data(), v.data(), q.data(), -1.0, n);
  }
  return true;
}

// One weighted least squares elastic net over the columns the caller has offered, by cyclic
// coordinate descent. `v` is the weight of the quadratic and `r` its residual already multiplied
// by that weight, so a case the family has pinned carries a gradient and no curvature, which is
// what glmnet does with a fitted probability at zero or one. A folded design carries its weight in
// its columns, and `v` is not read. It returns whether the descent settled before the pass budget
// ran out.
bool quadratic_solve(const Design& d, double lambda, double alpha, bool intercept, double thresh,
                     int& budget, const std::vector<double>& v, const std::vector<double>& xv,
                     std::vector<double>& r, Coefs& fit, Extrapolation& ex) {
  const std::size_t n = d.n;
  const double* lead = d.lead.data();
  const double* lead_v = d.folded ? lead : v.data();
  const double sv = dot(lead, lead_v, n);

  auto step = [&](std::size_t j, double& dlx) {
    if (!(xv[j] > 0.0)) return;
    const double* col = d.column(j);
    const double u = dot(r.data(), col, n) + xv[j] * fit.b[j];
    const double pen = lambda * d.vp[j];
    const double l1 = pen * alpha;
    double bj = 0.0;
    if (std::fabs(u) > l1) {
      bj = std::copysign(std::fabs(u) - l1, u) / (xv[j] + pen * (1.0 - alpha));
    }
    const double delta = bj - fit.b[j];
    if (delta == 0.0) return;
    fit.b[j] = bj;
    if (d.folded) {
      add_scaled(r.data(), col, -delta, n);
    } else {
      add_scaled3(r.data(), v.data(), col, -delta, n);
    }
    dlx = std::max(dlx, xv[j] * delta * delta);
    if (!fit.ever[j]) {
      fit.ever[j] = 1;
      fit.active.push_back(j);
    }
  };

  auto shift = [&](double& dlx) {
    if (!intercept || !(sv > 0.0)) return;
    const double delta = dot(lead, r.data(), n) / sv;
    if (delta == 0.0) return;
    fit.a0 += delta;
    add_scaled(r.data(), lead_v, -delta, n);
    dlx = std::max(dlx, sv * delta * delta);
  };

  for (;;) {
    double dlx = 0.0;
    for (std::size_t k = 0; k < fit.candidates.size(); ++k) step(fit.candidates[k], dlx);
    shift(dlx);
    if (--budget < 0) return false;
    if (dlx < thresh) break;
    // The columns that have left zero are then cycled on their own until they settle, and the
    // offered set is swept again only to see whether a column outside them has started to move.
    // The set is fixed inside this cycle, so the iterates the extrapolation reads are over one set.
    ex.reset(fit.active.size() + 1);
    ex.record(fit);
    for (;;) {
      double inner = 0.0;
      for (std::size_t k = 0; k < fit.active.size(); ++k) step(fit.active[k], inner);
      shift(inner);
      if (--budget < 0) return false;
      if (inner < thresh) break;
      ex.record(fit);
      if (ex.filled == Extrapolation::depth + 1) {
        extrapolate(d, lambda, alpha, intercept && sv > 0.0, v, r, fit, ex);
        ex.reset(ex.m);
        ex.record(fit);
      }
    }
  }
  return true;
}

// The linear predictor over the design's own columns, so on a folded design it is the predictor
// times the root of each case weight.
void linear_predictor(const Design& d, const Coefs& fit, std::vector<double>& eta) {
  const std::size_t n = d.n;
  eta.resize(n);
  for (std::size_t i = 0; i < n; ++i) eta[i] = fit.a0 * d.lead[i];
  for (std::size_t k = 0; k < fit.active.size(); ++k) {
    const std::size_t j = fit.active[k];
    const double bj = fit.b[j];
    if (bj == 0.0) continue;
    add_scaled(eta.data(), d.column(j), bj, n);
  }
}

// The coefficients on the scale the columns were handed over in, from the standardised ones.
void unstandardise(const Design& d, const Coefs& fit, double y_centre, double y_scale, double& a0,
                   double* beta) {
  double shift = y_centre;
  for (std::size_t j = 0; j < d.p; ++j) {
    const double bj = fit.b[j] * y_scale / d.scale[j];
    beta[j] = bj;
    shift -= bj * d.centre[j];
  }
  a0 = fit.a0 * y_scale + shift;
}

}  // namespace

Family family_from_name(const std::string& name) {
  if (name == "gaussian") return Family::gaussian;
  if (name == "binomial") return Family::binomial;
  throw Error("a penalised fit knows the gaussian and the binomial family, not '" + name + "'.");
}

const char* family_name(Family f) {
  return f == Family::gaussian ? "gaussian" : "binomial";
}

PenaltyPath penalised_path(const double* x, const double* y, const double* w, std::size_t n,
                           std::size_t p, Family family, const PenaltySpec& spec) {
  if (n == 0 || p == 0) throw Error("a penalised fit needs at least one unit and one column.");
  if (!(spec.alpha >= 0.0 && spec.alpha <= 1.0)) {
    throw Error("the elastic net's mixing is between zero and one.");
  }
  // The response's own values decide whether there is anything to fit, not a weighted mean of
  // them: the weights sum to one only to the last bit, so a response holding one value has a
  // spread of 1e-32 rather than of zero and a binomial one holding a single outcome has a mean
  // just under one. Either would be fitted, on nothing.
  double low = y[0], high = y[0];
  for (std::size_t i = 0; i < n; ++i) {
    if (!std::isfinite(y[i])) {
      throw Error("a penalised fit was handed a response that is not a number.");
    }
    low = std::min(low, y[i]);
    high = std::max(high, y[i]);
  }
  if (low == high) {
    throw Error(family == Family::binomial
                    ? "a binomial penalised fit was handed a response holding one outcome."
                    : "a penalised fit was handed a response holding one value, which has "
                      "nothing to penalise against.");
  }

  const Design d = build_design(x, w, n, p, spec, family == Family::gaussian);
  const std::size_t max_active = spec.max_active == 0 ? p : spec.max_active;

  // The response on the scale the descent reads it. A Gaussian response is centred and scaled the
  // way a column is, which is what puts the reported penalties on the response's own scale; a
  // binomial one is the outcome itself.
  double y_centre = 0.0, y_scale = 1.0;
  std::vector<double> yt(n);
  double null_deviance = 0.0;
  double dev_null = 0.0;
  double pbar = 0.0;
  if (family == Family::gaussian) {
    if (spec.intercept) {
      for (std::size_t i = 0; i < n; ++i) y_centre += d.w[i] * y[i];
    }
    double spread = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double z = y[i] - y_centre;
      spread += d.w[i] * z * z;
    }
    y_scale = std::sqrt(spread);
    if (!(y_scale > 0.0)) {
      throw Error("a penalised fit was handed a response holding one value, which has nothing to "
                  "penalise against.");
    }
    for (std::size_t i = 0; i < n; ++i) yt[i] = (y[i] - y_centre) / y_scale;
    null_deviance = spread;
    dev_null = 1.0;
  } else {
    for (std::size_t i = 0; i < n; ++i) {
      if (y[i] != 0.0 && y[i] != 1.0) {
        throw Error("a binomial penalised fit takes a response holding zero and one.");
      }
      yt[i] = y[i];
      pbar += d.w[i] * y[i];
    }
    if (low != 0.0 || high != 1.0 || !(pbar > 0.0) || !(pbar < 1.0)) {
      throw Error("a binomial penalised fit was handed a response holding one outcome.");
    }
    const double eta0 = std::log(pbar / (1.0 - pbar));
    for (std::size_t i = 0; i < n; ++i) {
      dev_null += d.w[i] * (yt[i] * eta0 - log1pexp(eta0));
    }
    dev_null *= -2.0;
    null_deviance = dev_null;
  }

  std::vector<double> v(n), r(n), xv(p, 0.0), eta(n, 0.0), grad(p, 0.0);
  // The coefficients a reweighted least squares step started from, which is what it is judged on.
  // A column outside the active set is at zero and has never been written here, so a column that
  // leaves zero inside a step is read against the zero it started at.
  std::vector<double> started(p, 0.0);
  Coefs fit;
  Extrapolation ex;
  fit.b.assign(p, 0.0);
  fit.ever.assign(p, 0);
  fit.offered.assign(p, 0);
  // A Gaussian fit's residual is read on the folded scale, the root of each case weight times the
  // response less the predictor, which is what the folded columns read against.
  std::vector<double> yw;
  if (family == Family::gaussian) {
    yw.resize(n);
    for (std::size_t i = 0; i < n; ++i) {
      yw[i] = d.lead[i] * yt[i];
      r[i] = yw[i];
    }
    // The curvature of a Gaussian fit does not move along the path, so it is read once.
    for (std::size_t j = 0; j < p; ++j) {
      if (!d.usable[j]) continue;
      xv[j] = dot(d.column(j), d.column(j), n);
    }
  } else {
    if (spec.intercept) fit.a0 = std::log(pbar / (1.0 - pbar));
    for (std::size_t i = 0; i < n; ++i) {
      v[i] = d.w[i] * pbar * (1.0 - pbar);
      r[i] = d.w[i] * (yt[i] - pbar);
    }
  }

  auto gradient = [&]() {
    for (std::size_t j = 0; j < p; ++j) {
      if (!d.usable[j]) continue;
      grad[j] = dot(r.data(), d.column(j), n);
    }
  };

  // The smallest penalty that leaves every coefficient at zero, from the gradient at the null
  // model. The floor under the mixing is glmnet's, and it is what gives a ridge a finite start.
  gradient();
  double lambda_max = 0.0;
  for (std::size_t j = 0; j < p; ++j) {
    if (!d.usable[j] || !(d.vp[j] > 0.0)) continue;
    lambda_max = std::max(lambda_max, std::fabs(grad[j]) / d.vp[j]);
  }
  lambda_max /= std::max(spec.alpha, kAlphaFloor);

  std::vector<double> path;
  const bool derived = spec.lambda.empty();
  if (derived) {
    if (spec.n_lambda < 1) throw Error("a penalised path holds at least one penalty.");
    double ratio = spec.lambda_min_ratio;
    if (!(ratio > 0.0)) ratio = n > p ? 1e-4 : 1e-2;
    if (!(ratio < 1.0)) throw Error("a penalty path's smallest ratio is below one.");
    path.resize(static_cast<std::size_t>(spec.n_lambda));
    const double step = spec.n_lambda > 1
                            ? std::pow(ratio, 1.0 / static_cast<double>(spec.n_lambda - 1))
                            : 1.0;
    double current = lambda_max;
    for (std::size_t k = 0; k < path.size(); ++k) {
      path[k] = current;
      current *= step;
    }
  } else {
    path = spec.lambda;
    std::sort(path.begin(), path.end(), std::greater<double>());
    for (std::size_t k = 0; k < path.size(); ++k) path[k] /= y_scale;
  }

  PenaltyPath out;
  out.n_column = p;
  out.null_deviance = null_deviance;
  out.family = family;
  std::vector<double> beta(p, 0.0);
  double previous_ratio = -std::numeric_limits<double>::infinity();
  double previous_lambda = lambda_max;
  int budget = spec.max_pass;
  // The descent stops on a move small against the deviance it is fitting, which is glmnet's
  // reading of the same number: a Gaussian response is scaled to a null deviance of one, so the
  // two families mean the same thing by it.
  const double tolerance = spec.thresh * dev_null;

  // The curvature of a binomial fit moves with every reweighting, and is read only over the
  // columns the descent may move: reading it over every column instead is where an unrestricted
  // descent spends its time.
  auto curvature = [&]() {
    for (std::size_t k = 0; k < fit.candidates.size(); ++k) {
      const std::size_t j = fit.candidates[k];
      xv[j] = dot3(v.data(), d.column(j), d.column(j), n);
    }
  };

  auto reweight = [&]() {
    linear_predictor(d, fit, eta);
    for (std::size_t i = 0; i < n; ++i) {
      const double e = std::min(std::max(eta[i], -kEtaClamp), kEtaClamp);
      double prob = 1.0 / (1.0 + std::exp(-e));
      double curve = prob * (1.0 - prob);
      if (prob < kProbFloor) {
        prob = 0.0;
        curve = 0.0;
      } else if (prob > 1.0 - kProbFloor) {
        prob = 1.0;
        curve = 0.0;
      }
      v[i] = d.w[i] * curve;
      r[i] = d.w[i] * (yt[i] - prob);
    }
  };

  if (family == Family::binomial) reweight();

  for (std::size_t k = 0; k < path.size(); ++k) {
    const double lambda = path[k];

    // Tibshirani's sequential strong rule: a column whose gradient at the penalty just fitted is
    // further than one step of the path from the threshold is offered to the descent, and the
    // rest are left out. It is a screen rather than a decision, and whatever it discards is
    // tested against the optimality condition below and taken back where it was wrong.
    //
    // The screen only ever adds, so a column already offered stays offered and is not tested
    // again. What that buys is the gradient: the only columns the rule reads are the ones the
    // optimality test at the penalty just fitted read as well, so `grad` is current where it is
    // needed and the path costs one sweep over the columns rather than two.
    const double bound = spec.alpha * (2.0 * lambda - previous_lambda);
    const std::size_t offered_before = fit.candidates.size();
    for (std::size_t j = 0; j < p; ++j) {
      if (!d.usable[j] || fit.offered[j]) continue;
      if (!(d.vp[j] > 0.0) || std::fabs(grad[j]) > d.vp[j] * bound) fit.offer(j);
    }
    if (fit.candidates.size() != offered_before) {
      std::sort(fit.candidates.begin(), fit.candidates.end());
    }

    bool settled = true;
    for (;;) {
      if (family == Family::gaussian) {
        settled = quadratic_solve(d, lambda, spec.alpha, spec.intercept, tolerance, budget, v, xv,
                                  r, fit, ex);
      } else {
        // A reweighted least squares is settled when a step of it moves nothing, and what that is
        // read on is the step's own move: the coefficients it started from against the ones it
        // reached, over the columns that have left zero and over the intercept, on the same scale
        // the descent inside it stops at. That is glmnet's own test, and it ends the step on the
        // reweighting the next one would have opened with, so the loop leaves `r` where the
        // optimality test below reads it and needs none of its own afterwards. It takes the same
        // number of steps as reading the move off the first sweep of a further step does --
        // measured, the same to two decimals on every shape tried -- and saves the reweighting
        // that reading needed after the loop, which is two of them at every penalty.
        for (int it = 0;; ++it) {
          const double started_at = fit.a0;
          for (std::size_t k2 = 0; k2 < fit.active.size(); ++k2) {
            started[fit.active[k2]] = fit.b[fit.active[k2]];
          }
          curvature();
          if (!quadratic_solve(d, lambda, spec.alpha, spec.intercept, tolerance, budget, v, xv, r,
                               fit, ex)) {
            settled = false;
            break;
          }
          reweight();
          const double shift = fit.a0 - started_at;
          double moved = total(v.data(), n) * shift * shift;
          for (std::size_t k2 = 0; k2 < fit.active.size(); ++k2) {
            const std::size_t j = fit.active[k2];
            const double delta = fit.b[j] - started[j];
            moved = std::max(moved, xv[j] * delta * delta);
          }
          if (moved < tolerance) break;
          if (it + 1 >= spec.max_irls) {
            settled = false;
            break;
          }
        }
      }
      if (!settled) break;

      // What the screen left out, tested: a column outside the offered set whose gradient is over
      // the threshold is not at zero at the optimum, so it is taken back and the penalty refitted.
      bool recovered = false;
      for (std::size_t j = 0; j < p; ++j) {
        if (!d.usable[j] || fit.offered[j]) continue;
        grad[j] = dot(r.data(), d.column(j), n);
        if (std::fabs(grad[j]) > d.vp[j] * spec.alpha * lambda) {
          fit.offer(j);
          recovered = true;
        }
      }
      if (!recovered) break;
      std::sort(fit.candidates.begin(), fit.candidates.end());
      if (family == Family::gaussian) {
        linear_predictor(d, fit, eta);
        for (std::size_t i = 0; i < n; ++i) r[i] = yw[i] - eta[i];
      }
    }

    // A penalty the fit did not settle at ends the path, and the points before it are the path,
    // which is what glmnet returns for the same event. A fit that does not settle at the first
    // penalty has nothing to return.
    if (!settled) {
      if (out.lambda.empty()) {
        throw Error(family == Family::binomial
                        ? "a binomial penalised fit did not settle at the first penalty of its path."
                        : "a penalised fit did not settle at the first penalty of its path inside "
                          "its pass budget.");
      }
      out.stalled = static_cast<std::int32_t>(k) + 1;
      break;
    }

    // The residual is rebuilt from the coefficients at every penalty rather than carried forward,
    // because a residual updated in place along a hundred warm starts drifts from the one those
    // coefficients imply. A binomial fit leaves the loop above on a reweighting, which is that
    // rebuild; a Gaussian one leaves it on a descent and is rebuilt here, once, which is what the
    // deviance is read off and what the next penalty opens on.
    if (family == Family::gaussian) {
      linear_predictor(d, fit, eta);
      for (std::size_t i = 0; i < n; ++i) r[i] = yw[i] - eta[i];
    }
    std::int32_t nonzero = 0;
    for (std::size_t j = 0; j < p; ++j) {
      if (fit.b[j] != 0.0) ++nonzero;
    }
    if (static_cast<std::size_t>(nonzero) > max_active && k > 0) break;

    double dev = 0.0;
    if (family == Family::gaussian) {
      dev = dot(r.data(), r.data(), n);
    } else {
      for (std::size_t i = 0; i < n; ++i) dev += d.w[i] * (yt[i] * eta[i] - log1pexp(eta[i]));
      dev *= -2.0;
    }
    const double ratio = dev_null > 0.0 ? 1.0 - dev / dev_null : 0.0;

    double a0 = 0.0;
    unstandardise(d, fit, y_centre, y_scale, a0, beta.data());
    out.a0.push_back(a0);
    out.lambda.push_back(lambda * y_scale);
    out.beta.insert(out.beta.end(), beta.begin(), beta.end());
    out.df.push_back(nonzero);
    out.dev_ratio.push_back(ratio);

    if (derived && static_cast<int>(k) + 1 >= spec.min_lambda) {
      if (ratio > spec.dev_max) break;
      // A step that explains almost nothing more ends the path. The Gaussian family reads that
      // share against the deviance explained so far and the binomial one reads it outright, which
      // is the difference glmnet's two solvers carry.
      const double gained = family == Family::gaussian ? spec.fdev * std::fabs(ratio) : spec.fdev;
      if (spec.fdev > 0.0 && ratio - previous_ratio < gained) break;
    }
    previous_ratio = ratio;
    previous_lambda = lambda;
  }
  if (out.lambda.empty()) throw Error("a penalised path fitted no penalty.");
  out.passes = spec.max_pass - std::max(budget, 0);
  return out;
}

void penalised_coef(const PenaltyPath& path, double lambda, double* a0, double* beta) {
  const std::size_t p = path.n_column;
  const std::size_t k = path.lambda.size();
  if (k == 0) throw Error("a penalised path holds no penalty to read a coefficient at.");
  if (k == 1) {
    *a0 = path.a0[0];
    for (std::size_t j = 0; j < p; ++j) beta[j] = path.beta[j];
    return;
  }
  const double at = std::min(std::max(lambda, path.lambda.back()), path.lambda.front());
  // The two points of the path the penalty falls between, and how far it sits from the lower one,
  // which is how glmnet reads a penalty that is not a point of the path it fitted.
  std::size_t right = 1;
  while (right < k - 1 && path.lambda[right] > at) ++right;
  const std::size_t left = right - 1;
  const double span = path.lambda[left] - path.lambda[right];
  double frac = 1.0;
  if (std::fabs(span) > std::numeric_limits<double>::epsilon()) {
    frac = (at - path.lambda[right]) / span;
  }
  *a0 = path.a0[left] * frac + path.a0[right] * (1.0 - frac);
  for (std::size_t j = 0; j < p; ++j) {
    beta[j] = path.beta[left * p + j] * frac + path.beta[right * p + j] * (1.0 - frac);
  }
}

void penalised_predict(const PenaltyPath& path, double lambda, const double* x, std::size_t n,
                       double* out) {
  const std::size_t p = path.n_column;
  double a0 = 0.0;
  std::vector<double> beta(p, 0.0);
  penalised_coef(path, lambda, &a0, beta.data());
  for (std::size_t i = 0; i < n; ++i) out[i] = a0;
  for (std::size_t j = 0; j < p; ++j) {
    if (beta[j] == 0.0) continue;
    add_scaled(out, x + j * n, beta[j], n);
  }
  if (path.family == Family::binomial) {
    for (std::size_t i = 0; i < n; ++i) {
      const double e = std::min(std::max(out[i], -kEtaClamp), kEtaClamp);
      out[i] = 1.0 / (1.0 + std::exp(-e));
    }
  }
}

PenaltyCV penalised_cv(const double* x, const double* y, const double* w, std::size_t n,
                       std::size_t p, Family family, const PenaltySpec& spec,
                       const std::int32_t* fold, std::int32_t n_fold) {
  if (n_fold < 2) throw Error("a cross-validated penalty needs at least two folds.");
  const std::size_t folds = static_cast<std::size_t>(n_fold);

  // Which units each fold holds back, settled before anything is fitted so the fits are
  // independent of each other and of the order they are run in.
  struct Fold {
    std::vector<std::size_t> in, held_out;
    PenaltyPath fit;
    std::exception_ptr failure;
  };
  std::vector<Fold> held(folds);
  for (std::size_t i = 0; i < n; ++i) {
    const std::int32_t f = fold[i];
    if (f < 0 || static_cast<std::size_t>(f) >= folds) {
      throw Error("a fold index of a cross-validated penalty is outside the folds it declares.");
    }
    for (std::size_t g = 0; g < folds; ++g) {
      (static_cast<std::size_t>(f) == g ? held[g].held_out : held[g].in).push_back(i);
    }
  }
  for (std::size_t g = 0; g < folds; ++g) {
    if (held[g].in.empty() || held[g].held_out.empty()) {
      throw Error("a fold of a cross-validated penalty holds every unit or none of them.");
    }
  }

  // Each fold is fitted the way the whole-unit path was, along a path of its own, and is then
  // read at the whole-unit path's penalties. A fold holds different units, so the largest penalty
  // that leaves every coefficient at zero is a different number there; aligning the folds on the
  // penalty rather than on the point of the path is what keeps the held-out deviance a function
  // of the penalty, and it is what glmnet aligns on.
  auto fit_fold = [&](std::size_t g) {
    Fold& one = held[g];
    try {
      const std::size_t nt = one.in.size();
      std::vector<double> train_x(nt * p), train_y(nt), train_w(nt);
      for (std::size_t j = 0; j < p; ++j) {
        for (std::size_t a = 0; a < nt; ++a) train_x[a + j * nt] = x[one.in[a] + j * n];
      }
      for (std::size_t a = 0; a < nt; ++a) {
        train_y[a] = y[one.in[a]];
        train_w[a] = w == nullptr ? 1.0 : w[one.in[a]];
      }
      one.fit = penalised_path(train_x.data(), train_y.data(), train_w.data(), nt, p, family,
                               spec);
    } catch (...) {
      one.failure = std::current_exception();
    }
  };

  // The whole-unit fit and the folds are one independent fit each, so they run at once where the
  // caller asked for it. Nothing is shared but the design they read, and what each returns is a
  // function of its own units alone, so a run on many threads returns the numbers a run on one
  // returns.
  PenaltyCV out;
  const int workers = std::max(1, spec.threads);
  if (workers <= 1) {
    out.path = penalised_path(x, y, w, n, p, family, spec);
    for (std::size_t g = 0; g < folds; ++g) fit_fold(g);
  } else {
    std::atomic<std::size_t> next{0};
    auto take = [&]() {
      for (;;) {
        const std::size_t g = next.fetch_add(1);
        if (g >= folds) return;
        fit_fold(g);
      }
    };
    std::vector<std::thread> pool;
    const std::size_t spare = std::min<std::size_t>(static_cast<std::size_t>(workers) - 1, folds);
    pool.reserve(spare);
    // A machine that will not give another thread is a reason to run on fewer, not to fail: what
    // is left goes to the threads that did start and to this one.
    for (std::size_t t = 0; t < spare; ++t) {
      try {
        pool.emplace_back(take);
      } catch (const std::system_error&) {
        break;
      }
    }
    std::exception_ptr failure;
    try {
      out.path = penalised_path(x, y, w, n, p, family, spec);
    } catch (...) {
      failure = std::current_exception();
    }
    take();
    for (std::thread& t : pool) t.join();
    if (failure) std::rethrow_exception(failure);
  }
  for (std::size_t g = 0; g < folds; ++g) {
    if (held[g].failure) std::rethrow_exception(held[g].failure);
  }
  out.fold_stalled.resize(folds);
  for (std::size_t g = 0; g < folds; ++g) out.fold_stalled[g] = held[g].fit.stalled;
  const std::size_t k = out.path.lambda.size();

  std::vector<double> fold_sum(folds, 0.0);
  std::vector<double> fold_mean(folds * k, 0.0);
  std::vector<double> test_x, predicted;
  for (std::size_t g = 0; g < folds; ++g) {
    const Fold& one = held[g];
    const std::size_t nh = one.held_out.size();
    test_x.assign(nh * p, 0.0);
    for (std::size_t j = 0; j < p; ++j) {
      for (std::size_t a = 0; a < nh; ++a) test_x[a + j * nh] = x[one.held_out[a] + j * n];
    }
    double weight = 0.0;
    for (std::size_t a = 0; a < nh; ++a) weight += w == nullptr ? 1.0 : w[one.held_out[a]];
    fold_sum[g] = weight;
    predicted.resize(nh);
    for (std::size_t l = 0; l < k; ++l) {
      penalised_predict(one.fit, out.path.lambda[l], test_x.data(), nh, predicted.data());
      double score = 0.0;
      for (std::size_t a = 0; a < nh; ++a) {
        const std::size_t i = one.held_out[a];
        const double wi = w == nullptr ? 1.0 : w[i];
        double raw;
        if (family == Family::gaussian) {
          const double e = y[i] - predicted[a];
          raw = e * e;
        } else {
          const double q = std::min(std::max(predicted[a], kCVProbFloor), 1.0 - kCVProbFloor);
          raw = -2.0 * (y[i] * std::log(q) + (1.0 - y[i]) * std::log(1.0 - q));
        }
        score += wi * raw;
      }
      fold_mean[g + l * folds] = weight > 0.0 ? score / weight : 0.0;
    }
  }

  // The held-out deviance is summarised over the folds rather than over the units: a fold is one
  // reading of the penalty, and its spread over the folds is what the standard error is of.
  double total = 0.0;
  for (std::size_t g = 0; g < folds; ++g) total += fold_sum[g];
  out.cv_mean.assign(k, 0.0);
  out.cv_sd.assign(k, 0.0);
  for (std::size_t l = 0; l < k; ++l) {
    double mean = 0.0;
    for (std::size_t g = 0; g < folds; ++g) mean += fold_sum[g] * fold_mean[g + l * folds];
    mean /= total;
    double spread = 0.0;
    for (std::size_t g = 0; g < folds; ++g) {
      const double e = fold_mean[g + l * folds] - mean;
      spread += fold_sum[g] * e * e;
    }
    out.cv_mean[l] = mean;
    out.cv_sd[l] = std::sqrt(spread / total / static_cast<double>(folds - 1));
  }

  out.index_min = 0;
  for (std::size_t l = 1; l < k; ++l) {
    if (out.cv_mean[l] < out.cv_mean[out.index_min]) out.index_min = l;
  }
  const double within = out.cv_mean[out.index_min] + out.cv_sd[out.index_min];
  out.index_1se = out.index_min;
  for (std::size_t l = 0; l < k; ++l) {
    if (out.cv_mean[l] <= within) {
      out.index_1se = l;
      break;
    }
  }
  return out;
}

}  // namespace timesift
