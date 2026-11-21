#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>
#include <cstring>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;

typedef long double accumulator;

namespace {

const double gauss_weight[30] = {
  0x1.5edf601e2dbfdp-3, 0x1.716b7b5794c18p-2, 0x1.df24d499545ddp-2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0x1.8275d9dea6d87p-5, 0x1.b60602bce6178p-4, 0x1.47d7258f22d95p-3, 0x1.a0163e6b1ab71p-3, 0x1.de3155c256ab1p-3,
  0x1.fe40ce6d4f02ap-3, 0.0, 0.0, 0.0, 0.0,
  0x1.209680274e8bp-6, 0x1.4c9b5ea53b67fp-5, 0x1.00b467df7e474p-4, 0x1.5519fe196e24ap-4, 0x1.a1817a317a81ep-4,
  0x1.e41ff31573b47p-4, 0x1.0db2c5db26dfep-3, 0x1.230348f34a537p-3, 0x1.31819b52c5991p-3, 0x1.38d6c490a3372p-3};

const double gauss_node[30] = {
  -0x1.dd6ca4e80a01fp-1, -0x1.528a09655c96p-1, -0x1.e8b12d03675c8p-3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  -0x1.f68f1d8e42e8p-1, -0x1.cee874ffb88b5p-1, -0x1.8a30aeed88f39p-1, -0x1.2cb4f05c077f6p-1, -0x1.78a8d20a8b19dp-2,
  -0x1.007a5f8f630eep-3, 0.0, 0.0, 0.0, 0.0,
  -0x1.fc7b5a0c71cep-1, -0x1.ed8dba7bd769fp-1, -0x1.d31064173fd92p-1, -0x1.ada0bd5efd6e7p-1, -0x1.7e1f37346a54ep-1,
  -0x1.45a8d3fa710dbp-1, -0x1.05905c13f7ff7p-1, -0x1.7eaccf15652c5p-2, -0x1.d281636928bc1p-3, -0x1.3973df98b86afp-4};

const double mvphi_coef[25] = {
  0x1.3864ac8d7b497p-1, -0x1.bd4707d501931p-2, 0x1.692ad08a7a91fp-3, -0x1.f157c434fcdcp-5, 0x1.2231cd452f222p-6,
  -0x1.1b305a717084bp-8, 0x1.bfdb058705335p-11, -0x1.0aa9de94fb5fcp-13, 0x1.796d0559e65adp-17, 0x1.50265876ea5d9p-22,
  -0x1.22f8a211f43d6p-22, 0x1.0808bf449e399p-25, 0x1.59be860e2acc3p-29, -0x1.1ad481a41831bp-30, 0x1.076402cfe46d4p-35,
  0x1.ca4ed4905eb2p-36, -0x1.72d21ce0d179p-39, -0x1.6a345ca8de92bp-41, 0x1.fa768e904cec3p-44, 0x1.375101836bfp-46,
  -0x1.3342caec7a748p-48, -0x1.3a62b2cad44c6p-51, 0x1.6dee887117d4cp-53, 0x1.8188b19884aa9p-56, -0x1.b361cd2085475p-58};

const double two_pi = 0x1.921fb54442d18p+2;
const double four_pi = 0x1.921fb54442d18p+3;
const double sqrt_two = 0x1.6a09e667f3bcdp+0;
const double single_three_tenths = 0x1.333334p-2;
const double single_point_925 = 0x1.d9999ap-1;
const double cell_sign_j[3] = {1, -1, 1};
const double cell_sign_k[3] = {1, -1, -1};
const double pnorm_both_symmetric = 8.2924;

// Lower-tail mass of mvphi(): mvphi(z) is this value, or one minus it when z > 0.
double mvphi_tail(double z) {
  const double xa = std::fabs(z) / sqrt_two;
  double p = 0;
  if (xa <= 100) {
    const double t = std::fma(xa, 8.0, -30.0) / std::fma(xa, 4.0, 15.0);
    double bm = 0, b = 0, bp = 0;
    for (int i = 24; i >= 0; --i) {
      bp = b;
      b = bm;
      bm = std::fma(t, b, -bp) + mvphi_coef[i];
    }
    p = (std::exp(-(xa * xa)) * (bm - bp)) * 0.25;
  }
  return p;
}

struct genz_nodes {
  bool ready = false, low = true;
  double rho = 0, asr = 0, as = 0, a = 0;
  int lg = 10;
  const double *w = gauss_weight;
  double sn[2][10], den[2][10], aw[10], xs[2][10], rs[2][10];
};

void prepare_nodes(double rho, genz_nodes &g) {
  int ng = 2, lg = 10;
  if (rho < single_three_tenths) {
    ng = 0;
    lg = 3;
  } else if (rho < 0.75) {
    ng = 1;
    lg = 6;
  }
  const double *x = gauss_node + 10 * ng, *w = gauss_weight + 10 * ng;
  g.ready = true;
  g.rho = rho;
  g.lg = lg;
  g.w = w;
  g.low = rho < single_point_925;
  if (g.low) {
    const double asr = std::asin(rho);
    g.asr = asr;
    for (int i = 0; i < lg; ++i) {
      double sn = std::sin(((x[i] + 1) * asr) * 0.5);
      g.sn[0][i] = sn;
      g.den[0][i] = std::fma(-sn, sn, 1.0);
      sn = std::sin(((1 - x[i]) * asr) * 0.5);
      g.sn[1][i] = sn;
      g.den[1][i] = std::fma(-sn, sn, 1.0);
    }
    return;
  }
  const double as = (1 - rho) * (1 + rho);
  const double a = std::sqrt(as), half = a * 0.5;
  g.as = as;
  g.a = a;
  for (int i = 0; i < 10; ++i) {
    g.aw[i] = half * w[i];
    double xs = (x[i] + 1) * half;
    xs = xs * xs;
    g.xs[0][i] = xs;
    g.rs[0][i] = std::sqrt(1 - xs);
    const double om = 1 - x[i];
    xs = ((om * om) * as) * 0.25;
    g.xs[1][i] = xs;
    g.rs[1][i] = std::sqrt(1 - xs);
  }
}

bool same_bits(double a, double b) {
  return std::memcmp(&a, &b, sizeof(double)) == 0;
}

void cell_probabilities(double t, const genz_nodes &g, double *p) {
  const double tail = mvphi_tail(t);
  const double upper = t > 0 ? 1 - tail : tail, lower = -t > 0 ? 1 - tail : tail;
  const double *w = g.w;
  const double hk = t * t;
  if (g.low) {
    const double hs = std::fma(t, t, hk) * 0.5;
    double bvn = 0;
    for (int i = 0; i < g.lg; ++i) {
      bvn = std::fma(w[i], std::exp(std::fma(g.sn[0][i], hk, -hs) / g.den[0][i]), bvn);
      bvn = std::fma(w[i], std::exp(std::fma(g.sn[1][i], hk, -hs) / g.den[1][i]), bvn);
    }
    const double quadrature = (g.asr * bvn) / four_pi;
    p[0] = std::fma(upper, upper, quadrature);
    p[1] = std::fma(lower, lower, quadrature);
    p[2] = std::fma(upper, lower, -quadrature);
    return;
  }
  const double as = g.as;
  const double c = (4 - hk) * 0.125, d = (12 - hk) * 0.0625;
  const double e1 = std::exp(-(hk * 0.5));
  double bvn = (1 + (as * c) / 3 + (((c * d) * as) * as) / 5) * (e1 * g.a);
  for (int i = 0; i < 10; ++i) {
    const double aw = g.aw[i];
    double xs = g.xs[0][i], rs = g.rs[0][i];
    const double e = std::exp(-(hk / (rs + 1))) / rs;
    bvn = std::fma(aw, std::fma(-e1, std::fma(c * xs, std::fma(d, xs, 1.0), 1.0), e), bvn);
    xs = g.xs[1][i];
    rs = g.rs[1][i];
    const double e4 = std::exp(-(((1 - rs) * hk) / ((rs + 1) + (rs + 1)))) / rs;
    bvn = std::fma(aw * e1, e4 - std::fma(c * xs, std::fma(d, xs, 1.0), 1.0), bvn);
  }
  const double series = -(bvn / two_pi);
  p[0] = series + upper;
  p[1] = series + lower;
  p[2] = 0.0 - series;
}

bool needs_fallback(double ans, double upper) {
  return !std::isfinite(ans) || ans <= 0 || ans > upper * (1 + 1e-10) || (ans < 1e-10 && upper > 0);
}

}

// Cells 11, 00 and 10 use (a, b, r) = (t, t, rho), (-t, -t, rho) and (t, -t, -rho).
// [[Rcpp::export(rng = false)]]
SEXP cimehr_pairwise_evaluate(NumericVector par, NumericVector mu, NumericVector s2, NumericMatrix XO,
                              NumericMatrix ZO, NumericMatrix counts, LogicalVector singleton,
                              NumericVector case_weights, NumericVector s, bool include_delta, bool scores,
                              Function scalar_bvn, CharacterVector nm) {
  const int nr = XO.nrow(), pa = XO.ncol(), q = ZO.ncol();
  const R_xlen_t n = nr;
  const int iv = pa + (include_delta ? q : 0), np = iv + q;
  const double *xo = XO.begin(), *zo = ZO.begin(), *cnt = counts.begin();
  const double *mup = mu.begin(), *s2p = s2.begin(), *cw = case_weights.begin(), *sp = s.begin();
  const int *single = singleton.begin();
  const std::vector<double> theta(par.begin(), par.end());
  const double *th = theta.data();
  const double neg_log_two_pi = -std::log(2 * M_PI);
  NumericMatrix score(scores ? nr : 0, scores ? np : 0);
  NumericVector loglik(nr);
  double *scp = score.begin(), *llp = loglik.begin();
  std::vector<double> dt(np), drho(np), sc(np);
  std::vector<accumulator> gradient(np, 0.0);
  accumulator total = 0.0;
  genz_nodes nodes;
  for (R_xlen_t i = 0; i < n; ++i) {
    double loading = 0, xb = 0, v = 0;
    if (include_delta)
      for (int k = 0; k < q; ++k) loading = std::fma(zo[i + k * n], th[pa + k], loading);
    for (int j = 0; j < pa; ++j) xb = std::fma(xo[i + j * n], th[j], xb);
    for (int k = 0; k < q; ++k) {
      const double z = zo[i + k * n];
      v = std::fma(z * z, th[iv + k], v);
    }
    const double mui = mup[i], s2i = s2p[i];
    const double mean = xb + loading * mui;
    const double T = (1 + v) + (loading * loading) * s2i;
    if (!std::isfinite(T) || T < 1) return R_NilValue;
    const double invsd = 1 / std::sqrt(T);
    const double tt = mean * invsd;
    const double rho = (T - 1) / T;
    if (!std::isfinite(tt) || rho >= 1) return R_NilValue;
    const bool used[3] = {cnt[i] > 0, cnt[i + n] > 0, cnt[i + 2 * n] > 0};
    const bool paired = used[0] || used[1] || used[2];
    if (!paired && !single[i]) continue;
    std::fill(sc.begin(), sc.end(), 0.0);
    double ll = 0;
    const double ld = R::dnorm(tt, 0.0, 1.0, 1);
    const double ct = (0.5 * tt) / T, T2 = T * T;
    for (int j = 0; j < pa; ++j) {
      dt[j] = invsd * xo[i + j * n] - ct * 0.0;
      drho[j] = 0.0;
    }
    if (include_delta)
      for (int k = 0; k < q; ++k) {
        const double z = zo[i + k * n];
        const double dT = ((2 * loading) * s2i) * z;
        dt[pa + k] = invsd * (mui * z) - ct * dT;
        drho[pa + k] = dT / T2;
      }
    for (int k = 0; k < q; ++k) {
      const double z = zo[i + k * n];
      const double dT = z * z;
      dt[iv + k] = 0.0 - ct * dT;
      drho[iv + k] = dT / T2;
    }
    if (paired) {
      double prob[3];
      if (!nodes.ready || !same_bits(rho, nodes.rho)) prepare_nodes(rho, nodes);
      cell_probabilities(tt, nodes, prob);
      double cdf[2];
      if (std::fabs(tt) < pnorm_both_symmetric) {
        R::pnorm_both(tt, &cdf[0], &cdf[1], 2, 0);
      } else {
        cdf[0] = R::pnorm(tt, 0.0, 1.0, 1, 0);
        cdf[1] = R::pnorm(-tt, 0.0, 1.0, 1, 0);
      }
      const double root = std::sqrt((1 - rho) * (1 + rho));
      double lpos, lneg;
      R::pnorm_both((tt - rho * tt) / root, &lpos, &lneg, 2, 1);
      const double ldpos = ld + lpos, ldneg = ld + lneg;
      const double lr = (neg_log_two_pi - std::log(root)) - (((tt * tt) - ((2 * rho) * tt) * tt) + (tt * tt)) /
        (2 * (root * root));
      for (int c = 0; c < 3; ++c) {
        if (!used[c]) continue;
        const double sj = cell_sign_j[c], sk = cell_sign_k[c];
        double p = prob[c];
        if (needs_fallback(p, std::min(cdf[sj < 0], cdf[sk < 0])))
          p = Rf_asReal(scalar_bvn(sj * tt, sk * tt, (sj * sk) * rho));
        if (!std::isfinite(p) || p <= 0 || p > 1 + 1e-10) return R_NilValue;
        const double lp = std::log(p);
        const double da = std::exp((c == 0 ? ldpos : ldneg) - lp);
        const double db = c == 2 ? std::exp(ldpos - lp) : da;
        const double dc = std::exp(lr - lp);
        const double weight = cnt[i + c * n];
        const double c1 = sj * da + sk * db, c2 = (sj * sk) * dc;
        for (int j = 0; j < np; ++j) {
          const double term = weight * (c1 * dt[j] + c2 * drho[j]);
          if (!std::isfinite(term)) return R_NilValue;
          sc[j] = sc[j] + term;
        }
        ll = ll + weight * lp;
      }
    }
    if (single[i]) {
      const double sg = 2 * sp[i] - 1;
      const double lp = R::pnorm(sg * tt, 0.0, 1.0, 1, 1);
      const double mills = sg * std::exp(ld - lp);
      const double cm = cw[i] * mills;
      for (int j = 0; j < np; ++j) sc[j] = sc[j] + cm * dt[j];
      ll = ll + cw[i] * lp;
    }
    for (int j = 0; j < np; ++j) gradient[j] += sc[j];
    if (scores)
      for (int j = 0; j < np; ++j) scp[i + j * n] = sc[j];
    llp[i] = ll;
    total += ll;
  }
  NumericVector g(np);
  for (int j = 0; j < np; ++j) g[j] = -static_cast<double>(gradient[j]);
  g.attr("names") = nm;
  if (scores) score.attr("dimnames") = List::create(R_NilValue, nm);
  return List::create(_["value"] = -static_cast<double>(total), _["gradient"] = g,
                      _["subject_score"] = scores ? static_cast<SEXP>(score) : R_NilValue,
                      _["subject_loglik"] = loglik);
}
