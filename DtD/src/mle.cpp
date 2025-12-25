#include "optim.h"
#include "bs.h"
#include "utils.h"
#include <math.h>
#include "Brent_fmin.h"

class log_like {
  const arma::uword n;
  const arma::vec &S, &D, &T, &r, dts, log_D, log_dts, sqrt_ts;
  mutable arma::vec vol_vec;

  const double tol,
           sum_dts = arma::sum(dts);

  double compute(const double mu, const double vol,
                 const arma::vec &V) const {
    // compute log likelihood
    double log_like_t1 = 0, log_like_t2 = 0, log_prev,
      log_new = std::log(V[0]);
    const double *dt = dts.begin(), *log_dt = log_dts.begin(),
      vol_sq = vol * vol, *log_D_i = log_D.begin() + 1, *r_i = r.begin() + 1,
      *t_i = T.begin() + 1, *sqrt_t = sqrt_ts.begin() + 1,
      mean_factor = mu - vol_sq / 2.;
    for(auto V_i = V.begin() + 1;
        V_i != V.end();
        ++V_i, ++dt, ++log_dt, ++log_D_i, ++r_i, ++t_i, ++sqrt_t){
      log_prev = log_new;
      log_new = std::log(*V_i);

      const double err = log_new - log_prev - mean_factor * *dt;

      // Notice the t_i instead of dt
      const double log_deriv = R::pnorm(
        (log_new - *log_D_i + (*r_i + vol_sq / 2) * *t_i) /
          (*sqrt_t * vol),
          0, 1, 1, 1);

      // the log(dt) is from the normalization constant
      log_like_t1 -= (err * err/ (vol_sq * *dt) + *log_dt);
      log_like_t2 -= (log_new + log_deriv);
    }

    return - ((double)n - 1.) * std::log(2. * M_PI * vol_sq) / 2. +
      log_like_t1 / 2 + log_like_t2;
  }

public:
  log_like(
    const arma::vec &S, const arma::vec &D, const arma::vec &T,
    const arma::vec &r, const arma::vec time, const double tol):
  n(S.n_elem), S(S), D(D), T(T), r(r), dts(diff(time)), log_D(arma::log(D)),
  log_dts(arma::log(dts)), sqrt_ts(arma::sqrt(T)), vol_vec(n), tol(tol) { }

  double get_mu(const double vol, const double V0, const double Vn) const {
    return (std::log(Vn) - std::log(V0)) / sum_dts + vol * vol / 2.;
  }

  double get_mu(const double vol) const {
    const std::size_t n = S.n_elem;
    const double V_min = std::min(S[0], S[n - 1]),
                 V_max = 100 * std::max(S[0] + D[0], S[n - 1] + D[n - 1]),
                 V_mid =  10 * std::max(S[0] + D[0], S[n - 1] + D[n - 1]);

    auto get_V = [&](const std::size_t i){
      return BS_call_cpp_inv(
        *(S.begin() + i), *(D.begin() + i), *(T.begin() + i),
        *(r.begin() + i), vol, tol, V_min, V_max, V_mid);
    };

    return get_mu(vol, get_V(0L), get_V(n - 1L));
  }

  double compute(const double mu, const double vol) const {
    // find underlying
    vol_vec.fill(vol);
    const arma::vec V = get_underlying_cpp(S, D, T, r, vol_vec, tol);

    return compute(mu, vol, V);
  }

  double compute(const double vol) const {
    // find underlying
    vol_vec.fill(vol);
    const arma::vec V = get_underlying_cpp(S, D, T, r, vol_vec, tol);

    const double mu = get_mu(vol, *V.begin(), *(V.end() - 1L));
    return compute(mu, vol, V);
  }
};

/*
 * Use Nelder-Mead method from `optim`. See
 *    r-source/src/library/stats/R/optim.R
 *    r-source/src/library/stats/src/optim.c#L244
 *    r-source/src/appl/optim.c
 */
static double optimfunc(int n, double *p, void *ex)
{
  log_like* ll = (log_like*) ex;

  /* -1 as minimization is the default */
  return -ll->compute(std::exp(p[0]));
}

static unsigned optimfunc_counter = 0L;

static double optimfunc(double p, void *ex)
{
  ++optimfunc_counter;
  log_like* ll = (log_like*) ex;

  /* -1 as minimization is the default */
  return -ll->compute(std::exp(p));
}

est_result mle(
    const arma::vec &S, const arma::vec &D, const arma::vec &T,
    const arma::vec &r, const arma::vec &time, double vol_start,
    const double tol, const double eps){
  // assign arguments
  double dpar[1] = { std::log(vol_start) },
         *opar = new double[1], val;
  int fail(0L), fncount;
  const double alpha = 1;
  const double beta  = 0.5;
  const double gamm  = 2.0;

  est_result out;
  log_like ll(S, D, T, r, time, tol);
  if(false){
    /* old method */
    optim(1L /* npar */, dpar, opar, &val, optimfunc, &fail, -1e8 /* abstol */,
          eps /* reltol */, (void *) &ll, alpha, beta, gamm, 0L /* trace */,
          &fncount, 10000L /* maxit */);
  } else {
    optimfunc_counter = 0L;
    double const fac = std::exp(5);
    double lb = vol_start / fac,
           ub = vol_start * fac,
          out;

    unsigned const nmax = 1000L;
    unsigned i;
    for(i = 0; i < nmax; ++i){
      double const llb = std::log(lb),
                   lub = std::log(ub);
      out = Brent_fmin(llb, lub, optimfunc, &ll, eps);

      /* check convergence */
      if(std::abs(out - llb) < 1.25 * eps){
        ub = lb;
        lb /= fac;
        continue;
      }
      if(std::abs(out - lub) < 1.25 * eps){
        lb = ub;
        ub *= fac;
        continue;
      }

      /* found interior value */
      break;
    }

    if(i == nmax)
      fail = 1L;

    opar[0] = out;
    fncount = optimfunc_counter;
  }

  out.vol = std::exp(opar[0]);
  out.mu  = ll.get_mu(out.vol);
  delete[] opar;
  out.n_iter = fncount;
  out.success = fail == 0L;

  return out;
}

// [[Rcpp::export]]
double merton_ll_cpp(
    const arma::vec &S, const arma::vec &D, const arma::vec &T,
    const arma::vec &r, const arma::vec &time,
    const double vol, const double mu, const double tol){
  return log_like(S, D, T, r, time, tol).compute(mu, vol);
}
