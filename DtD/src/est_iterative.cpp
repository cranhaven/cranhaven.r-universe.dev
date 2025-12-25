#include "bs.h"
#include "utils.h"

inline bool almost_eq(double a, double b, double eps){
  double abs_a = std::abs(a);
  return (abs_a < eps) ?
    std::abs(a - b) < eps :
    std::abs(a - b) / (abs_a + 1e-8) < eps;
}

est_result est_iterative(
    const arma::vec &S, const arma::vec &D, const arma::vec &T,
    const arma::vec &r, const arma::vec &time, double vol_start,
    const double tol, const double eps){
  est_result out;

  // assume that all have equal length
  const arma::uword n = S.n_elem;

  // find solution
  const arma::vec dts = diff(time), sqrt_dts = arma::sqrt(dts);
  double &mu = out.mu, &vol = out.vol, sum_dts = arma::sum(dts);
  vol = vol_start;
  const unsigned int it_max = 10000L;
  arma::vec vol_vec(n);

  unsigned int &i = out.n_iter;
  for(i = 0; i < it_max; ++i){
    double vol_old = vol, mu_old = mu;
    vol_vec.fill(vol);
    arma::vec V = get_underlying_cpp(S, D, T, r, vol_vec, tol);

    /* compute vol and mean */
    /* log n save so we do not need to do it again in the next loop */
    V = arma::log(V);
    double mu_tilde = (*(V.end() - 1L) - V[0]) / sum_dts;

    const double *sqrt_dt = sqrt_dts.begin();
    double sse = 0, log_prev, log_new = V[0];
    for(auto V_i = V.begin() + 1; V_i != V.end(); ++V_i, ++sqrt_dt){
      log_prev = log_new;
      log_new = *V_i;
      double log_diff = log_new - log_prev;

      double delt = log_diff / *sqrt_dt - *sqrt_dt * mu_tilde;
      sse += delt * delt;
    }

    vol = std::sqrt(sse / (n - 1.)); /* recall this is the regular division by
                                        n as we only have n - 1 returns */
    mu = mu_tilde + vol * vol / 2.;

    // check if converged
    if(i > 0L and almost_eq(mu_old, mu, eps) and
       almost_eq(vol_old, vol, eps)){
      out.success = true;
      ++i;
      break;
    }
  }

  return out;
}
