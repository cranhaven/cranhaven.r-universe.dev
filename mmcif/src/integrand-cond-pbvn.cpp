#include "integrand-cond-pbvn.h"
#include "pbvn.h"
#include <array>
#include "ghq-lp-utils.h"

namespace ghqCpp {
template<bool comp_grad>
cond_pbvn<comp_grad>::cond_pbvn
  (arma::vec const &eta, arma::mat const &Psi, arma::mat const &V):
  eta{eta}, Psi{Psi}, V(V) {
    if(eta.n_elem != 2)
      throw std::invalid_argument("eta.n_elem != 2");
    else if(V.n_rows != 2)
      throw std::invalid_argument("V.n_rows != 2");
  }

template<bool comp_grad>
void cond_pbvn<comp_grad>::eval
  (double const *points, size_t const n_points, double * __restrict__ outs,
   simple_mem_stack<double> &mem) const {
  double * __restrict__ mu{mem.get(2 * n_points)};
  for(size_t i = 0; i < n_points; ++i)
    std::copy(eta.begin(), eta.end(), mu + i * 2);

  for(size_t k = 0; k < 2; ++k)
    for(size_t i = 0; i < n_vars(); ++i)
      for(size_t j = 0; j < n_points; ++j)
        mu[k + j * 2] += V(k, i) * points[j + i * n_points];

  if constexpr (!comp_grad){
    for(size_t j = 0; j < n_points; ++j)
      outs[j] = pbvn(mu + j * 2, Psi.memptr());
    return;
  }

  double gr[6];
  for(size_t j = 0; j < n_points; ++j){
    outs[j] = pbvn_grad<1>(mu + j * 2, Psi.memptr(), gr);

    // handle the derivatives w.r.t. eta and Psi
    double const * const d_eta{gr},
                 * const d_Psi{d_eta + 2};
    for(size_t i = 0; i < 2; ++i)
      outs[j + (i + 1) * n_points] = d_eta[i];
    for(size_t i = 0; i < 4; ++i)
      outs[j + (i + 3 + V.n_elem) * n_points] = d_Psi[i];
  }

  // handle the derivatives w.r.t. V
  double const * const d_eta{outs + n_points};
  double * const d_Vs{outs + n_points * 3};

  for(size_t k = 0; k < 2; ++k)
    for(size_t i = 0; i < n_vars(); ++i)
      for(size_t j = 0; j < n_points; ++j)
        d_Vs[j + k * n_points + i * 2 * n_points] =
          d_eta[j + k * n_points] * points[j + i * n_points];
}


template<bool comp_grad>
double cond_pbvn<comp_grad>::log_integrand
  (double const *point, simple_mem_stack<double> &mem) const {
  double mu[2];
  std::copy(eta.begin(), eta.end(), mu);
  for(size_t k = 0; k < 2; ++k)
    for(size_t i = 0; i < n_vars(); ++i)
      mu[k] += V(k, i) * point[i];

  return std::log(pbvn(mu, Psi.memptr()));
}

template<bool comp_grad>
double cond_pbvn<comp_grad>::log_integrand_grad
  (double const *point, double * __restrict__ grad,
   simple_mem_stack<double> &mem) const {
  double gr_inter[2],
               mu[2];
  std::copy(eta.begin(), eta.end(), mu);
  for(size_t k = 0; k < 2; ++k)
    for(size_t i = 0; i < n_vars(); ++i)
        mu[k] += V(k, i) * point[i];

  double const fn{pbvn_grad<1, false>(mu, Psi.memptr(), gr_inter)};
  std::fill(grad, grad + n_vars(), 0);
  for(size_t k = 0; k < 2; ++k)
    for(size_t i = 0; i < n_vars(); ++i)
      grad[i] += V(k, i) * gr_inter[k] / fn;
  return std::log(fn);
}

template<bool comp_grad>
void cond_pbvn<comp_grad>::log_integrand_hess
  (double const *point, double *hess,
   simple_mem_stack<double> &mem) const {
  double gr_inter[2],
               mu[2],
       hess_inter[4];
  std::copy(eta.begin(), eta.end(), mu);
  for(size_t k = 0; k < 2; ++k)
    for(size_t i = 0; i < n_vars(); ++i)
      mu[k] += V(k, i) * point[i];

  double const fn{pbvn_grad<1, false>(mu, Psi.memptr(), gr_inter)};
  pbvn_hess(mu, Psi.memptr(), hess_inter);
  std::for_each(hess_inter, hess_inter + 4, [&](double &x){  x /= fn; });
  std::for_each(gr_inter, gr_inter + 2, [&](double &x){  x /= fn; });

  for(unsigned j = 0; j < 2; ++j)
    for(unsigned i = 0; i < 2; ++i)
      hess_inter[i + j * 2] -= gr_inter[i] * gr_inter[j];

  // TODO: do this in a smarter way
  arma::mat hess_inter_mat(hess_inter, 2, 2, false, true);
  arma::mat res(hess, n_vars(), n_vars(), false, true);
  res = V.t() * hess_inter_mat * V;
}

template class cond_pbvn<false>;
template class cond_pbvn<true>;

} // namespace ghqCpp
