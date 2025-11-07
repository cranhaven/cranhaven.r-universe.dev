#include "integrand-mixed-mult-logit-term.h"
#include "ghq-lp-utils.h"

namespace ghqCpp {

template<bool comp_grad>
mixed_mult_logit_term<comp_grad>::mixed_mult_logit_term
  (arma::mat const &eta, arma::uvec const &which_category):
  eta{eta}, which_category{which_category} {
  if(which_category.n_elem != eta.n_cols)
    throw std::invalid_argument("which_category.n_elem != eta.n_cols");
  for(arma::uword i : which_category)
    if(i > eta.n_rows)
      throw std::invalid_argument
        ("which_category has entries with i > eta.n_rows");
}

template<bool comp_grad>
void mixed_mult_logit_term<comp_grad>::eval
  (double const *points, size_t const n_points, double * __restrict__ outs,
   simple_mem_stack<double> &mem) const {
  if constexpr (comp_grad){
    double * const __restrict__ point{mem.get(n_vars())};
    double * const terms{mem.get(2 * eta.n_cols + eta.n_cols * eta.n_rows)},
           * const denoms{terms + eta.n_cols},
           * const lps{denoms + eta.n_cols};

    // do the computations point by point
    for(size_t j = 0; j < n_points; ++j){
      // copy the random effect
      for(size_t i = 0; i < n_vars(); ++i)
        point[i] = points[j + i * n_points];

      // compute the integrand
      outs[j] = 1;
      for(arma::uword k = 0; k < eta.n_cols; ++k){
        size_t const offset = k * n_vars();
        denoms[k] = 1;
        double const * eta_k{eta.colptr(k)};
        for(size_t i = 0; i < n_vars(); ++i, ++eta_k){
          lps[i + offset] = std::exp(*eta_k + point[i]);
          denoms[k] += lps[i + offset];
        }

        double const numerator
          {which_category[k] < 1 ? 1 : lps[which_category[k] - 1 + offset]};
        terms[k] = numerator / denoms[k];
        outs[j] *= terms[k];
      }

      // compute the gradient
      double * d_eta_j{outs + j + n_points};
      for(arma::uword k = 0; k < eta.n_cols; ++k)
        for(size_t i = 0; i < n_vars(); ++i, d_eta_j += n_points){
          *d_eta_j = i + 1 == which_category[k]
            ? (denoms[k] - lps[i + k * n_vars()])
            : -lps[i + k * n_vars()];
          *d_eta_j *= outs[j] / denoms[k];
        }
    }

  } else {
    double * const __restrict__ denoms
      {mem.get((1 + n_vars()) * n_points * eta.n_cols)},
           * const __restrict__ lps{denoms + n_points * eta.n_cols};

    double const * eta_ptr{eta.memptr()};
    for(size_t k = 0; k < eta.n_cols; ++k)
      for(size_t i = 0; i < n_vars(); ++i)
        for(size_t j = 0; j < n_points; ++j)
          lps[j + i * n_points + k * n_points * n_vars()] =
            eta_ptr[i + k * n_vars()] + points[j + i * n_points];

    std::for_each(lps, lps + eta.n_cols * n_vars() * n_points,
                  [](double &x) { x = std::exp(x); });

    std::fill(denoms, denoms + n_points * eta.n_cols, 1);
    for(size_t k = 0; k < eta.n_cols; ++k)
      for(size_t i = 0; i < n_vars(); ++i)
        for(size_t j = 0; j < n_points; ++j)
          denoms[j + k * n_points] +=
            lps[j + i * n_points + k * n_points * n_vars()];

    std::fill(outs, outs + n_points, 1);
    for(size_t k = 0; k < eta.n_cols; ++k){
      if(which_category[k] < 1){
        for(size_t j = 0; j < n_points; ++j)
          outs[j] *= 1 / denoms[j + k * n_points];

      } else {
        size_t const offset_case =
          (which_category[k] - 1) * n_points;
        for(size_t j = 0; j < n_points; ++j)
          outs[j] *=
            lps[j + offset_case + k * n_points * n_vars()] /
              denoms[j + k * n_points];
      }
    }
  }
}

template<bool comp_grad>
double mixed_mult_logit_term<comp_grad>::log_integrand
  (double const *point, simple_mem_stack<double> &mem) const {
  double * const __restrict__ lp{mem.get(n_vars())};

  double out{};
  for(arma::uword k = 0; k < eta.n_cols; ++k){
    double denom{1};
    double const * eta_k{eta.colptr(k)};
    for(size_t i = 0; i < n_vars(); ++i, ++eta_k){
      lp[i] = *eta_k + point[i];
      denom += std::exp(lp[i]);
    }

    if(which_category[k] < 1)
      out -= log(denom);
    else
      out += lp[which_category[k] - 1] - log(denom);
  }

  return out;
}

template<bool comp_grad>
double mixed_mult_logit_term<comp_grad>::log_integrand_grad
  (double const *point, double * __restrict__ grad,
   simple_mem_stack<double> &mem) const {
  double * const __restrict__ lp{mem.get(2 * n_vars())},
         * const __restrict__ lp_exp{lp + n_vars()};

  double out{};
  std::fill(grad, grad + n_vars(), 0);
  for(arma::uword k = 0; k < eta.n_cols; ++k){
    double denom{1};
    double const * eta_k{eta.colptr(k)};
    for(size_t i = 0; i < n_vars(); ++i, ++eta_k){
      lp[i] = *eta_k + point[i];
      lp_exp[i] = exp(lp[i]);
      denom += lp_exp[i];
    }

    // handle the denominator term of the derivative
    for(size_t i = 0; i < n_vars(); ++i)
      grad[i] -= lp_exp[i] / denom;

    if(which_category[k] < 1)
      out -= log(denom);
    else {
      out += lp[which_category[k] - 1] - log(denom);
      grad[which_category[k] - 1] += 1;
    }
  }

  return out;
}

template<bool comp_grad>
void mixed_mult_logit_term<comp_grad>::log_integrand_hess
  (double const *point, double *hess,
 simple_mem_stack<double> &mem) const {
  double * const __restrict__ lp_exp{mem.get(n_vars())};

  std::fill(hess, hess + n_vars() * n_vars(), 0);
  for(arma::uword k = 0; k < eta.n_cols; ++k){
    double denom{1};
    double const * eta_k{eta.colptr(k)};
    for(size_t i = 0; i < n_vars(); ++i, ++eta_k){
      lp_exp[i] = exp(*eta_k + point[i]);
      denom += lp_exp[i];
    }

    double const denom_sq{denom * denom};
    for(size_t j = 0; j < n_vars(); ++j){
      for(size_t i = 0; i < j; ++i){
        double entry{lp_exp[i] * lp_exp[j] / denom_sq};
        hess[i + j * n_vars()] += entry;
        hess[j + i * n_vars()] += entry;
      }
      hess[j + j * n_vars()] -=
        (denom - lp_exp[j]) * lp_exp[j] / denom_sq;
    }
  }
}

template class mixed_mult_logit_term<false>;
template class mixed_mult_logit_term<true>;

} // namespace ghqCpp
