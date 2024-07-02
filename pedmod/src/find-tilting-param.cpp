#include "pnorm.h"
#include "find-tilting-param.h"
#include <psqn-bfgs.h>
#include <cmath>
#include <numeric>
#include <algorithm>
#include <limits.h>

#include <R_ext/RS.h> // for F77_NAME and F77_CALL

using std::exp;

extern "C" {
  void F77_NAME(dtpsv)
    (char const *uplo, char const *trans, char const *diag, int const *n,
     double const *ap, double *x, int const *incx, size_t, size_t, size_t);
}

namespace {
inline double pnrm
  (double const x, bool const lower_tail, bool const use_log){
  return pnorm_std(x, lower_tail, use_log);
}
inline double dnrm_log(double const x){
  // dput(sqrt(.Machine$double.xmax / 10))
  constexpr double sqrt_double_max{4.23992114886859e+153},
                   log_sqrt_2pi{0.918938533204673};
  return x > sqrt_double_max ? -std::numeric_limits<double>::infinity()
                             : -log_sqrt_2pi - x * x / 2;
}

/// the class to find the root of the gradient ignoring the constraint
class root_problem final : public PSQN::problem {
  size_t const dim;

  double const * const lower_limits;
  double const * const upper_limits;
  double const * const cholesky;

  enum class limit_type : char {
    upper_bounded,
    lower_bounded,
    both
  };

  std::vector<limit_type> l_type;
  std::vector<double> working_mem
    {std::vector<double>(5 * dim + 4 * dim * dim + 2 * dim * dim)};

  /// computes C^T.x setting the diagonal of C to zero
  void cholesky_product_T(double *res, double const * x){
    double const *c{cholesky};
    for(size_t i = 0; i < dim; ++i, c += i)
      res[i] = std::inner_product(x, x + i, c, 0.);
  }

  /// computes C.x setting the diagonal of C to zero
  void cholesky_product(double *res, double const * x){
    double const *c{cholesky};
    std::fill(res, res + dim, 0);
    for(size_t i = 0; i < dim; ++i, c += i)
      for(size_t j = 0; j < i; ++j)
        res[j] += c[j] * x[i];
  }

  template<bool comp_grad>
  double eval(double const * PSQN_RESTRICT val,
              double       * PSQN_RESTRICT gr) {
    double const * const tilt{val},
                 * const point{tilt + dim};

    double * mem_ptr{working_mem.data()};
    auto get_mem = [&](size_t const n_mem){
      auto out = mem_ptr;
      mem_ptr += n_mem;
      return out;
    };

    double * const choleksy_T_point{get_mem(dim)};
    cholesky_product_T(choleksy_T_point, point);

    double * const derivs_pnrm_terms{get_mem(dim)},
           * const hess_pnrm_terms{get_mem(comp_grad ? dim : 1)};
    for(size_t i = 0; i < dim; ++i){
      double lb_shift{-std::numeric_limits<double>::infinity()},
             ub_shift{std::numeric_limits<double>::infinity()};

      switch(l_type[i]){
      case limit_type::lower_bounded:
        lb_shift = lower_limits[i] - choleksy_T_point[i] - tilt[i];
        break;
      case limit_type::upper_bounded:
        ub_shift = upper_limits[i] - choleksy_T_point[i] - tilt[i];
        break;

      case limit_type::both:
        lb_shift = lower_limits[i] - choleksy_T_point[i] - tilt[i];
        ub_shift = upper_limits[i] - choleksy_T_point[i] - tilt[i];
        break;
      }

      double denom_log;
      if(lb_shift > 0){
        double const v_lb{pnrm(lb_shift, false, true)},
                     v_ub{pnrm(ub_shift, false, true)};

        denom_log = v_lb + std::log1p(-exp(v_ub - v_lb));
      } else if(ub_shift < 0){
        double const v_lb{pnrm(lb_shift, true, true)},
                     v_ub{pnrm(ub_shift, true, true)};

        denom_log = v_ub + std::log1p(-exp(v_lb - v_ub));

      } else {
        double const v_lb{pnrm(lb_shift, true, false)},
                     v_ub{pnrm(ub_shift, false, false)};

        denom_log = std::log1p(-v_lb - v_ub);
      }

      switch(l_type[i]){
      case limit_type::lower_bounded:
        {
          double const dnrm_log_lb{dnrm_log(lb_shift)},
                          ratio_lb{exp(dnrm_log_lb - denom_log)};

          derivs_pnrm_terms[i] = ratio_lb;
          if constexpr (comp_grad)
            hess_pnrm_terms[i] = lb_shift * ratio_lb -
              derivs_pnrm_terms[i] * derivs_pnrm_terms[i];

        }
        break;

      case limit_type::upper_bounded:
        {
          double const dnrm_log_ub{dnrm_log(ub_shift)},
                       ratio_ub{exp(dnrm_log_ub - denom_log)};

          derivs_pnrm_terms[i] = - ratio_ub;
          if constexpr(comp_grad)
            hess_pnrm_terms[i] = - ub_shift * ratio_ub -
              derivs_pnrm_terms[i] * derivs_pnrm_terms[i];
        }
        break;

      case limit_type::both:
        {
          double const dnrm_log_lb{dnrm_log(lb_shift)},
                       dnrm_log_ub{dnrm_log(ub_shift)},
                       ratio_lb{exp(dnrm_log_lb - denom_log)},
                       ratio_ub{exp(dnrm_log_ub - denom_log)};

          derivs_pnrm_terms[i] = ratio_lb - ratio_ub;
          if constexpr(comp_grad)
            hess_pnrm_terms[i] = lb_shift * ratio_lb - ub_shift * ratio_ub -
              derivs_pnrm_terms[i] * derivs_pnrm_terms[i];
        }
        break;

      }
    }

    double * const gr_params{get_mem(2 * dim)};
    for(size_t i = 0; i < dim; ++i)
      gr_params[i] = tilt[i] - point[i] + derivs_pnrm_terms[i];

    double * choleksy_derivs_pnrm_terms = choleksy_T_point;
    cholesky_product(choleksy_derivs_pnrm_terms, derivs_pnrm_terms);
    for(size_t i = 0; i < dim; ++i)
      gr_params[i + dim] = -tilt[i] + choleksy_derivs_pnrm_terms[i];

    double out{};
    for(size_t i = 0; i < 2 * dim; ++i)
      out += gr_params[i] * gr_params[i];

    if constexpr(comp_grad){
      // TODO: we can avoid explicitly computing the Hessian
      size_t const dim_hess{2 * dim};
      double * const hess{get_mem(dim_hess * dim_hess)},
             * const diff_mat{get_mem(dim * dim)},
             * const diff_mat_outer{get_mem(dim * dim)};
      std::fill(hess, hess + dim_hess * dim_hess, 0);
      std::fill(diff_mat, diff_mat + dim * dim, 0);
      std::fill(diff_mat_outer, diff_mat_outer + dim * dim, 0);

      for(size_t i = 0; i < dim; ++i)
        hess[i + i *dim_hess] = hess_pnrm_terms[i] + 1;

      {
        double const *c{cholesky};
        for(size_t i = 0; i < dim; ++i, c += i)
          for(size_t j = 0; j < i; ++j)
            diff_mat[j + i * dim] = hess_pnrm_terms[i] * c[j];
      }

      for(size_t i = 0; i < dim; ++i){
        for(size_t j = 0; j < i; ++j){
          hess[j + dim + i * dim_hess] = diff_mat[j + i * dim];
          hess[i + (j + dim) * dim_hess] = diff_mat[j + i * dim];
        }
        hess[i + dim + i * dim_hess] = -1;
        hess[i + (i + dim) * dim_hess] = -1;
      }

      double * const hess_last_block{hess + dim * (1 + dim_hess)};
      for(size_t k = 0; k < dim; ++k)
        for(size_t j = 0; j < k; ++j)
          for(size_t i = 0; i < dim; ++i)
            hess_last_block[i + j * dim_hess] +=
              diff_mat[i + k * dim] * cholesky[j + (k * (k + 1)) / 2];

      std::fill(gr, gr + 2 * dim, 0);
      for(size_t i = 0; i < dim_hess; ++i)
        for(size_t j = 0; j <dim_hess; ++j)
          gr[j] += hess[j + i * dim_hess] * gr_params[i];

      std::for_each(gr, gr + 2 * dim, [](double &x) { x *= 2; });
    }

    return out;
  }

public:
  root_problem(
    size_t const dim, double const * const lower_limits,
    double const * const upper_limits, double const * const cholesky):
  dim{dim}, lower_limits{lower_limits}, upper_limits{upper_limits},
  cholesky{cholesky} {
    l_type.reserve(dim);
    for(size_t i = 0; i < dim; ++i){
      bool const inf_lb{std::isinf(lower_limits[i])},
                 inf_ub{std::isinf(upper_limits[i])};
      if(inf_lb && inf_ub)
        throw std::runtime_error("both limits are infinite");
      else if(inf_lb && !inf_ub)
        l_type.emplace_back(limit_type::upper_bounded);
      else if(!inf_lb && inf_ub)
        l_type.emplace_back(limit_type::lower_bounded);
      else
        l_type.emplace_back(limit_type::both);
    }
  }

  PSQN::psqn_uint size() const {
    return 2 * dim;
  }

  double func(double const *val) {
    return eval<false>(val, nullptr);
  }

  double grad(double const * PSQN_RESTRICT val,
              double       * PSQN_RESTRICT gr){
    return eval<true>(val, gr);
  }

  void start_val(double *res){
    double * const tilt{res},
           * const point{tilt + dim};

    for(size_t i = 0; i < dim; ++i){
      switch(l_type[i]){
      case limit_type::lower_bounded:
        point[i] = lower_limits[i] + 1;
        break;

      case limit_type::upper_bounded:
        point[i] = upper_limits[i] - 1;
        break;

      case limit_type::both:
        point[i] = (lower_limits[i] + upper_limits[i]) / 2;
      }
    }

    constexpr char uplo{'U'}, trans{'T'}, diag{'U'};
    int const n = dim;
    constexpr int incx{1};
    F77_CALL(dtpsv)(&uplo, &trans, &diag, &n, cholesky, point, &incx, 1, 1, 1);

    cholesky_product_T(tilt, point);
    std::for_each(tilt, tilt + dim, [](double &x) { x *= -1; });
  }

  bool is_interior_solution(double const *res) {
    double const * const point{res + dim};
    double * const choleksy_T_point{working_mem.data()};
    {
      double const *c{cholesky};
      for(size_t i = 0; i < dim; ++i, c += i)
        choleksy_T_point[i] = std::inner_product(point, point + i + 1, c, 0.);
    }

    bool out{true};
    for(size_t i = 0; i < dim && out; ++i)
      out &= lower_limits[i] <= choleksy_T_point[i] &&
        choleksy_T_point[i] <= upper_limits[i];

    return out;
  }
};

} // namespace

find_tilting_param_res find_tilting_param
  (size_t const dim, double const * const lower_limits,
   double const * const upper_limits, double const * const cholesky,
   double const rel_eps){
  std::vector<double> param(2 * dim);

  root_problem prob(dim, lower_limits, upper_limits, cholesky);
  prob.start_val(param.data());

  auto res = PSQN::bfgs(prob, param.data(), rel_eps, 1000L);
  bool const succeeded =
    res.info == PSQN::info_code::converged ||
    res.info == PSQN::info_code::max_it_reached;
  bool const is_interior{prob.is_interior_solution(param.data())};

  param.data()[dim - 1] = 0;
  find_tilting_param_res out
    { std::vector<double>(dim), succeeded, is_interior };

  if(succeeded){
    std::copy(param.data(), param.data() + dim, out.tilting_param.data());
    return out;
  }

  std::fill(out.tilting_param.data(), out.tilting_param.data() + dim, 0);
  return out;
}
