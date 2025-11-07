#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <vector>
#include <numeric>
#include <limits>

namespace ghqCpp {

/// returns the amount of working memory for
constexpr unsigned n_wk_mem_extrapolation
  (unsigned const n_vars, unsigned const order){
  return n_vars * (3 + order);
}

/***
 * performs numerical differentiation with Richardson extrapolation. Given
 *
 *  - function f(x)
 *  - step size h > 0 (or a relative/absolute step size)
 *  - reduction factor v > 1
 *
 * our starting point is
 *
 *   A_0(h) = (f(x + h) - f(x - h)) / 2h
 *
 * subsequent improvements are
 *
 *   A_i(h) = (v^(2i) A_(i - 1)(h / v) - A_(i - 1)(h)) / (v^(2i) - 1)
 *
 * yielding an error of order O(h^(2(i + 1))).
 *
 * We ever only need to store i + 1 version A_k at ny one point. The computation
 * proceeds as follows.
 *
 *  A_0(h)
 *  A_0(h) A_0(h/v)
 *  A_1(h) A_0(h/v)
 *
 *  A_1(h) A_0(h/v) A_0(h/v^2)
 *  A_1(h) A_1(h/v) A_0(h/v^2)
 *  A_2(h) A_1(h/v) A_0(h/v^2)
 *
 *  A_2(h) A_1(h/v) A_0(h/v^2) A_0(h/v^4)
 *  A_2(h) A_1(h/v) A_1(h/v^2) A_0(h/v^4)
 *  A_2(h) A_2(h/v) A_1(h/v^2) A_0(h/v^4)
 *  A_3(h) A_2(h/v) A_1(h/v^2) A_0(h/v^4)
 *
 *  etc. We can stop early by computing the error approximation
 *
 *    E ~ (A_i(h) - A_i(h/v)) * v^(2i) / (v^(2i) - 1)
 *
 *
 *  is small.
 *
 *  The TFunctor class needs to take two arguments. The point the function is
 *  evaluated at and a double pointer for the output. The output may be
 *  multivariate.
 */
template<class TFunctor>
struct richardson_extrapolation {
  richardson_extrapolation(TFunctor &func, unsigned const order,
                           double * wk_mem, double const eps,
                           double const scale, double const tol,
                           unsigned const n_vars):
    func(func), n_vars{n_vars}, order{order}, eps{eps},
    scale{scale}, wk_mem{wk_mem}, tol{tol}
  {
    if(scale <= 1)
      throw std::invalid_argument("scale <= 1");
    else if(eps <= 0)
      throw std::invalid_argument("eps <= 0");
    else if(tol <= 0)
      throw std::invalid_argument("tol <= 0");
  }

  /***
   * computes the approximate derivatives and stores the output in the passed
   * pointer.
   */
  void operator()(double const x, double *out) const {
    static double const step_min
    {std::sqrt(std::numeric_limits<double>::epsilon())};
    double const step{std::max(step_min, std::abs(x) * eps)};

    // compute the initial points
    double delta{step};
    comp_aprx(x, delta, 0);

    std::vector<char> converged;
    if(order > 0){
      // compute the thresholds
      func(x, thresholds);
      for(unsigned i = 0; i < n_vars; ++i)
        thresholds[i] = std::max(tol, std::abs(thresholds[i]) * tol);
      converged.assign(n_vars, 0);
    }

    // compute the result
    double const scale_sq{scale * scale};
    for(unsigned i = 0; i < order; ++i){
      /* the current stored output is
       * A_(i - 1)(h) A_(i - 2)(h / v) ... A_0(h / v^(i - 1))
       *
       * we need to update this to
       *
       * A_i(h) A_(i - 1)(h / v), ..., A_0(h / v^i)
       *
       * starting from the right.
       */
      delta /= scale;
      comp_aprx(x, delta, i + 1);

      double mult{1};
      for(unsigned j = i + 1; j-- > 1;){
        double *lhs{cur_apprx(j)},
               *rhs{cur_apprx(j + 1)};
        mult *= scale_sq;
        for(unsigned k = 0; k < n_vars; ++k)
          if(!converged[k])
            lhs[k] = rhs[k] + (rhs[k] - lhs[k]) / (mult - 1);
      }

      // check if we pass the relative error
      double *lhs{cur_apprx(0)},
             *rhs{cur_apprx(1)};
      bool passed{i > 0};
      for(unsigned k = 0; k < n_vars; ++k){
        if(!converged[k]){
          double const err_est{(lhs[k] - rhs[k]) * mult / (mult - 1)};
          converged[k] = std::abs(err_est) < thresholds[k];
          passed &= converged[k];
        }
      }

      if(passed)
        break;

      // update the last one
      mult *= scale_sq;
      for(unsigned k = 0; k < n_vars; ++k)
        if(!converged[k])
          lhs[k] = rhs[k] + (rhs[k] - lhs[k]) / (mult - 1);
    }

    std::copy(cur_apprx(0), cur_apprx(0) + n_vars, out);
  }

private:
  TFunctor &func;
  /// the number of variables func returns
  unsigned n_vars;
  /// maximum order for the extrapolation
  unsigned order;
  /// used to construct the step size. This is max(eps, |x| * eps)
  double eps;
  /// the scaling factor
  double scale;
  /// working memory
  double * wk_mem;
  /// convergence thresholds
  double * thresholds{wk_mem + n_vars};
  /// stores the approximations
  double * apprx{thresholds + n_vars};
  /**
   * tolerance to deem convergence. The threshold is max(tol, |f| * tol) for
   * each element of functor f.
   */
  double tol;

  /// returns the approximation at the index
  double * cur_apprx(unsigned const idx) const {
    return apprx + n_vars * idx;
  }

  /// computes the approximation
  void comp_aprx(double const x, double const delta, unsigned const idx) const {
    double *out{cur_apprx(idx)};
    func(x + delta, out);
    func(x - delta, wk_mem);
    double const denom{2 * delta};
    for(unsigned i = 0; i < n_vars; ++i)
      out[i] = (out[i] - wk_mem[i]) / denom;
  }
};

} // namespace ghqCpp
