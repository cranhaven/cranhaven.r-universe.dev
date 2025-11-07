#include "integrand-mixed-mult-logit-term.h"
#include <testthat.h>

using namespace ghqCpp;

namespace {
constexpr size_t n_vars{3};
arma::mat const eta
  {
    ([]{
      arma::mat out{-0.231792563572526, 0.539682839997113, -0.00460151582956314, 0.435237016528845, 0.983812189660966, -0.239929641131312, 0.554890442639589, 0.869410462211818, -0.575714957434684, 0.303347532171756, -0.748889808077365, -0.465558662544936};
      out.reshape(n_vars, 4);
      return out;
    })()
  };
arma::mat const Sigma
  {
    ([]{
      arma::mat out{1.07173376588632, 0.760530258851724, -0.920427236518008, 0.760530258851724, 3.4214999078618, -1.56325086522103, -0.920427236518008, -1.56325086522103, 2.44510218991128};
      out.reshape(n_vars, n_vars);
      return out;
    })()
  };
arma::uvec const which_cat{0, 1, 2, 3};

} // namespace

/*
 eta <- matrix(c(-0.231792563572526, 0.539682839997113, -0.00460151582956314, 0.435237016528845, 0.983812189660966, -0.239929641131312, 0.554890442639589, 0.869410462211818, -0.575714957434684, 0.303347532171756, -0.748889808077365, -0.465558662544936), 3)
 Sigma <- matrix(c(1.07173376588632, 0.760530258851724, -0.920427236518008, 0.760530258851724, 3.4214999078618, -1.56325086522103, -0.920427236518008, -1.56325086522103, 2.44510218991128), 3)
 which_cat <- 1:4
 n <- NCOL(Sigma)
 */

context("mixed_mult_logit_term works as expected") {
  test_that("log_integrand, log_integrand_grad, and log_integrand_x works") {
    /*
     log_integrand <- \(x){
     x <- crossprod(chol(Sigma), x) |> drop()
     exp_lp <- exp(eta + x)
     denom <- 1 + colSums(exp_lp)
     num <- mapply(
     \(i, j) if(i == 1L) 1 else exp_lp[i - 1L, j], i = which_cat,
     j = 1:NCOL(eta))
     sum(log(num / denom))
     }
     set.seed(111)
     dput(point <- runif(n, -1))
     dput(log_integrand(point))
     dput(numDeriv::grad(log_integrand, point))
     dput(numDeriv::hessian(log_integrand, point))
     */

    constexpr double point[]{0.185962568968534, 0.452962242998183, -0.259155992884189},
                     true_fn{-6.60961883573595},
                   true_gr[]{-1.55267270391114, -2.49602740295471, 0.915885878976044},
                 true_hess[]{-0.905709341570217, -0.613183151058906, 0.374124733829623, -0.613183151058906, -2.82309941652404, 0.35104427634114, 0.374124733829623, 0.35104427634114, -0.278036077869056};

    simple_mem_stack<double> mem;
    mixed_mult_logit_term<false> logit_term(eta, which_cat);
    rescale_problem<false> prob(Sigma, logit_term);

    expect_true
      (std::abs(prob.log_integrand(point, mem) - true_fn) <
        std::abs(true_fn) * 1e-8);

    {
      double gr[n_vars];
      expect_true
        (std::abs(prob.log_integrand_grad(point, gr, mem) - true_fn) <
          std::abs(true_fn) * 1e-8);
      for(size_t i = 0; i < n_vars; ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-8);
    }

    double hess[n_vars * n_vars];
    prob.log_integrand_hess(point, hess, mem);
    for(size_t i = 0; i < n_vars * n_vars; ++i)
      expect_true
        (std::abs(hess[i] - true_hess[i]) < std::abs(true_hess[i]) * 1e-8);
  }

  test_that("eval works and so does the gradient") {
    /*
     brute_ests <- apply(mvtnorm::rmvnorm(1e5, sigma = Sigma), 1L, \(u){
     exp_lp <- exp(eta + u)
     denom <- 1 + colSums(exp_lp)
     num <- mapply(
     \(i, j) if(i == 1L) 1 else exp_lp[i - 1L, j], i = which_cat,
     j = 1:NCOL(eta))
     prod(num / denom)
     })
     dput(3 * sd(brute_ests) / sqrt(length(brute_ests)))
     dput(mean(brute_ests))

     dput(gl <- fastGHQuad::gaussHermiteData(15))

     fn <- \(x){
     e <- x[seq_along(eta)] |> matrix(nrow = NROW(eta))

     S <- matrix(0, NROW(Sigma), NROW(Sigma))
     S[upper.tri(S, TRUE)] <- tail(x, -length(e))
     S[lower.tri(S)] <- t(S)[lower.tri(S)]

     ghqCpp::mixed_mult_logit_term(
     eta = e, Sigma = S, which_category = which_cat - 1L,
     weights = gl$w, nodes = gl$x)
     }
     num_grad <- numDeriv::grad(fn, c(eta, Sigma[upper.tri(Sigma, TRUE)]),
     method.args = list(r = 6, eps = 1e-1))

     d_V <- matrix(0, NROW(Sigma), NCOL(Sigma))
     d_V[upper.tri(Sigma, TRUE)] <- tail(num_grad, 2 * 3)
     d_V[upper.tri(Sigma)] <- d_V[upper.tri(Sigma)] / 2
     d_V[lower.tri(Sigma)] <- t(d_V)[lower.tri(Sigma)]

     c(head(num_grad, -2 * 3), d_V) |> dput()
     */
    constexpr double true_fn{0.0014209655948192},
                      eps_fn{4.24977676906005e-06},
                   true_gr[]{-0.00024007729819535, -0.000416804855159174, -0.000455000708495158, 0.00106286963865587, -0.000496899238879953, -0.000313932907303394, -0.000424382636099625, 0.000944229451364074, -0.000249489926266853, -0.000486418895999847, -0.000166064107058078, 0.00104306315468486, -0.000262350232894601, 9.46575402741063e-05, 1.90396810673787e-05, 9.46575402741063e-05, -0.000145419375861095, -1.65276520883059e-05, 1.90396810673787e-05, -1.65276520883059e-05, -0.000145290030872598};
    constexpr double ghq_nodes[]{-4.49999070730939, -3.66995037340445, -2.9671669279056, -2.32573248617386, -1.71999257518649, -1.13611558521092, -0.565069583255576, -3.84143836181876e-16, 0.565069583255576, 1.13611558521092, 1.71999257518649, 2.32573248617386, 2.9671669279056, 3.66995037340445, 4.49999070730939},
                   ghq_weights[]{1.52247580425352e-09, 1.05911554771106e-06, 0.0001000044412325, 0.00277806884291276, 0.0307800338725461, 0.158488915795936, 0.412028687498898, 0.564100308726418, 0.412028687498898, 0.158488915795935, 0.030780033872546, 0.00277806884291278, 0.0001000044412325, 1.05911554771107e-06, 1.52247580425352e-09};

    simple_mem_stack<double> mem;
    ghq_data dat{ghq_nodes, ghq_weights, 15};

    {
      mixed_mult_logit_term<false> mult_term(eta, which_cat);
      rescale_problem<false> prob_resclaed(Sigma, mult_term);
      adaptive_problem prob(prob_resclaed, mem);

      auto res = ghq(dat, prob, mem);
      expect_true(res.size() == 1);
      expect_true(std::abs(res[0] - true_fn) < eps_fn);
    }

    mixed_mult_logit_term<true> mult_term(eta, which_cat);
    rescale_problem<true> prob(Sigma, mult_term);
    adaptive_problem prob_adap(prob, mem);

    auto res = ghq(dat, prob_adap, mem);

    size_t const n_grad = std::distance(std::begin(true_gr), std::end(true_gr));
    expect_true(res.size() == 1 + n_grad);

    size_t const fixef_shift{mult_term.n_out()};
    expect_true(std::abs(res[0] - true_fn) < eps_fn);
    for(size_t i = 0; i < fixef_shift; ++i)
      expect_true
        (std::abs(res[i + 1] - true_gr[i]) < 1e-4 * std::abs(true_gr[i]));
    for(size_t i = fixef_shift; i < n_grad; ++i)
      expect_true
        (std::abs(res[i + 1] - true_gr[i]) < 1e-3 * std::abs(true_gr[i]));
  }
}
