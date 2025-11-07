#include "integrand-probit-term.h"
#include <testthat.h>

using namespace ghqCpp;

namespace {
/*
 K <- 3L
 set.seed(555)
 dput(Sigma <- rWishart(1, K, diag(1/K, K)) |> drop() |> round(3))
 dput(eta <- runif(1, -1) |> round(3))
 dput(s <- runif(1) |> round(3))
 dput(z <- runif(K, -1) |> round(3))

 get_n_remove <- \(x, n){
 out <- x[1:n]
 eval(substitute(out <- out[-(1:n)], list(out = substitute(x), n = n)),
 parent.frame())
 out
 }
 */

constexpr size_t K{3};
constexpr double s{0.914},
               eta{-0.133},
             v_z[]{0.402, 0.029, -0.667},
         v_Sigma[]{0.465, -0.704, 0.403, -0.704, 1.445, -0.878, 0.403, -0.878, 0.578};

const arma::mat Sigma(v_Sigma, K, K);
const arma::vec z(v_z, K);

} // namespace

context("mixed_probit_term works as expected") {
  test_that("log_integrand, log_integrand_grad, and log_integrand_x works") {
    /*
     set.seed(77)
     dput(point <- runif(K, -1) |> round(3))
     f <- \(u) pnorm((eta + z %*% crossprod(chol(Sigma), u)) / s, log.p = TRUE) |> drop()
     dput(f(point))
     dput(numDeriv::grad(f, point))
     dput(numDeriv::hessian(f, point))
     */
    constexpr double point[]{-0.417, 0.435, 0.725},
                     true_fn{-0.721978383862351},
                   true_gr[]{-0.134689411308399, 0.276573529656919, -0.119020103547564},
                 true_hess[]{-0.017353738156663, 0.0356344612756232, -0.0153348633039639, 0.0356344612756232, -0.0731724092974138, 0.031488869314097, -0.0153348633039639, 0.031488869314097, -0.0135508574925665};

    simple_mem_stack<double> mem;
    mixed_probit_term probit_prob(s, eta, z);
    rescale_problem prob_rescaled(Sigma, probit_prob);

    {
      double const res{prob_rescaled.log_integrand(point, mem)};
      expect_true(std::abs(res - true_fn) < std::abs(true_fn) * 1e-8);
    }
    {
      double gr[K];
      double const res{prob_rescaled.log_integrand_grad(point, gr, mem)};
      expect_true(std::abs(res - true_fn) < std::abs(true_fn) * 1e-8);
      for(size_t i = 0; i < K; ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-8);
    }

    double hess[K * K];
    prob_rescaled.log_integrand_hess(point, hess, mem);
    for(size_t i = 0; i < K * K; ++i)
      expect_true
        (std::abs(hess[i] - true_hess[i]) < std::abs(true_hess[i]) * 1e-8);
  }

  test_that("eval works and so does the gradient") {
    /*
     # we use that there is a very simple alternative form
     dput(fastGHQuad::gaussHermiteData(15))
     dput(pnorm(eta / s, sd = sqrt(1 + z %*% Sigma %*% z / s / s)))
     f <- \(x){
     eta <- get_n_remove(x, 1)
     s <- get_n_remove(x, 1)
     z <- x
     pnorm(eta / s, sd = sqrt(1 + z %*% Sigma %*% z / s / s))
     }
     dput(numDeriv::grad(f, c(eta, s, z)))
     */
    constexpr double true_fn{0.446299033408421},
                   true_gr[]{0.401319107373109, 0.0502764127148969, -0.00562650265911483, 0.0189511822315638, -0.0136957568789037},
                   ghq_nodes[]{-4.49999070730939, -3.66995037340445, -2.9671669279056, -2.32573248617386, -1.71999257518649, -1.13611558521092, -0.565069583255576, -3.84143836181876e-16, 0.565069583255576, 1.13611558521092, 1.71999257518649, 2.32573248617386, 2.9671669279056, 3.66995037340445, 4.49999070730939},
                   ghq_weights[]{1.52247580425352e-09, 1.05911554771106e-06, 0.0001000044412325, 0.00277806884291276, 0.0307800338725461, 0.158488915795936, 0.412028687498898, 0.564100308726418, 0.412028687498898, 0.158488915795935, 0.030780033872546, 0.00277806884291278, 0.0001000044412325, 1.05911554771107e-06, 1.52247580425352e-09};

    simple_mem_stack<double> mem;
    ghq_data dat{ghq_nodes, ghq_weights, 15};

    {
      mixed_probit_term<false> probit_prob(s, eta, z);
      rescale_problem<false> prob_recaled(Sigma, probit_prob);
      adaptive_problem prob(prob_recaled, mem);

      auto res = ghq(dat, prob, mem);
      expect_true(res.size() == 1);
      expect_true(std::abs(res[0] - true_fn) < std::abs(true_fn) * 1e-8);
    }

    mixed_probit_term<true> probit_prob(s, eta, z);
    rescale_problem<false> prob_recaled(Sigma, probit_prob);
    adaptive_problem prob(prob_recaled, mem);

    auto res = ghq(dat, prob, mem);
    expect_true(res.size() == 3 + K);
    expect_true(std::abs(res[0] - true_fn) < std::abs(true_fn) * 1e-8);

    for(size_t i = 0; i < 2 + K; ++i)
      expect_true
        (std::abs(res[i + 1] - true_gr[i]) < std::abs(true_gr[i]) * 1e-6);
  }
}
