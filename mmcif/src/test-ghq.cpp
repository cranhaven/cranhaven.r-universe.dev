#include "ghq.h"
#include <testthat.h>
#include "integrand-mixed-mult-logit-term.h"

using namespace ghqCpp;

namespace {
constexpr size_t n_vars{3};
arma::mat const eta1
{
  ([]{
    arma::mat out{-0.231792563572526, 0.539682839997113, -0.00460151582956314, 0.435237016528845, 0.983812189660966, -0.239929641131312};
    out.reshape(n_vars, 2);
    return out;
  })()
};
arma::mat const eta2
{
  ([]{
    arma::mat out{0.554890442639589, 0.869410462211818, -0.575714957434684, 0.303347532171756, -0.748889808077365, -0.465558662544936};
    out.reshape(n_vars, 2);
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
arma::vec const mu{-0.408, -0.863, -0.36};
arma::uvec const which_cat1{0, 1},
                 which_cat2{2, 3};

} // namespace

/*
 eta <- matrix(c(-0.231792563572526, 0.539682839997113, -0.00460151582956314, 0.435237016528845, 0.983812189660966, -0.239929641131312, 0.554890442639589, 0.869410462211818, -0.575714957434684, 0.303347532171756, -0.748889808077365, -0.465558662544936), 3)
 Sigma <- matrix(c(1.07173376588632, 0.760530258851724, -0.920427236518008, 0.760530258851724, 3.4214999078618, -1.56325086522103, -0.920427236518008, -1.56325086522103, 2.44510218991128), 3)
 which_cat <- 1:4
 m <- c(-0.408, -0.863, -0.36)
 n <- NCOL(Sigma)
 */

context("combined_problem and rescale_shift_problem works as expected") {
  test_that("log_integrand, log_integrand_grad, and log_integrand_x works") {
    /*
     log_integrand <- \(x){
     x <- crossprod(chol(Sigma), x) |> drop()
     x <- x + m
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
                     true_fn{-6.05734159519078},
                   true_gr[]{-1.22388917566505, -1.53012665599948, 0.870035412103452},
                 true_hess[]{-1.12007628307162, -0.742404758729616, 0.416532427824629, -0.742404758729616, -2.8290927128437, 0.351007627175719, 0.416532427824629, 0.351007627175719, -0.325157616830906};


    simple_mem_stack<double> mem;
    mixed_mult_logit_term<false> logit_term1(eta1, which_cat1),
                                 logit_term2(eta2, which_cat2);

    combined_problem prob_comb({&logit_term1, &logit_term2});
    rescale_shift_problem<false> prob(Sigma, mu, prob_comb);

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
     brute_ests <- apply(mvtnorm::rmvnorm(1e5, mean = m, sigma = Sigma), 1L, \(u){
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
     x <- x[-seq_along(eta)]
     m <- x[seq_along(m)]

     S <- matrix(0, NROW(Sigma), NROW(Sigma))
     S[upper.tri(S, TRUE)] <- tail(x, -length(m))
     S[lower.tri(S)] <- t(S)[lower.tri(S)]

     ghqCpp::mixed_mult_logit_term(
     eta = e + m, Sigma = S, which_category = which_cat - 1L,
     weights = gl$w, nodes = gl$x)
     }
     num_grad <- numDeriv::grad(fn, c(eta, m, Sigma[upper.tri(Sigma, TRUE)]),
     method.args = list(r = 6, eps = 1e-1))

     d_V <- matrix(0, NROW(Sigma), NCOL(Sigma))
     d_V[upper.tri(Sigma, TRUE)] <- tail(num_grad, 2 * 3)
     d_V[upper.tri(Sigma)] <- d_V[upper.tri(Sigma)] / 2
     d_V[lower.tri(Sigma)] <- t(d_V)[lower.tri(Sigma)]

     c(head(num_grad, -2 * 3), d_V) |> dput()
     */
    constexpr double true_fn{0.00144470445976081},
                      eps_fn{1.29621913929005e-05},
                   true_gr[]{-0.000235551071111068, -0.00036810547574373, -0.000423603741075054, 0.00108780516207751, -0.000445235900277989, -0.000296595402502146, -0.000419721393689893, 0.00102122464060735, -0.000233658457445976, -0.000459892629013978, -0.000139955045955368, 0.00110639788457643, -2.73599354622007e-05, 6.79282196413751e-05, 0.000152540244932557, -0.000274558297997265, 8.0948415582569e-05, 7.36936793451513e-06, 8.0948415582569e-05, -0.00014932776147236, -1.60327392355564e-05, 7.36936793451513e-06, -1.60327392355564e-05, -0.000145233384204499};
    constexpr double ghq_nodes[]{-4.49999070730939, -3.66995037340445, -2.9671669279056, -2.32573248617386, -1.71999257518649, -1.13611558521092, -0.565069583255576, -3.84143836181876e-16, 0.565069583255576, 1.13611558521092, 1.71999257518649, 2.32573248617386, 2.9671669279056, 3.66995037340445, 4.49999070730939},
                   ghq_weights[]{1.52247580425352e-09, 1.05911554771106e-06, 0.0001000044412325, 0.00277806884291276, 0.0307800338725461, 0.158488915795936, 0.412028687498898, 0.564100308726418, 0.412028687498898, 0.158488915795935, 0.030780033872546, 0.00277806884291278, 0.0001000044412325, 1.05911554771107e-06, 1.52247580425352e-09};

    simple_mem_stack<double> mem;
    ghq_data dat{ghq_nodes, ghq_weights, 15};

    {
      mixed_mult_logit_term<false> logit_term1(eta1, which_cat1),
                                   logit_term2(eta2, which_cat2);

      combined_problem prob_comb({&logit_term1, &logit_term2});
      rescale_shift_problem<false> prob(Sigma, mu, prob_comb);
      adaptive_problem prob_adap(prob, mem);

      auto res = ghq(dat, prob_adap, mem);
      expect_true(res.size() == 1);
      expect_true(std::abs(res[0] - true_fn) < eps_fn);
    }

    mixed_mult_logit_term<true> logit_term1(eta1, which_cat1),
                                logit_term2(eta2, which_cat2);

    combined_problem prob_comb({&logit_term1, &logit_term2});
    rescale_shift_problem<true> prob(Sigma, mu, prob_comb);
    adaptive_problem prob_adap(prob, mem);

    auto res = ghq(dat, prob_adap, mem);

    size_t const n_grad = std::distance(std::begin(true_gr), std::end(true_gr));
    expect_true(res.size() == 1 + n_grad);

    size_t const fixef_shift{logit_term1.n_out() + logit_term2.n_out()};
    expect_true(std::abs(res[0] - true_fn) < eps_fn);
    for(size_t i = 0; i < fixef_shift; ++i)
      expect_true
      (std::abs(res[i + 1] - true_gr[i]) < 1e-4 * std::abs(true_gr[i]));
    for(size_t i = fixef_shift; i < n_grad; ++i)
      expect_true
      (std::abs(res[i + 1] - true_gr[i]) < 1e-3 * std::abs(true_gr[i]));
  }

}
