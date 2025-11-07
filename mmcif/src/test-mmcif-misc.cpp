#include "mmcif-misc.h"
#include "testthat.h"

context("mmcif functions work") {
  test_that("mmcif_logLik works with singleton data") {
    /*
     set.seed(111)
     Sig <- rWishart(1, 3, diag(3)) |> round(3) |> drop()
     x <- c(-1, 0, 1)
     dput(mvtnorm::dmvnorm(x, sigma = Sig, log = TRUE))
     */
    arma::mat Sig{2.498, -0.326, 0.361, -0.326, 0.629, -1.326, 0.361, -1.326, 3.632};
    Sig.reshape(3, 3);
    arma::vec x{-1, 0, 1};
    constexpr double truth{-3.51260818319038};
    ghqCpp::simple_mem_stack<double> mem;

    expect_true
      (std::abs(log_dmvn(x, Sig, mem) - truth) < std::abs(truth) * 1e-8);
  }

  test_that("log_dmvn_grad works with singleton data") {
    /*
     upper_to_full <- \(x){
     dim <- (sqrt(8 * length(x) + 1) - 1) / 2
     out <- matrix(0, dim, dim)
     out[upper.tri(out, TRUE)] <- x
     out[lower.tri(out)] <- t(out)[lower.tri(out)]
     out
     }
     d_upper_to_full <- \(x){
     dim <- (sqrt(8 * length(x) + 1) - 1) / 2
     out <- matrix(0, dim, dim)
     out[upper.tri(out, TRUE)] <- x
     out[upper.tri(out)] <- out[upper.tri(out)] / 2
     out[lower.tri(out)] <- t(out)[lower.tri(out)]
     out
     }

     set.seed(111)
     Sig <- rWishart(1, 3, diag(3)) |> round(3) |> drop()
     x <- c(-1, 0, 1)

     f <- \(x){
     Sig <- upper_to_full(tail(x, -3))
     x <- head(x, 3)
     mvtnorm::dmvnorm(x, sigma = Sig, log = TRUE)
     }

     par <- c(x, Sig[upper.tri(Sig, TRUE)])
     f(par) |> dput()
     gr <- numDeriv::grad(f, par)
     c(head(gr, 3), d_upper_to_full(tail(gr, -3))) |> dput()
     */
    arma::mat Sig{2.498, -0.326, 0.361, -0.326, 0.629, -1.326, 0.361, -1.326, 3.632};
    Sig.reshape(3, 3);
    arma::vec x{-1, 0, 1};
    constexpr double truth{-3.51260818319038},
                     d_x[]{0.27706999457422, -2.14834297562305, -1.0872040346261},
                 d_Sigma[]{-0.188700534563502, -0.601986450746734, -0.239165236210084, -0.601986450746734, -1.55107694077196, -0.210693674807399, -0.239165236210084, -0.210693674807399, -0.0411451391039126};
    ghqCpp::simple_mem_stack<double> mem;

    auto res = log_dmvn_grad(x, Sig, mem);

    expect_true(std::abs(res.value - truth) < std::abs(truth) * 1e-8);
    expect_true(res.d_x.n_elem == 3);
    for(size_t i = 0; i < 3; ++i)
      expect_true(std::abs(res.d_x[i] - d_x[i]) < std::abs(d_x[i]) * 1e-8);
    expect_true(res.d_Sigma.n_elem == 9);
    for(size_t i = 0; i < 9; ++i)
      expect_true(std::abs(res.d_Sigma[i] - d_Sigma[i]) < std::abs(d_Sigma[i]) * 1e-8);
  }
}
