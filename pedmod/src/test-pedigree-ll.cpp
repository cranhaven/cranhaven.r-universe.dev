#include "testthat.h"
#include "pedigree-ll.h"

context("pedigree_ll_term_loading unit tests") {
  test_that("The log likelihood and gradient works in a 3D example") {
    /*
     set.seed(222)
     C1 <- matrix(c(1, 1, 0, 1, 1, 1, 0, 1, 1), 3)
     C2 <- matrix(c(1, .2, .2, .2, 1, .2, .2, .2 , 1), 3)
     dput(Z <- runif(3 * 4, -1) |> round(3) |> matrix(3))
     theta <- (runif(4 * 2, -1) / 4) |> round(4) |> matrix(4)
     dput(X <- runif(3 * 5, -1) |> round(3) |> matrix(3))
     beta <- runif(5) |> round(3)
     y <- c(1, 0, 1)
     dput(par <- c(beta, theta))

     library(mvtnorm)
     f <- \(x){
     beta <- head(x, 5)
     theta <- tail(x, -5) |> matrix(4)
     mu <- drop(X %*% beta)
     scales <- exp(Z %*% theta)

     D1 <- diag(scales[, 1])
     D2 <- diag(scales[, 2])
     Sig <- diag(3) + D1 %*% C1 %*% D1 + D2 %*% C2 %*% D2

     set.seed(1)
     pmvnorm(
     lower = ifelse(y > 0, 0, -Inf),  upper = ifelse(y > 0, Inf, 0),
     mean = mu, sigma = Sig, algorithm = GenzBretz(1e6, abseps = 0)) |> log()
     }

     f(par) |> dput()
     numDeriv::grad(f, par) |> dput()
     */

    constexpr size_t dim{3},
           n_scale_coefs{4},
            n_fix_effect{5},
                n_scales{2},
             n_sequences{8},
                   n_par{n_fix_effect + n_scale_coefs * n_scales};

    arma::mat C1{1, 1, 0, 1, 1, 1, 0, 1, 1},
              C2{1, .2, .2, .2, 1, .2, .2, .2 , 1};
    C1.reshape(dim, dim);
    C2.reshape(dim, dim);

    std::vector<arma::mat> scale_mats{C1, C2};

    arma::mat Z{0.863, -0.867, -0.002, -0.999, 0.833, 0.915, -0.296, -0.16, 0.146, -0.711, -0.195, -0.854};
    Z.reshape(dim, n_scale_coefs);

    arma::mat X{0.707, -0.485, -0.808, 0.025, -0.511, 0.92, 0.453, -0.058, -0.157, 0.808, -0.764, 0.36, -0.955, 0.077, 0.006};
    X.reshape(dim, n_fix_effect);

    arma::vec const y{1, 0, 1};

    constexpr double par[]{0.947, 0.698, 0.391, 0.228, 0.792, -0.194, 0.1023, 0.2204, -0.2233, 0.0827, -0.2246, -0.1926, -0.1418},
               true_logLik{-2.12355279034785},
               true_grad[]{0.0961522073094641, 0.871141137530409, 0.134870862737882, 1.01679782025544, -0.456648093268406, 0.312804187306774, -0.452926949150462, 0.120716768531984, 0.43008594879241, 0.0946372332363572, 0.0237335770697455, 0.00976885664201966, -0.150710722168742};

    parallelrng::set_rng_seeds(std::vector<unsigned>{1});
    pedmod::pedigree_ll_term_loading term(X, Z, y, scale_mats, 1, n_sequences);

    {
      bool did_fail;
      auto const fn_res{term.fn(par, 1000000, -1, 1e-5, 100000, true, true,
                                did_fail, pedmod::cdf_methods::Korobov, false)};
      expect_true
        (std::abs(fn_res.log_likelihood - true_logLik) <
          std::abs(true_logLik) * 1e-4);
    }

    constexpr double gr_shift{-1},
                       weight{.33};
    double gr[n_par], var_est[n_par + 1];
    std::fill(gr, gr + n_par, gr_shift);
    std::fill(var_est, var_est + n_par + 1, 0);

    expect_true(term.n_par() == n_par);

    bool did_fail;
    double const fn_res{term.gr(par, gr, var_est, 1000000, -1, 1e-5, 100000,
                                true, true, did_fail, weight,
                                pedmod::cdf_methods::Korobov, false)};

    expect_true
      (std::abs(fn_res - true_logLik * weight) <
        std::abs(true_logLik) * weight * 1e-4);

    for(size_t i = 0; i < n_par; ++i)
      expect_true
        (std::abs(gr[i] - gr_shift - true_grad[i] * weight) <
          std::abs(true_grad[i]) * weight * 1e-4);
  }
}
