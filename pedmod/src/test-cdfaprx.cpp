#include "cdfaprx.h"
#include <testthat.h>
#include <limits>
#include "threat-safe-random.h"

context("cdfaprx unit tests") {
  test_that("cdf<pedigree_l_factor_Hessian> gives similar output as R with a multivariate example") {
    /*
     X <- matrix(c(1, 1, 1, .5, 0, 1), 3)
     C1 <- matrix(c(1, 1, 0, 1, 1, 0, 0, 0, 1), 3)
     C2 <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), 3)

     lbs <- c(-Inf, 0, -Inf)
     ubs <- c(0, Inf, 0)

     beta <- c(1, -.5)
     sigs <- c(.25, .75)

     library(mvtnorm)
     f <- \(x){
     set.seed(1)
     S <- diag(3) + x[3] * C1 + x[4] * C2
     pmvnorm(
     lower = lbs, upper = ubs, mean = drop(X %*% head(x, 2)), sigma = S,
     algorithm = GenzBretz(maxpts = 1000000L, abseps = 0, releps = 1e-10)) |>
     log()
     }

     f(c(beta, sigs)) |> exp() |> dput()
     numDeriv::grad(f, c(beta, sigs)) |> dput()
     numDeriv::hessian(f, c(beta, sigs)) |> dput()
     */
    constexpr arma::uword n_mem{3},
                          n_fixef{2},
                          n_scales{2},
                          hess_dim{n_fixef + n_scales};

    constexpr double Inf = std::numeric_limits<double>::infinity(),
        likelihood_truth{0.0781160409595038};

    arma::vec const gr_truth{-1.03216414351651, -1.1021549879458, -0.171394026278093, 0.154479446468373},
                  hess_truth{-0.782510675917859, -0.422515184613313, 0.341958572815166, 0.549001418475123, -0.422515184613313, -0.411823557000097, 0.208809432913472, 0.367567511381604, 0.341958572815166, 0.208809432913472, 0.0899748010215663, -0.0496726848474764, 0.549001418475123, 0.367567511381604, -0.0496726848474764, -0.235561384187514};

    arma::mat X{1, 1, 1, .5, 0, 1},
             C1{1, 1, 0, 1, 1, 0, 0, 0, 1},
             C2{1, .5, .5, .5, 1, .5, .5, .5, 1};
    X.reshape(n_mem, n_fixef);
    C1.reshape(n_mem, n_mem);
    C2.reshape(n_mem, n_mem);
    std::vector<arma::mat> const scale_mats{C1, C2};

    arma::vec const lb{-Inf, 0,-Inf}, ub{0, Inf, 0};

    arma::vec const beta{1, -.5},
                    sigs{.25, .75},
                      mu = X * beta;

    std::vector<unsigned> seeds{ 1L };
    parallelrng::set_rng_seeds(seeds);

    pedmod::pedigree_l_factor_Hessian l_factor{scale_mats, 1L, X.t(), 8};
    pedmod::cdf<pedmod::pedigree_l_factor_Hessian>::alloc_mem(n_mem, 1);

    arma::mat vcov_mat;
    l_factor.setup(vcov_mat, sigs.begin(), 100);

    constexpr double eps{1e-4};
    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor_Hessian>(
        l_factor, lb, ub, mu, vcov_mat, true, false, true).approximate(
            1000000L, -1, eps, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.gradient.size() == hess_dim);
      expect_true(res.hessian.n_rows == hess_dim);
      expect_true(res.hessian.n_cols == hess_dim);

      expect_true
        (std::abs(res.likelihood - likelihood_truth) <
          eps * 10 * likelihood_truth);

      for(size_t i = 0; i < hess_dim; ++i)
        expect_true
          (std::abs(res.gradient[i] - gr_truth[i])
             < std::abs(gr_truth[i]) * 1e-3);

      for(size_t i = 0; i < hess_dim * hess_dim; ++i)
        expect_true
          (std::abs(res.hessian.begin()[i] - hess_truth[i])
             < std::abs(hess_truth[i]) * 1e-3);
    }
    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor_Hessian>(
        l_factor, lb, ub, mu, vcov_mat, true, false, false).approximate(
            1000000L, -1, eps, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.gradient.size() == hess_dim);
      expect_true(res.hessian.n_rows == hess_dim);
      expect_true(res.hessian.n_cols == hess_dim);

      expect_true
        (std::abs(res.likelihood - likelihood_truth) <
          eps * 10 * likelihood_truth);

      for(size_t i = 0; i < hess_dim; ++i)
        expect_true
          (std::abs(res.gradient[i] - gr_truth[i])
             < std::abs(gr_truth[i]) * 1e-3);

      for(size_t i = 0; i < hess_dim * hess_dim; ++i)
        expect_true
          (std::abs(res.hessian.begin()[i] - hess_truth[i])
             < std::abs(hess_truth[i]) * 1e-3);
    }
  }

  test_that("cdf<pedigree_l_factor_Hessian> gives similar output as R with a univariate example") {
    /*
     X <- matrix(c(1, .5), 1)
     C1 <- matrix(1)
     C2 <- matrix(.75)

     lbs <- -Inf
     ubs <- 0

     beta <- c(.5, -.75)
     sigs <- c(.35, .55)

     library(mvtnorm)
     f <- \(x){
     set.seed(1)
     S <- 1 + x[3] * C1 + x[4] * C2
     pmvnorm(
     lower = lbs, upper = ubs, mean = drop(X %*% head(x, 2)), sigma = S,
     algorithm = GenzBretz(maxpts = 1000000L, abseps = 0, releps = 1e-10)) |>
     log()
     }

     f(c(beta, sigs)) |> exp() |> dput()
     numDeriv::grad(f, c(beta, sigs)) |> dput()
     numDeriv::hessian(f, c(beta, sigs)) |> dput()

     lbs <- 0
     ubs <- Inf

     f(c(beta, sigs)) |> exp() |> dput()
     numDeriv::grad(f, c(beta, sigs)) |> dput()
     numDeriv::hessian(f, c(beta, sigs), method.args = list(eps = 1e-3, r = 5)) |> dput()
     */
    constexpr arma::uword n_mem{1},
                        n_fixef{2},
                       n_scales{2},
                       hess_dim{n_fixef + n_scales};

    constexpr double Inf{std::numeric_limits<double>::infinity()},
                     eps{1e-8};

    arma::mat X{1, .5},
             C1{1.},
             C2{.75};
    X.reshape(n_mem, n_fixef);
    std::vector<arma::mat> const scale_mats{C1, C2};

    arma::vec const beta{.5, -.75},
                    sigs{.35, .55},
                    mu = X * beta;

    std::vector<unsigned> seeds{ 1L };
    parallelrng::set_rng_seeds(seeds);

    {
      constexpr double likelihood_truth{0.462492837757237};
      arma::vec const gr_truth{-0.646867642116195, -0.323433821049096, 0.0229385688575181, 0.017203926656904},
                    hess_truth{-0.37256060867889, -0.186280304344205, 0.196719920053968, 0.147539940039962, -0.186280304344205, -0.093140152171208, 0.0983599600280048, 0.0737699700222857, 0.196719920053968, 0.0983599600280048, -0.0199906745261838, -0.0149930059061747, 0.147539940039962, 0.0737699700222857, -0.0149930059061747, -0.0112447544265877};

      arma::vec const lb{-Inf}, ub{0};

      pedmod::pedigree_l_factor_Hessian l_factor{scale_mats, 1L, X.t(), 8};
      pedmod::cdf<pedmod::pedigree_l_factor_Hessian>::alloc_mem(n_mem, 1);

      arma::mat vcov_mat;
      l_factor.setup(vcov_mat, sigs.begin(), 100);

      {
        auto const res = pedmod::cdf<pedmod::pedigree_l_factor_Hessian>(
          l_factor, lb, ub, mu, vcov_mat, true, false, true).approximate(
              1000000L, -1, eps, pedmod::cdf_methods::Korobov, 0, 8);

        expect_true(res.gradient.size() == hess_dim);
        expect_true(res.hessian.n_rows == hess_dim);
        expect_true(res.hessian.n_cols == hess_dim);

        expect_true
          (std::abs(res.likelihood - likelihood_truth) <
            eps * 10 * likelihood_truth);

        for(size_t i = 0; i < hess_dim; ++i)
          expect_true
            (std::abs(res.gradient[i] - gr_truth[i])
               < std::abs(gr_truth[i]) * 1e-3);

        for(size_t i = 0; i < hess_dim * hess_dim; ++i)
          expect_true
            (std::abs(res.hessian.begin()[i] - hess_truth[i])
               < std::abs(hess_truth[i]) * 1e-3);
      }
      {
        auto const res = pedmod::cdf<pedmod::pedigree_l_factor_Hessian>(
          l_factor, lb, ub, mu, vcov_mat, true, false, false).approximate(
              1000000L, -1, eps, pedmod::cdf_methods::Korobov, 0, 8);

        expect_true(res.gradient.size() == hess_dim);
        expect_true(res.hessian.n_rows == hess_dim);
        expect_true(res.hessian.n_cols == hess_dim);

        expect_true
          (std::abs(res.likelihood - likelihood_truth) <
            eps * 10 * likelihood_truth);

        for(size_t i = 0; i < hess_dim; ++i)
          expect_true
          (std::abs(res.gradient[i] - gr_truth[i])
             < std::abs(gr_truth[i]) * 1e-3);

        for(size_t i = 0; i < hess_dim * hess_dim; ++i)
          expect_true
          (std::abs(res.hessian.begin()[i] - hess_truth[i])
             < std::abs(hess_truth[i]) * 1e-3);
      }
    }
    {
      constexpr double likelihood_truth{0.537507162242763};
      arma::vec const gr_truth{0.556591004679402, 0.278295502349235, -0.019737269693756, -0.0148029522422098},
                    hess_truth{-0.349268085817092, -0.174634042912492, -0.14551276426383, -0.109134573187886, -0.174634042912492, -0.0873170214682717, -0.0727563821223562, -0.0545672865770353, -0.14551276426383, -0.0727563821223562, 0.0163584780014546, 0.0122688584866644, -0.109134573187886, -0.0545672865770353, 0.0122688584866644, 0.00920164385139158};

      arma::vec const lb{0}, ub{Inf};

      pedmod::pedigree_l_factor_Hessian l_factor{scale_mats, 1L, X.t(), 8};
      pedmod::cdf<pedmod::pedigree_l_factor_Hessian>::alloc_mem(n_mem, 1);

      arma::mat vcov_mat;
      l_factor.setup(vcov_mat, sigs.begin(), 100);

      {
        auto const res = pedmod::cdf<pedmod::pedigree_l_factor_Hessian>(
          l_factor, lb, ub, mu, vcov_mat, true, false, true).approximate(
              1000000L, -1, eps, pedmod::cdf_methods::Korobov, 0, 8);

        expect_true(res.gradient.size() == hess_dim);
        expect_true(res.hessian.n_rows == hess_dim);
        expect_true(res.hessian.n_cols == hess_dim);

        expect_true
          (std::abs(res.likelihood - likelihood_truth) <
            eps * 10 * likelihood_truth);

        for(size_t i = 0; i < hess_dim; ++i)
          expect_true
          (std::abs(res.gradient[i] - gr_truth[i])
             < std::abs(gr_truth[i]) * 1e-3);

        for(size_t i = 0; i < hess_dim * hess_dim; ++i)
          expect_true
          (std::abs(res.hessian.begin()[i] - hess_truth[i])
             < std::abs(hess_truth[i]) * 1e-3);
      }
      {
        auto const res = pedmod::cdf<pedmod::pedigree_l_factor_Hessian>(
          l_factor, lb, ub, mu, vcov_mat, true, false, false).approximate(
              1000000L, -1, eps, pedmod::cdf_methods::Korobov, 0, 8);

        expect_true(res.gradient.size() == hess_dim);
        expect_true(res.hessian.n_rows == hess_dim);
        expect_true(res.hessian.n_cols == hess_dim);

        expect_true
          (std::abs(res.likelihood - likelihood_truth) <
            eps * 10 * likelihood_truth);

        for(size_t i = 0; i < hess_dim; ++i)
          expect_true
          (std::abs(res.gradient[i] - gr_truth[i])
             < std::abs(gr_truth[i]) * 1e-3);

        for(size_t i = 0; i < hess_dim * hess_dim; ++i)
          expect_true
          (std::abs(res.hessian.begin()[i] - hess_truth[i])
             < std::abs(hess_truth[i]) * 1e-3);
      }
    }
  }

  test_that("cdf<likelihood> gives similar output to R") {
    /*
     set.seed(1)
     n <- 4
     mean <- rnorm(n)
     sigma <- drop(rWishart(1L, 2L * n, diag(n)))

     lower <- c(-Inf, -1 ,  1, -Inf)
     upper <- c(   1, Inf,  3,    2)
     mean  <- round(mean , 3)
     sigma <- round(sigma, 3)

     library(mvtnorm)
     prob <- pmvnorm(lower, upper, mean, sigma = sigma,
     algorithm = GenzBretz(abseps = 1e-9, maxpts = 1000000L))

     dput(mean)
     dput(sigma)
     dput(prob)
     */
    std::vector<unsigned> seeds = { 1L };
    parallelrng::set_rng_seeds(seeds);
    constexpr double const Inf = std::numeric_limits<double>::infinity();

    arma::vec lower{-Inf, -1, 1, -Inf},
              upper{1, Inf, 3, 2},
              mean{-0.626, 0.18, -0.836, 1.595};
    arma::mat sigma{{8.287, -0.848, -0.879, -1.788, -0.848, 3.581, 2.916, -3.957, -0.879, 2.916, 7.361, -0.648, -1.788, -3.957, -0.648, 11.735}};
    sigma.reshape(4L, 4L);

    double constexpr E_prop(0.0693596863013216);
    constexpr double abs_eps{E_prop * 1e-3};
    pedmod::cdf<pedmod::likelihood>::alloc_mem(4, 1);
    pedmod::likelihood::alloc_mem(4, 1, 8);
    pedmod::likelihood func;

    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, false, false, false).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, false, false).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, false).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, false).approximate(
            1000000L, abs_eps / 100, -1, pedmod::cdf_methods::Korobov, 0, 2);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, false).approximate(
            100000L, abs_eps / 100, 100000L, pedmod::cdf_methods::Korobov, 0, 1);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, false).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Sobol, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, false).approximate(
            1000000L, abs_eps / 100, 1000000L,
            pedmod::cdf_methods::Sobol, 0, 1);

      expect_true(res.inform == 0L);
      expect_true(std::abs(res.likelihood - E_prop) < 100. * abs_eps);
    }



    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, false, false, true).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, false, true).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, true).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, true).approximate(
            1000000L, abs_eps / 100, -1, pedmod::cdf_methods::Korobov, 0, 2);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, true).approximate(
            100000L, abs_eps / 100, 100000L, pedmod::cdf_methods::Korobov, 0, 1);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
    {
      auto res = pedmod::cdf<pedmod::likelihood>(
        func, lower, upper, mean, sigma, true, true, true).approximate(
            1000000L, abs_eps, -1, pedmod::cdf_methods::Sobol, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                        < 10. * abs_eps);
      expect_true(std::abs(res.likelihood - E_prop) < 10. * abs_eps);
    }
  }

  test_that("cdf<likelihood> gives similar output to R (1D)") {
/*
 lbs <- c(-1, -Inf, -.5)
 ubs <- c(Inf, 1, 2)
 mu <- .5
 va <- .8

 dput(mapply(function(l, u){
 f <- function(mu, va)
 pnorm(u, mean = mu, sd = sqrt(va)) - pnorm(l, mean = mu, sd = sqrt(va))
 f(mu, va)
 }, l = lbs, u = ubs))
 */
    constexpr double const Inf = std::numeric_limits<double>::infinity();
    arma::vec lbs{-1, -Inf, -.5},
              ubs{Inf, 1, 2},
           expect{0.953233743655453, 0.711924938984711, 0.821457505013967};
    double const mu(.5);
    double const va(.8);

    double const eps = std::pow(std::numeric_limits<double>::epsilon(), .5);
    pedmod::cdf<pedmod::likelihood>::alloc_mem(1, 1);
    pedmod::likelihood::alloc_mem(1, 1, 8);
    pedmod::likelihood func;
    for(size_t i = 0; i < 3; ++i){
      arma::vec l(1), u(1), m(1);
      arma::mat s(1, 1);
      l[0] = lbs[i];
      u[0] = ubs[i];
      m[0] = mu;
      s[0] = va;

      auto res = pedmod::cdf<pedmod::likelihood>(
        func, l, u, m, s, false, false, false).approximate(
            1000000L, 1e-8, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                           <= 0);
      expect_true(std::abs(res.likelihood - expect[i]) <  eps);
    }

    for(size_t i = 0; i < 3; ++i){
      arma::vec l(1), u(1), m(1);
      arma::mat s(1, 1);
      l[0] = lbs[i];
      u[0] = ubs[i];
      m[0] = mu;
      s[0] = va;

      auto res = pedmod::cdf<pedmod::likelihood>(
        func, l, u, m, s, true, false, false).approximate(
            1000000L, 1e-8, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(res.inform == 0L);
      expect_true(res.abserr                           <= 0);
      expect_true(std::abs(res.likelihood - expect[i]) <  eps);
    }
  }

  test_that("cdf<pedigree_l_factor> gives similar output to R (1D)") {
/*
 sigs <- list(matrix(.5, 1, 1), matrix(1.5, 1, 1))

 lws <- c(-Inf, 1  )
 ubs <- c(   2, Inf)
 mu <- .5
 xs <- c(.33, .5)

 f <- function(x, lw, ub, use_log){
 va <- sigs[[1]] * x[2] + sigs[[2]] * x[3] + 1
 vub <- if(is.finite(ub))
 pnorm(ub, x[1], sqrt(va)) else 1
 vlb <- if(is.finite(lw))
 pnorm(lw, x[1], sqrt(va)) else 0
 if(use_log) log(vub - vlb) else vub - vlb
 }

 library(numDeriv)
 dput(mapply(function(lw, ub){
 arg <- c(mu, xs)
 o <- f(arg, lw, ub, use_log = FALSE)
 do <- numDeriv::grad(f, arg, lw = lw, ub = ub, use_log = TRUE)
 c(o, do)
 }, lw = lws, ub = ubs))
 */
    constexpr double const Inf = std::numeric_limits<double>::infinity();
    arma::vec lbs{-Inf, 1},
              ubs{2, Inf};
    arma::mat expect
      {0.860805198822333, -0.186117393334307, -0.0364459647473255, -0.109337894242028, 0.358932107909215, 0.752428372350956, 0.0491141235094509, 0.14734237051237};
    expect.reshape(4, 2);

    std::vector<arma::mat> scales;
    arma::mat s1(1, 1), s2(1, 1);
    s1.at(0, 0) =  .5;
    s2.at(0, 0) = 1.5;
    scales.emplace_back(s1);
    scales.emplace_back(s2);
    arma::mat X(1, 1, arma::fill::ones);

    pedmod::pedigree_l_factor func(scales, 1L, X, 8);
    arma::vec const par{.5, .33, .5};

    arma::mat sig(1, 1);
    double const eps = std::pow(std::numeric_limits<double>::epsilon(), .5);
    pedmod::cdf<pedmod::pedigree_l_factor>::alloc_mem(1, 1);
    for(int i = 0; i < 2; ++i){
      func.setup(sig, par.begin() + 1, 1.);
      arma::vec lower(1), upper(1), mu(1);
      lower[0] = lbs[i];
      upper[0] = ubs[i];
      mu   [0] = par[0];

      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lower, upper, mu, sig, true, false, false).approximate(
            1000000L, 1e-8, -1, pedmod::cdf_methods::Korobov, 0, 8);

      arma::vec const ex_res = expect.col(i);
      expect_true(res.inform == 0L);
      expect_true(res.abserr                           <= 0);

      expect_true(std::abs(res.likelihood - ex_res[0]) <  eps);
      for(int j = 0; j < 3; ++j)
        expect_true(std::abs(res.derivs[j] - ex_res[j + 1]) <  eps);
    }
  }

  test_that("cdf<pedigree_l_factor> gives similar output to R with one scale matrix") {
    /*
     sigs <- list(matrix(c(1, .25, 0, .25, 1, .1, 0, .1, 1), 3))

     lws <- c(-Inf, -1  , -1.5)
     ubs <- c(   2, Inf , 1)
     mu <- c(.5, -.25, 0)
     sc <- .5

     library(mvtnorm)
     f <- function(x){
     set.seed(1)
     mu <- x[1:3]
     Sigma <- sigs[[1L]] * x[4]
     diag(Sigma) <- diag(Sigma) + 1
     pmvnorm(lower = lws, upper = ubs, mean = mu, sigma = Sigma,
     algorithm = GenzBretz(maxpts = 1000000L, abseps = 1e-10,
     releps = 0))
     }

     dput(f(c(mu, sc)))
     library(numDeriv)
     dput(grad(f, c(mu, sc)))
     */
    std::vector<unsigned> seeds = { 1L };
    parallelrng::set_rng_seeds(seeds);

    constexpr double const Inf = std::numeric_limits<double>::infinity();
    arma::vec lbs{-Inf, -1, -1.5},
              ubs{2, Inf, 1},
           expect{0.438847591008297, -0.0800167903883939, 0.166356462364297, -0.0550185761207399, -0.186593525090286},
               mu{.5, -.25, 0};

    arma::mat s1{1, .25, 0, .25, 1, .1, 0, .1, 1};
    s1.reshape(3, 3);
    std::vector<arma::mat> scales;
    scales.emplace_back(s1);

    arma::mat X(3, 3, arma::fill::zeros);
    X.diag().ones();
    pedmod::pedigree_l_factor func(scales, 1L, X, 8);

    arma::mat sig(3, 3);
    double const scalar = .5;
    double const eps =
      std::pow(std::numeric_limits<double>::epsilon(), .25);
    constexpr unsigned const n_deriv = 4;

    {
      // set the normalization constant
      pedmod::cdf<pedmod::likelihood>::alloc_mem(3, 1);
      pedmod::likelihood::alloc_mem(3, 1, 8);
      pedmod::likelihood lfunc;
      func.setup(sig, &scalar, 1., true);

      auto const norm_const = pedmod::cdf<pedmod::likelihood>(
        lfunc, lbs, ubs, mu, sig, false, false, false).approximate(
            1000000L, eps, -1, pedmod::cdf_methods::Korobov, 0, 8);

      func.setup(sig, &scalar, norm_const.likelihood);
    }

    pedmod::cdf<pedmod::pedigree_l_factor>::alloc_mem(3, 1);
    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, false, false, false).approximate(
            10000000L, eps / 10000, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);

      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }

    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, false, false).approximate(
            10000000L, eps / 1000, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }

    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, true, false).approximate(
            10000000L, eps / 1000, -1, pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }
    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, true, false).approximate(
            10000000L, eps / 1000, -1, pedmod::cdf_methods::Korobov, 0, 2);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }
    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, true, false).approximate(
            1000000L, eps / 1000, 1000000L, pedmod::cdf_methods::Korobov, 0, 1);

      expect_true(std::abs(res.likelihood - expect[0]) <  100 * eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) < 100 * eps);
    }
    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, true, false).approximate(
            10000000L, eps / 1000, -1, pedmod::cdf_methods::Sobol, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }
  }

  test_that("cdf<pedigree_l_factor> gives similar output to R with two scale matrices") {
    /*
     sigs <- list(matrix(c(1, .25, 0, .25, 1, .1, 0, .1, 1), 3),
     matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), 3))

     lws <- c(-Inf, -1  , -1.5)
     ubs <- c(   2, Inf , 1)
     mu <- c(.5, -.25, 0)
     sc <- c(.5, .67)

     library(mvtnorm)
     f <- function(x){
     set.seed(1)
     mu <- x[1:3]
     Sigma <- sigs[[1L]] * x[4] + sigs[[2L]] * x[5]
     diag(Sigma) <- diag(Sigma) + 1
     pmvnorm(lower = lws, upper = ubs, mean = mu, sigma = Sigma,
     algorithm = GenzBretz(maxpts = 1000000L, abseps = 1e-10,
     releps = 0))
     }

     dput(f(c(mu, sc)))
     library(numDeriv)
     dput(grad(f, c(mu, sc)))
     */
    std::vector<unsigned> seeds = { 1L };
    parallelrng::set_rng_seeds(seeds);

    constexpr double const Inf = std::numeric_limits<double>::infinity();
    arma::vec lbs{-Inf, -1, -1.5},
              ubs{2, Inf, 1},
           expect{0.355656058666919, -0.065516198469146, 0.123908592190096, -0.0184193612120618, -0.116799569758003, -0.0926881442370132},
               mu{.5, -.25, 0};

    arma::mat s1{{1, .25, 0, .25, 1, .1, 0, .1, 1}},
              s2{{1, 0, 1, 0, 1, 0, 1, 0, 1}};
    s1.reshape(3, 3);
    s2.reshape(3, 3);
    std::vector<arma::mat> scales;
    scales.emplace_back(s1);
    scales.emplace_back(s2);

    arma::mat X(3, 3, arma::fill::zeros);
    X.diag().ones();
    pedmod::pedigree_l_factor func(scales, 1L, X, 8);

    arma::mat sig(3, 3);
    double const scs[2] = { .5, .67 };
    double const eps =
      std::pow(std::numeric_limits<double>::epsilon(), .25);
    constexpr unsigned const n_deriv = 5;

    {
      pedmod::cdf<pedmod::likelihood>::alloc_mem(3, 1);
      pedmod::likelihood::alloc_mem(3, 1, 8);
      pedmod::likelihood lfunc;
      func.setup(sig, scs, 1., true);

      auto const norm_const = pedmod::cdf<pedmod::likelihood>(
        lfunc, lbs, ubs, mu, sig, false, false, false).approximate(
            1000000L, 1e-8, -1, pedmod::cdf_methods::Korobov, 0, 8);

      func.setup(sig, scs, norm_const.likelihood);
    }

    pedmod::cdf<pedmod::pedigree_l_factor>::alloc_mem(3, 1);
    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, false, false, false).approximate(
            10000000L, eps / 1000, -1,
            pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }

    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, false, false).approximate(
            10000000L, eps / 1000, -1,
            pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }

    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, true, false).approximate(
            10000000L, eps / 1000, -1,
            pedmod::cdf_methods::Korobov, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }

    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, true, false).approximate(
            10000000L, eps / 1000, -1,
            pedmod::cdf_methods::Sobol, 0, 8);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }

    {
      auto const res = pedmod::cdf<pedmod::pedigree_l_factor>(
        func, lbs, ubs, mu, sig, true, true, false).approximate(
            10000000L, eps / 10000, -1,
            pedmod::cdf_methods::Sobol, 0, 2);

      expect_true(std::abs(res.likelihood - expect[0]) <  eps);
      expect_true(res.derivs.n_elem == n_deriv);
      for(unsigned i = 0; i < n_deriv; ++i)
        expect_true(std::abs(res.derivs[i] - expect[i + 1] / expect[0]) <  eps);
    }
  }

  test_that("cdf<generic_l_factor> gives similar output to R with a multivariate example") {
    /*
     set.seed(1)
     n <- 4
     dput(Sig <- rWishart(1, n, diag(1/n, n)) |> round(3) |> drop())
     dput(mu <- runif(n) |> round(2))

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

     library(mvtnorm)
     f <- \(x){
     mu <- head(x, n)
     Sig <- tail(x, -n) |> upper_to_full()

     set.seed(1)
     pmvnorm(upper = numeric(n), mean = mu, sigma = Sig,
     algorithm = GenzBretz(maxpts = 1000000L, abseps = 0)) |>
     log()
     }

     f(c(mu, Sig[upper.tri(Sig, TRUE)])) |> exp() |> dput()
     gr <- numDeriv::grad(f, c(mu, Sig[upper.tri(Sig, TRUE)]))
     dput(c(head(gr, n), tail(gr, -n) |> d_upper_to_full()))
     */

    constexpr unsigned dim{4}, n_sequences{8};
    arma::mat Sigma{0.415, 0.41, -0.496, -0.002, 0.41, 1.791, -1.036, 1.414, -0.496, -1.036, 1.227, -0.309, -0.002, 1.414, -0.309, 1.695};
    Sigma.reshape(dim, dim);
    arma::vec const mean{0.21, 0.65, 0.13, 0.27};
    constexpr double const Inf = std::numeric_limits<double>::infinity();
    arma::vec const lb{-Inf, -Inf, -Inf, -Inf},
                    ub{0, 0, 0, 0};

    constexpr double true_likelihood{0.0069965920887278},
                     true_gr[]{-3.07835212049791, -1.56457774584047, -2.93000348516626, -0.0831394115021516, 2.95017357215864, 2.53497453851452, 3.91200497767273, 0.0400758740756828, 2.53497453851452, 0.529085992133384, 1.84325384191507, 0.304926573412647, 3.91200497767273, 1.84325384191507, 3.35128377934077, 0.23174602446223, 0.0400758740756828, 0.304926573412647, 0.23174602446223, -0.205458808913187};

    double const eps =
      std::pow(std::numeric_limits<double>::epsilon(), .25);

    std::vector<unsigned> seeds{ 1L };
    parallelrng::set_rng_seeds(seeds);

    pedmod::generic_l_factor::alloc_mem(dim, 1, n_sequences);
    pedmod::likelihood::alloc_mem(dim, 1, n_sequences);

    pedmod::cdf<pedmod::likelihood>::alloc_mem(dim, 1);
    pedmod::cdf<pedmod::generic_l_factor>::alloc_mem(dim, 1);

    pedmod::likelihood lfunc;
    auto const norm_const = pedmod::cdf<pedmod::likelihood>(
      lfunc, lb, ub, mean, Sigma, true, true, false).approximate(
          1000000L, 1e-8, -1, pedmod::cdf_methods::Korobov, 0, n_sequences);

    expect_true(
      std::abs(true_likelihood - norm_const.likelihood) <
        std::abs(true_likelihood) * eps);

    pedmod::generic_l_factor l_factor(mean, Sigma, norm_const.likelihood);
    auto const res = pedmod::cdf<pedmod::generic_l_factor>(
      l_factor, lb, ub, mean, Sigma, true, true, false).approximate(
          1000000L, -1, -1, pedmod::cdf_methods::Korobov, 0, n_sequences);

    expect_true(
      std::abs(true_likelihood - res.likelihood) <
        std::abs(true_likelihood) * eps);

    expect_true(res.derivs.size() == dim * (dim + 1));
    for(unsigned i = 0; i < dim * (dim + 1); ++i)
      expect_true(
        std::abs(res.derivs[i] - true_gr[i]) <
          std::abs(true_gr[i]) * 5e-3);
  }

  test_that("cdf<generic_l_factor> gives similar output to R with a univariate example") {
    /*
     mu <- 1.5
     Sig <- .67
     f <- \(x) pnorm(0, x[1], sqrt(x[2]), log = TRUE)
     dput(f(c(mu, Sig)) |> exp())
     dput(numDeriv::grad(f, c(mu, Sig)))

     f <- \(x) pnorm(0, x[1], sqrt(x[2]), log = TRUE, lower.tail = FALSE)
     dput(f(c(mu, Sig)) |> exp())
     dput(numDeriv::grad(f, c(mu, Sig)))
     */
    constexpr unsigned dim{4}, n_sequences{1};
    constexpr double mu{1.5},
                     Sig{.67};
    constexpr double Inf = std::numeric_limits<double>::infinity();

    std::vector<unsigned> seeds{ 1L };
    parallelrng::set_rng_seeds(seeds);

    pedmod::generic_l_factor::alloc_mem(dim, 1, n_sequences);
    pedmod::cdf<pedmod::generic_l_factor>::alloc_mem(dim, 1);

    arma::vec mu_vec{mu};
    arma::mat Sig_mat{Sig};

    {
      constexpr double true_likelihood{0.0334353800612323},
                             true_gr[]{-2.71919649074598, 3.04387666878185};

      arma::vec const lb{-Inf}, ub{0};
      pedmod::generic_l_factor l_factor(mu_vec, Sig_mat, 1);
      auto const res = pedmod::cdf<pedmod::generic_l_factor>(
        l_factor, lb, ub, mu_vec, Sig_mat, true, true, false).approximate(
            1000000L, -1, -1, pedmod::cdf_methods::Korobov, 0, n_sequences);

      expect_true(
        std::abs(true_likelihood - res.likelihood) <
          true_likelihood * 1e-8);

      expect_true(res.derivs.size() == 2);
      for(unsigned i = 0; i < 2; ++i)
        expect_true(
          std::abs(res.derivs[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }
    {
      constexpr double true_likelihood{0.966564619938768},
                       true_gr[]{0.0940623795382624, -0.105293708440383};

      arma::vec const lb{0}, ub{Inf};

      pedmod::generic_l_factor l_factor(mu_vec, Sig_mat, 1);
      auto const res = pedmod::cdf<pedmod::generic_l_factor>(
        l_factor, lb, ub, mu_vec, Sig_mat, true, true, false).approximate(
            1000000L, -1, -1, pedmod::cdf_methods::Korobov, 0, n_sequences);

      expect_true(
        std::abs(true_likelihood - res.likelihood) <
          true_likelihood * 1e-8);

      expect_true(res.derivs.size() == 2);
      for(unsigned i = 0; i < 2; ++i)
        expect_true(
          std::abs(res.derivs[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }
  }
}
