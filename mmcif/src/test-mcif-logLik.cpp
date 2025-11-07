#include "mmcif-logLik.h"
#include <testthat.h>

context("mcif functions work") {
  /*
   set.seed(123)
   n_cov_traject <- 4L
   n_cov_risk <- 2L
   n_causes <- 3L

   g <- \(x) cbind(x, log(x))
   d_g <- \(x) cbind(1, 1/x)

   dput(covs_risk <- runif(n_cov_risk, -1) |> round(3))
   covs_traject <- runif(n_cov_traject, -1) |> round(3)

   coefs_risk <- runif(n_causes * n_cov_risk, -1) |>
   round(3) |> matrix(n_cov_risk)
   coefs_traject <- runif(n_causes * n_cov_traject, -1) |>
   round(3) |> matrix(n_cov_traject)
   coefs_traject <- rbind(-.5, -.25, coefs_traject)
   c(coefs_risk, coefs_traject) |> dput()

# sample a plausible value
   etas <- covs_risk %*% coefs_risk |> drop()
   phats <- c(exp(etas), 1) / (1 + sum(exp(etas)))
   dput(cause <- sample(n_causes + 1, 1, prob = phats))
   stopifnot(cause < n_causes + 1) # we did not implement this case

   g_w_covs <- \(ti)
   cbind(g(ti), rep(covs_traject, each = length(ti)) |> matrix(length(ti)))
   rng <- runif(1)
   obs_time <- uniroot(
   \(ti){
   lp <- -g_w_covs(ti) %*% coefs_traject[, cause]
   pnorm(lp) - rng
   },
   c(1e-16, 1000))$root

   d_g_w_covs <- \(ti)
   cbind(d_g(ti), matrix(0., length(ti), length(covs_traject)))

# the matrices we will need
   dput(covs_traject_w_time <- g_w_covs(obs_time) |> drop() |> rep(n_causes))
   dput(d_covs_traject_w_time <- d_g_w_covs(obs_time) |> drop() |> rep(n_causes))

   get_n_remove <- \(x, n){
   out <- x[1:n]
   eval(substitute(out <- out[-(1:n)], list(out = substitute(x), n = n)),
   parent.frame())
   out
   }
   */

  constexpr size_t n_causes{3}, n_cov_risk{2}, n_cov_traject{6}, cause{1};
  constexpr double covs_risk[]{-0.425, 0.577},
                covs_traject[]{0.00846107419528661, -4.77227913998632, -0.182, 0.766, 0.881, -0.909, 0.00846107419528661, -4.77227913998632, -0.182, 0.766, 0.881, -0.909, 0.00846107419528661, -4.77227913998632, -0.182, 0.766, 0.881, -0.909},
              d_covs_traject[]{1, 118.188302917503, 0, 0, 0, 0, 1, 118.188302917503, 0, 0, 0, 0, 1, 118.188302917503, 0, 0, 0, 0},
                         par[]{0.056, 0.785, 0.103, -0.087, 0.914, -0.093, -0.5, -0.25, 0.355, 0.145, -0.794, 0.8, -0.5, -0.25, -0.508, -0.916, -0.344, 0.909, -0.5, -0.25, 0.779, 0.386, 0.281, 0.989};
  constexpr double gr_eps{1e-5};

  test_that("works with an observed cause") {
    /*
     f1 <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(n_cov_risk)
     coefs_traject <- matrix(x, 2L + n_cov_traject)

     log(-sum(d_covs_traject_w_time * coefs_traject[, cause])) +
     dnorm(-sum(covs_traject_w_time * coefs_traject[, cause]), log = TRUE)
     }

     par <- c(coefs_risk, coefs_traject)
     f1(par) |> dput()
     numDeriv::grad(f1, par) |> dput()

     f2 <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(n_cov_risk)
     coefs_traject <- matrix(x, 2L + n_cov_traject)

     out <- log(-sum(d_covs_traject_w_time * coefs_traject[, cause])) +
     dnorm(-sum(covs_traject_w_time * coefs_traject[, cause]), log = TRUE)

     etas <- covs_risk %*% coefs_risk |> drop()
     denom <- exp(etas) |> sum() + 1

     out + etas[cause] - log(denom)
     }
     f2(par) |> dput()
     numDeriv::grad(f2, par) |> dput()
     */

    ghqCpp::simple_mem_stack<double> mem;
    param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};
    mmcif_data const dat{covs_traject, d_covs_traject, covs_risk, true, cause,
                         nullptr};

    {
      constexpr double truth{2.33273860259647},
                      offset{-1},
                 true_grad[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.0286300077774044, -6.55678707593064, -0.10004644690055, 0.421074606123595, 0.484290767579494, -0.499682528669878, 0, 0, 0, 0, 0, 0};

      double res = mcif_logLik<false>(par, indexer, dat, mem);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
      res = mcif_logLik_grad<false>(par, grad.data(), indexer, dat, mem);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < grad.size(); ++i)
        expect_true
          (std::abs(grad[i] - offset - true_grad[i]) <
            (std::abs(true_grad[i]) + gr_eps) * gr_eps);
    }

    constexpr double truth{0.830481658506154},
                    offset{2},
               true_grad[]{0.15964478037357, -0.216741266624087, -0.330383467378871, 0.448544142550531, 0.0667994752319878, -0.0906901110958889, 0, 0, 0, 0, 0, 0, -0.0286300077774044, -6.55678707593064, -0.10004644690055, 0.421074606123595, 0.484290767579494, -0.499682528669878, 0, 0, 0, 0, 0, 0};

    double res = mcif_logLik<true>(par, indexer, dat, mem);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
    res = mcif_logLik_grad<true>(par, grad.data(), indexer, dat, mem);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    for(size_t i = 0; i < grad.size(); ++i)
      expect_true
        (std::abs(grad[i] - offset - true_grad[i]) <
          (std::abs(true_grad[i]) + gr_eps) * gr_eps);
  }

  test_that("works with a censored observation with probablity one for the trajectory") {
    /*
     f1 <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(n_cov_risk)
     coefs_traject <- matrix(x, 2L + n_cov_traject)

     etas <- covs_risk %*% coefs_risk |> drop()
     denom <- exp(etas) |> sum() + 1

     - log(denom)
     }

     par <- c(coefs_risk, coefs_traject)
     f1(par) |> dput()
     numDeriv::grad(f1, par) |> dput()
     */

    ghqCpp::simple_mem_stack<double> mem;
    param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};
    mmcif_data const dat
      {covs_traject, d_covs_traject, covs_risk, false, n_causes, nullptr};

    {
      constexpr double offset{3};

      double res = mcif_logLik<false>(par, indexer, dat, mem);
      expect_true(res == 0);

      std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
      res = mcif_logLik_grad<false>(par, grad.data(), indexer, dat, mem);
      expect_true(res == 0);

      for(size_t i = 0; i < grad.size(); ++i)
        expect_true(grad[i] == offset);
    }

    constexpr double truth{-1.40828294409031},
                    offset{2},
               true_grad[]{0.15964478037357, -0.216741266624087, 0.0946165325639635, -0.128455857365894, 0.0667994752319878, -0.0906901110958889, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    double res = mcif_logLik<true>(par, indexer, dat, mem);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
    res = mcif_logLik_grad<true>(par, grad.data(), indexer, dat, mem);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    for(size_t i = 0; i < grad.size(); ++i)
      expect_true
        (std::abs(grad[i] - offset - true_grad[i]) <
          (std::abs(true_grad[i]) + gr_eps) * gr_eps);
  }

  test_that("works with a censored observation with probablity for the trajectory in (0, 1)") {
    /*
     f1 <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(n_cov_risk)
     coefs_traject <- matrix(x, 2L + n_cov_traject)

     etas <- covs_risk %*% coefs_risk |> drop()
     denom <- exp(etas) |> sum() + 1

     integrand_terms <- sapply(1:n_causes, \(cause){
     lp_traject <- -sum(covs_traject_w_time * coefs_traject[, cause])
     pnorm(lp_traject) * exp(etas[cause]) / denom
     })

     log(1 - sum(integrand_terms))
     }

     par <- c(coefs_risk, coefs_traject)
     f1(par) |> dput()
     numDeriv::grad(f1, par) |> dput()
     */

    ghqCpp::simple_mem_stack<double> mem;
    param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};
    mmcif_data const dat
      {covs_traject, d_covs_traject, covs_risk, true, n_causes, nullptr};

    {
      constexpr double offset{3};

      double res = mcif_logLik<false>(par, indexer, dat, mem);
      expect_true(res == 0);

      std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
      res = mcif_logLik_grad<false>(par, grad.data(), indexer, dat, mem);
      expect_true(res == 0);

      for(size_t i = 0; i < grad.size(); ++i)
        expect_true(grad[i] == offset);
    }

    constexpr double truth{-0.532001042543614},
                    offset{7},
               true_grad[]{0.0443874774426018, -0.0602625281392007, 0.0477034585344386, -0.0647644600256424, -0.0190907533895791, 0.0259185051915334, 0.00211928952787073, -1.19533772377672, -0.0455864921686501, 0.191864027517853, 0.220668679188212, -0.227681985710412, 0.00109987164955784, -0.620357940705621, -0.0236585375711419, 0.0995738448324722, 0.114522920758956, -0.118162695760887, 0.000711200267143155, -0.401136552678053, -0.0152981103003366, 0.0643865520743632, 0.0740529404153219, -0.0764064958610772};

    double res = mcif_logLik<true>(par, indexer, dat, mem);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
    res = mcif_logLik_grad<true>(par, grad.data(), indexer, dat, mem);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    for(size_t i = 0; i < grad.size(); ++i)
      expect_true
      (std::abs(grad[i] - offset - true_grad[i]) <
        (std::abs(true_grad[i]) + gr_eps) * gr_eps);
  }
}

context("mcif functions work with different basis and left truncation") {
  /*
   set.seed(4321)
   n_cov_traject <- 1L
   n_cov_risk <- 2L
   n_causes <- 3L

   g <- \(x) cbind(x, log(x))
   d_g <- \(x) cbind(1, 1/x)

   dput(covs_risk <- runif(n_cov_risk, -1) |> round(3))
   covs_traject <- runif(n_cov_traject * n_causes, -1) |>
   round(3) |>  matrix(n_cov_traject)

   coefs_risk <- runif(n_causes * n_cov_risk, -1) |>
   round(3) |> matrix(n_cov_risk)
   coefs_traject <- runif(n_causes * n_cov_traject, -1) |>
   round(3) |> matrix(n_cov_traject)
   coefs_traject <- rbind(-.5, -.25, coefs_traject)
   c(coefs_risk, coefs_traject) |> dput()

# sample a plausible value
   etas <- covs_risk %*% coefs_risk |> drop()
   phats <- c(exp(etas), 1) / (1 + sum(exp(etas)))
   dput(cause <- sample(n_causes + 1, 1, prob = phats))
   stopifnot(cause < n_causes + 1) # we did not implement this case

   g_w_covs <- \(ti)
   sapply(ti, \(ti_val)
   rbind(outer(g(ti_val) |> drop(), seq_len(n_causes) / n_causes),
   covs_traject),
   simplify = "array") |>
   aperm(c(3, 1, 2))

   rng <- runif(1)
   obs_time <- uniroot(
   \(ti){
   lp <- -g_w_covs(ti)[, , cause] %*% coefs_traject[, cause]
   pnorm(lp) - rng
   },
   c(1e-16, 1000))$root

   d_g_w_covs <- \(ti)
   sapply(ti, \(ti_val)
   rbind(outer(d_g(ti_val) |> drop(), seq_len(n_causes) / n_causes),
   matrix(0, NROW(covs_traject), NCOL(covs_traject))),
   simplify = "array") |>
   aperm(c(3, 1, 2))

# the matrices we will need
   dput(covs_traject_w_time <- g_w_covs(obs_time))
   dput(d_covs_traject_w_time <- d_g_w_covs(obs_time))

   dput(covs_traject_w_time_delayed <- g_w_covs(obs_time / 2))

   get_n_remove <- \(x, n){
   out <- x[1:n]
   eval(substitute(out <- out[-(1:n)], list(out = substitute(x), n = n)),
   parent.frame())
   out
   }
   */

  constexpr size_t n_causes{3}, n_cov_risk{2}, n_cov_traject{3}, cause{0};
  constexpr double covs_risk[]{-0.33, 0.818},
                covs_traject[]{0.0285194046230662, -0.819519423584967, -0.177, 0.0570388092461325, -1.63903884716993, -0.912, 0.0855582138691987, -2.4585582707549, 0.527},
              d_covs_traject[]{0.333333333333333, 3.89598284324791, 0, 0.666666666666667, 7.79196568649581, 0, 1, 11.6879485297437, 0},
          covs_traject_delay[]{0.0142597023115331, -1.05056848377161, -0.177, 0.0285194046230662, -2.10113696754323, -0.912, 0.0427791069345994, -3.15170545131484, 0.527},
                         par[]{0.501, 0.6, 0.77, -0.102, -0.025, 0.892, -0.5, -0.25, -0.788, -0.5, -0.25, -0.234, -0.5, -0.25, 0.252};
  constexpr double gr_eps{1e-5};

  test_that("works with an observed cause") {
     /*
      f1 <- \(x){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(n_cov_risk)
      coefs_traject <- matrix(x, 2L + n_cov_traject)

      log(-sum(d_covs_traject_w_time[, , cause] * coefs_traject[, cause])) +
      dnorm(-sum(covs_traject_w_time[, , cause] * coefs_traject[, cause]),
      log = TRUE)
      }

      par <- c(coefs_risk, coefs_traject)
      f1(par) |> dput()
      numDeriv::grad(f1, par) |> dput()

      f_numerator <-
      \(x, covs_traject_w_time, d_covs_traject_w_time){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(n_cov_risk)
      coefs_traject <- matrix(x, 2L + n_cov_traject)

      out <- log(-sum(d_covs_traject_w_time[, , cause] * coefs_traject[, cause])) +
      dnorm(-sum(covs_traject_w_time[, , cause] * coefs_traject[, cause]),
      log = TRUE)

      etas <- covs_risk %*% coefs_risk |> drop()
      denom <- exp(etas) |> sum() + 1

      out + etas[cause] - log(denom)
      }

      f_denom <- \(x, covs_traject_w_time, d_covs_traject_w_time){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(n_cov_risk)
      coefs_traject <- matrix(x, 2L + n_cov_traject)

      etas <- covs_risk %*% coefs_risk |> drop()
      denom <- exp(etas) |> sum() + 1

      integrand_terms <- sapply(1:n_causes, \(cause){
      lp_traject <- -sum(covs_traject_w_time[, , cause] * coefs_traject[, cause])
      pnorm(lp_traject) * exp(etas[cause]) / denom
      })

      log(1 - sum(integrand_terms))
      }

      f2 <- \(x)
      f_numerator(x, covs_traject_w_time, d_covs_traject_w_time) -
      f_denom(x, covs_traject_w_time_delayed, d_covs_traject_w_time_delayed)

      f2(par) |> dput()
      numDeriv::grad(f2, par) |> dput()
      */
     ghqCpp::simple_mem_stack<double> mem;
     param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};
     mmcif_data const dat{covs_traject, d_covs_traject, covs_risk, true, cause,
                          covs_traject_delay};

     {
        constexpr double truth{-0.84181114196222},
                        offset{-1.5},
                   true_grad[]{0, 0, 0, 0, 0, 0, -0.301642012571094, -3.14502405683958, 0.0584270191857755, 0, 0, 0, 0, 0, 0};

        double res = mcif_logLik<false>(par, indexer, dat, mem);
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
        res = mcif_logLik_grad<false>(par, grad.data(), indexer, dat, mem);
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        for(size_t i = 0; i < grad.size(); ++i)
           expect_true
           (std::abs(grad[i] - offset - true_grad[i]) <
              (std::abs(true_grad[i]) + gr_eps) * gr_eps);
     }

     constexpr double truth{-1.9412504366145},
                     offset{2},
                true_grad[]{-0.258166266445342, 0.639939412064124, 0.0433606250125264, -0.107481791637932, 0.135430649616397, -0.335703851659884, -0.303394527488351, -3.01590937513574, 0.0801802880213681, -0.00150200312223692, 0.110658491997305, 0.0480313974435862, -0.00572991705612512, 0.422145577481862, -0.0705874082720095};

     double res = mcif_logLik<true>(par, indexer, dat, mem);
     expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

     std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
     res = mcif_logLik_grad<true>(par, grad.data(), indexer, dat, mem);
     expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

     for(size_t i = 0; i < grad.size(); ++i)
        expect_true
        (std::abs(grad[i] - offset - true_grad[i]) <
           (std::abs(true_grad[i]) + gr_eps) * gr_eps);
  }

  test_that("works with a censored observation with probablity one for the trajectory") {
     /*
      f_numerator <-
      \(x, covs_traject_w_time, d_covs_traject_w_time){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(n_cov_risk)
      coefs_traject <- matrix(x, 2L + n_cov_traject)

      etas <- covs_risk %*% coefs_risk |> drop()
      denom <- exp(etas) |> sum() + 1

      - log(denom)
      }

      f_denom <- \(x, covs_traject_w_time, d_covs_traject_w_time){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(n_cov_risk)
      coefs_traject <- matrix(x, 2L + n_cov_traject)

      etas <- covs_risk %*% coefs_risk |> drop()
      denom <- exp(etas) |> sum() + 1

      integrand_terms <- sapply(1:n_causes, \(cause){
      lp_traject <- -sum(covs_traject_w_time[, , cause] * coefs_traject[, cause])
      pnorm(lp_traject) * exp(etas[cause]) / denom
      })

      log(1 - sum(integrand_terms))
      }

      f2 <- \(x)
      f_numerator(x, covs_traject_w_time, d_covs_traject_w_time) -
      f_denom(x, covs_traject_w_time_delayed, d_covs_traject_w_time_delayed)

      f2(par) |> dput()
      numDeriv::grad(f2, par) |> dput()
      */

      ghqCpp::simple_mem_stack<double> mem;
      param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};
      mmcif_data const dat
         {covs_traject, d_covs_traject, covs_risk, false, n_causes,
          covs_traject_delay};

      {
         constexpr double offset{3};

         double res = mcif_logLik<false>(par, indexer, dat, mem);
         expect_true(res == 0);

         std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
         res = mcif_logLik_grad<false>(par, grad.data(), indexer, dat, mem);
         expect_true(res == 0);

         for(size_t i = 0; i < grad.size(); ++i)
            expect_true(grad[i] == offset);
      }

      constexpr double truth{-1.42490929465228},
                      offset{2},
                 true_grad[]{0.0718337335534868, -0.178060587963202, 0.0433606250153466, -0.107481791743748, 0.135430649526937, -0.335703851643137, -0.00175251489356466, 0.129114681755032, 0.0217532688369285, -0.00150200312223692, 0.110658491997305, 0.0480313974435862, -0.00572991705612512, 0.422145577481862, -0.0705874082720095};

      double res = mcif_logLik<true>(par, indexer, dat, mem);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
      res = mcif_logLik_grad<true>(par, grad.data(), indexer, dat, mem);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < grad.size(); ++i)
         expect_true
         (std::abs(grad[i] - offset - true_grad[i]) <
            (std::abs(true_grad[i]) + gr_eps) * gr_eps);
  }

  test_that("works with a censored observation with probablity for the trajectory in (0, 1)") {
     /*
      f_inner <- \(x, covs_traject_w_time, d_covs_traject_w_time){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(n_cov_risk)
      coefs_traject <- matrix(x, 2L + n_cov_traject)

      etas <- covs_risk %*% coefs_risk |> drop()
      denom <- exp(etas) |> sum() + 1

      integrand_terms <- sapply(1:n_causes, \(cause){
      lp_traject <- -sum(covs_traject_w_time[, , cause] * coefs_traject[, cause])
      pnorm(lp_traject) * exp(etas[cause]) / denom
      })

      log(1 - sum(integrand_terms))
      }

      f1 <- \(x)
      f_inner(x, covs_traject_w_time, d_covs_traject_w_time) -
      f_inner(x, covs_traject_w_time_delayed, d_covs_traject_w_time_delayed)

      par <- c(coefs_risk, coefs_traject)
      f1(par) |> dput()
      numDeriv::grad(f1, par) |> dput()
      */

     ghqCpp::simple_mem_stack<double> mem;
     param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};
     mmcif_data const dat
        {covs_traject, d_covs_traject, covs_risk, true, n_causes,
         covs_traject_delay};

      {
         constexpr double offset{3};

         double res = mcif_logLik<false>(par, indexer, dat, mem);
         expect_true(res == 0);

         std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
         res = mcif_logLik_grad<false>(par, grad.data(), indexer, dat, mem);
         expect_true(res == 0);

         for(size_t i = 0; i < grad.size(); ++i)
            expect_true(grad[i] == offset);
      }

      constexpr double truth{-0.0445051305363254},
                      offset{7},
                 true_grad[]{-0.000483976791188073, 0.00119967584210192, 0.000492251796556363, -0.00122018778820951, 0.00360410788092993, -0.0089338190642162, 0.00199926468412229, 0.0213054096222737, -0.00153140338362324, 0.00191939712769174, 0.0123428363931866, -0.00667375985939779, 0.0082760619533798, 0.0196767325617522, 0.0156831039405668};

      double res = mcif_logLik<true>(par, indexer, dat, mem);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      std::vector<double> grad(indexer.n_par_wo_vcov(), offset);
      res = mcif_logLik_grad<true>(par, grad.data(), indexer, dat, mem);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < grad.size(); ++i)
         expect_true
         (std::abs(grad[i] - offset - true_grad[i]) <
            (std::abs(true_grad[i]) + gr_eps) * gr_eps);
  }
}
