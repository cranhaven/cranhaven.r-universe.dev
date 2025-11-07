#include <testthat.h>
#include "mmcif-logLik.h"

namespace {
constexpr double ghq_nodes[]{-3.43615911883774, -2.53273167423279, -1.75668364929988, -1.03661082978951, -0.342901327223705, 0.342901327223705, 1.03661082978951, 1.75668364929988, 2.53273167423279, 3.43615911883774},
               ghq_weights[]{7.6404328552326e-06, 0.00134364574678124, 0.0338743944554811, 0.240138611082314, 0.610862633735326, 0.610862633735326, 0.240138611082315, 0.033874394455481, 0.00134364574678124, 7.64043285523265e-06};

const ghqCpp::ghq_data ghq_dat_use{ghq_nodes, ghq_weights, 10};

/*
# util functions
 upper_to_full <- \(x){
 dim <- (sqrt(8 * length(x) + 1) - 1) / 2
 out <- matrix(0, dim, dim)
 out[upper.tri(out, TRUE)] <- x
 out[lower.tri(out)] <- t(out)[lower.tri(out)]
 out
 }
 get_n_remove <- \(x, n){
 out <- x[1:n]
 eval(substitute(out <- out[-(1:n)], list(out = substitute(x), n = n)),
 parent.frame())
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
 */

} // namespace

context("mmcif_logLik works as expected with singleton data") {
  test_that("mmcif_logLik works") {
    /*
     set.seed(321)
     n_cov_traject <- 4L
     n_cov_risk <- 2L
     n_causes <- 3L

     g <- \(x) cbind(x, log(x))
     d_g <- \(x) cbind(1, 1/x)

     dput(covs_risk <- runif(n_cov_risk, -1) |> round(3))
     covs_traject <- runif(n_cov_traject, -1) |> round(3)
     vcov <-
     drop(rWishart(1, 2 * n_causes, diag(1/2/n_causes, 2 * n_causes))) |>
     drop() |> round(3)

     coefs_risk <- runif(n_causes * n_cov_risk, -1) |>
     round(3) |> matrix(n_cov_risk)
     coefs_traject <- runif(n_causes * n_cov_traject, -1) |>
     round(3) |> matrix(n_cov_traject)
     coefs_traject <- rbind(-.5, -.25, coefs_traject)
     c(coefs_risk, coefs_traject, vcov) |> dput()

# sample a plausible value
     u <- mvtnorm::rmvnorm(1, sigma = vcov) |> drop()
     etas <- covs_risk %*% coefs_risk |> drop()
     etas <- etas + u[1:n_causes]
     phats <- c(exp(etas), 1) / (1 + sum(exp(etas)))
     dput(cause <- sample(n_causes + 1, 1, prob = phats))
     stopifnot(cause < n_causes + 1) # we did not implement this case

     g_w_covs <- \(ti)
     cbind(g(ti), rep(covs_traject, each = length(ti)) |> matrix(length(ti)))
     rng <- runif(1)
     obs_time <- uniroot(
     \(ti){
     lp <- -g_w_covs(ti) %*% coefs_traject[, cause] - u[cause + n_causes]
     pnorm(lp) - rng
     },
     c(1e-16, 1000))$root

     d_g_w_covs <- \(ti)
     cbind(d_g(ti), matrix(0., length(ti), length(covs_traject)))

# the matrices we will need
     covs_traject_w_time <- g_w_covs(obs_time) |> drop()
     rep(covs_traject_w_time, n_causes) |> dput()
     d_covs_traject_w_time <- d_g_w_covs(obs_time) |> drop()
     rep(d_covs_traject_w_time, n_causes) |> dput()

     dput(gl <- fastGHQuad::gaussHermiteData(10L))

# compute the log densities we need. Start with the censored case
# trajectory probability is one
     library(ghqCpp)
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |> matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)

     etas <- covs_risk %*% coefs_risk |> drop()
     res <- mixed_mult_logit_term(
     eta = as.matrix(etas), Sigma = vcov[1:n_causes, 1:n_causes],
     which_category = 0L, weights = gl$w, nodes = gl$x)
     log(res)
     }

     par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
     f(par) |> dput()

     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     gr <- c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov)))
     dput(gr + 1.5)

# then with the observed outcome
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |> matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)

     idx_cause <- cause + n_causes

     res <- log(-d_covs_traject_w_time %*% coefs_traject[, cause]) |> drop()
     res <- res + dnorm(-covs_traject_w_time %*% coefs_traject[, cause] |> drop(),
     sd = sqrt(1 + vcov[idx_cause, idx_cause]), log = TRUE)

     M <- solve(vcov)
     M[idx_cause, idx_cause] <- M[idx_cause, idx_cause] + 1
     M <- solve(M)

     mean_cond <- M[, idx_cause] *
     sum(-covs_traject_w_time %*% coefs_traject[, cause])
     etas <- covs_risk %*% coefs_risk |> drop()
     etas <- etas + mean_cond[1:n_causes]

     res_integral <- mixed_mult_logit_term(
     eta = as.matrix(etas), Sigma = M[1:n_causes, 1:n_causes],
     which_category = cause, weights = gl$w, nodes = gl$x)

     res + log(res_integral)
     }

     f(par) |> dput()

     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     gr <- c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov)))
     dput(gr + .5)

# the censored case
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |> matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)

     integrand <- 1
     etas <- covs_risk %*% coefs_risk |> drop()
     vcov_sub <- vcov[1:n_causes, 1:n_causes]

     for(cause in 1:n_causes){
     idx_cause <- cause + n_causes
     z <- -solve(vcov_sub, vcov[1:n_causes, idx_cause]) |> drop()
     s <- sqrt(1 + vcov[idx_cause, idx_cause] -
     sum(vcov[idx_cause, 1:n_causes] * (-z)))

     offset <- -covs_traject_w_time %*% coefs_traject[, cause] |> drop()

     integrand <- integrand - mixed_mult_logit_n_probit_term(
     eta = as.matrix(etas),
     which_category = cause, s = s, eta_probit = offset,
     Sigma = vcov_sub, z = z, weights = gl$w, nodes = gl$x)
     }
     log(integrand)
     }

     f(par) |> dput()

     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     gr <- c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov)))
     dput(gr + .5)
     */

    constexpr size_t n_causes{3}, n_cov_risk{2}, n_cov_traject{6};
    constexpr double covs_risk[]{0.912, 0.875},
                  covs_traject[]{4.1936419665673, 1.43356956082597, -0.524, -0.49, -0.219, -0.318, 4.1936419665673, 1.43356956082597, -0.524, -0.49, -0.219, -0.318, 4.1936419665673, 1.43356956082597, -0.524, -0.49, -0.219, -0.318},
                d_covs_traject[]{1, 0.238456217286129, 0, 0, 0, 0, 1, 0.238456217286129, 0, 0, 0, 0, 1, 0.238456217286129, 0, 0, 0, 0},
                           par[]{0.355, -0.737, -0.475, -0.365, 0.353, -0.874, -0.5, -0.25, -0.871, 0.322, -0.791, 0.474, -0.5, -0.25, -0.85, 0.782, 0.78, -0.123, -0.5, -0.25, -0.81, 0.091, -0.198, -0.974, 0.771, -0.126, -0.198, 0.068, 0.879, -0.145, -0.126, 1.158, 0.184, 1.053, -0.211, 0.595, -0.198, 0.184, 1.456, -0.43, -1.171, -0.332, 0.068, 1.053, -0.43, 2.234, 0.431, 0.462, 0.879, -0.211, -1.171, 0.431, 1.673, -0.001, -0.145, 0.595, -0.332, 0.462, -0.001, 0.832};

    ghqCpp::simple_mem_stack<double> mem;
    param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};

    {
      // the censored case with one as the probability of the trajectory
      mmcif_data dat{covs_traject, d_covs_traject, covs_risk, false, n_causes,
                     nullptr};
      double res{mmcif_logLik(par, indexer, dat, mem, ghq_dat_use)};
      constexpr double truth{-1.21423993502317};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      constexpr double shift{1.5},
                   true_gr[]{1.26365754060744, 1.2732459955188, 1.34146548646436, 1.34789725971731, 1.30061198121401, 1.30870118784666, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.46383170621554, 1.53592172902375, 1.54244818397882, 1.5, 1.5, 1.5, 1.53592172902375, 1.4623397748844, 1.53217128260594, 1.5, 1.5, 1.5, 1.54244818397882, 1.53217128260594, 1.46703163289562, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5};

      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, dat, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-6);
    }

    {
      // the observed case works
      constexpr unsigned cause{2L};
      mmcif_data dat{covs_traject, d_covs_traject, covs_risk, true, cause,
                     nullptr};
      double res{mmcif_logLik(par, indexer, dat, mem, ghq_dat_use)};
      constexpr double truth{-4.26186714415087};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      constexpr double shift{.5},
                   true_gr[]{0.351552206482173, 0.357574759929686, 0.282709259050858, 0.2915247825613, 1.10517471974935, 1.0806226750331, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 3.43029581567022, 1.68645040335615, -0.0894243337010308, -0.0511792441427352, 0.253656624124913, 0.142295919574298, 0.461770301601013, 0.532975866589409, 0.45954237109379, 0.500000003534147, 0.500000000567034, 0.402421270586787, 0.532975866589409, 0.460900127868904, 0.447644291654297, 0.499999999797272, 0.499999999555276, 0.393827579249467, 0.45954237109379, 0.447644291654297, 0.648183312792179, 0.500000000362377, 0.499999999926135, 0.852567814444038, 0.500000003534147, 0.499999999797272, 0.500000000362377, 0.499999999996471, 0.500000000000939, 0.499999999532829, 0.500000000567034, 0.499999999555276, 0.499999999926135, 0.500000000000939, 0.499999999824738, 0.5, 0.402421270586787, 0.393827579249467, 0.852567814444038, 0.499999999532829, 0.5, 0.846467447468548};

      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, dat, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }

    // the censored case with a probability of the trajectory in (0, 1) works
    mmcif_data dat{covs_traject, d_covs_traject, covs_risk, true, n_causes,
                   nullptr};
    double res{mmcif_logLik(par, indexer, dat, mem, ghq_dat_use)};
    constexpr double truth{-1.04608338904514};
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    double * gr{mem.get(indexer.n_par<false>())};
    constexpr double shift{.5},
                 true_gr[]{0.330648438629487, 0.33751906083885, 0.350726364390927, 0.356782421714224, 0.352693743921712, 0.358669984812303, 0.833300440713479, 0.613936613988158, 0.458353757359891, 0.461055994306309, 0.482594413812261, 0.47472613513888, 0.638228487237129, 0.547252520431373, 0.482728204306005, 0.48384889336008, 0.492781444202289, 0.489518261901478, 0.800680721846452, 0.60278577298994, 0.462429625797045, 0.464867397951781, 0.484297878241694, 0.47719965841653, 0.477058826417935, 0.524688031342338, 0.532398053180518, 0.524647165965919, 0.493874870084439, 0.494164701133886, 0.524688031342338, 0.468439391298966, 0.525877057569041, 0.491270491704209, 0.511791124947451, 0.491458569479165, 0.532398053180518, 0.525877057569041, 0.477259089716002, 0.494835460522858, 0.498832652888146, 0.523788667244225, 0.524647165965919, 0.491270491704209, 0.494835460522858, 0.527860742985394, 0.5, 0.5, 0.493874870084439, 0.511791124947451, 0.498832652888146, 0.5, 0.517999932390265, 0.5, 0.494164701133886, 0.491458569479165, 0.523788667244225, 0.5, 0.5, 0.540325615606095};

    std::fill(gr, gr + indexer.n_par<false>(), shift);
    res = mmcif_logLik_grad(par, gr, indexer, dat, mem, ghq_dat_use);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
  }
}

context("mmcif_logLik works as expected with singleton data with left-truncation and transition specific covariates") {
  /*
   set.seed(321)
   n_cov_traject <- 2L
   n_cov_risk <- 4L
   n_causes <- 3L

   g <- \(x) cbind(x, log(x))
   d_g <- \(x) cbind(1, 1/x)

   dput(covs_risk <- runif(n_cov_risk, -1) |> round(3))
   covs_traject <- runif(n_cov_traject * n_causes, -1) |>
   round(3) |> matrix(n_cov_traject)

   vcov <-
   drop(rWishart(1, 2 * n_causes, diag(1/2/n_causes, 2 * n_causes))) |>
   drop() |> round(3)

   coefs_risk <- runif(n_causes * n_cov_risk, -1) |>
   round(3) |> matrix(n_cov_risk)
   coefs_traject <- runif(n_causes * n_cov_traject, -1) |>
   round(3) |> matrix(n_cov_traject)
   coefs_traject <- rbind(-.5, -.25, coefs_traject)
   c(coefs_risk, coefs_traject, vcov) |> dput()

# sample a plausible value
   u <- mvtnorm::rmvnorm(1, sigma = vcov) |> drop()
   etas <- covs_risk %*% coefs_risk |> drop()
   etas <- etas + u[1:n_causes]
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
   lp <- -g_w_covs(ti)[, , cause] %*% coefs_traject[, cause] - u[cause + n_causes]
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

   dput(gl <- fastGHQuad::gaussHermiteData(10L))

# computes the weight when both are censored
   cens_interior <- \(x, covs_traject_w_time, ...){
   coefs_risk <- get_n_remove(x, length(coefs_risk)) |> matrix(NROW(coefs_risk))
   coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
   matrix(NROW(coefs_traject))
   vcov <- upper_to_full(x)

   integrand <- 1
   etas <- covs_risk %*% coefs_risk |> drop()
   vcov_sub <- vcov[1:n_causes, 1:n_causes]

   for(cause in 1:n_causes){
   idx_cause <- cause + n_causes
   z <- -solve(vcov_sub, vcov[1:n_causes, idx_cause]) |> drop()
   s <- sqrt(1 + vcov[idx_cause, idx_cause] -
   sum(vcov[idx_cause, 1:n_causes] * (-z)))

   offset <- -covs_traject_w_time[, , cause] %*% coefs_traject[, cause] |> drop()

   integrand <- integrand - mixed_mult_logit_n_probit_term(
   eta = as.matrix(etas),
   which_category = cause, s = s, eta_probit = offset,
   Sigma = vcov_sub, z = z, weights = gl$w, nodes = gl$x)
   }
   log(integrand)
   }

# compute the log densities we need. Start with the censored case
# trajectory probability is one
   library(ghqCpp)
   f_numerator <- \(x, ...){
   coefs_risk <- get_n_remove(x, length(coefs_risk)) |> matrix(NROW(coefs_risk))
   coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
   matrix(NROW(coefs_traject))
   vcov <- upper_to_full(x)

   etas <- covs_risk %*% coefs_risk |> drop()
   res <- mixed_mult_logit_term(
   eta = as.matrix(etas), Sigma = vcov[1:n_causes, 1:n_causes],
   which_category = 0L, weights = gl$w, nodes = gl$x)
   log(res)
   }

   f <- \(x)
   f_numerator(x, covs_traject_w_time, d_covs_traject_w_time) -
   cens_interior(x, covs_traject_w_time_delayed)

   par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
   f(par) |> dput()

   gr <- numDeriv::grad(f, par)
   dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
   gr <- c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov)))
   dput(gr + 1.5)

# then with the observed outcome
   f_numerator <- \(x, covs_traject_w_time, d_covs_traject_w_time){
   coefs_risk <- get_n_remove(x, length(coefs_risk)) |> matrix(NROW(coefs_risk))
   coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
   matrix(NROW(coefs_traject))
   vcov <- upper_to_full(x)

   idx_cause <- cause + n_causes

   res <- log(-d_covs_traject_w_time[, , cause] %*% coefs_traject[, cause]) |> drop()
   res <- res + dnorm(-covs_traject_w_time[, , cause] %*% coefs_traject[, cause] |> drop(),
   sd = sqrt(1 + vcov[idx_cause, idx_cause]), log = TRUE)

   M <- solve(vcov)
   M[idx_cause, idx_cause] <- M[idx_cause, idx_cause] + 1
   M <- solve(M)

   mean_cond <- M[, idx_cause] *
   sum(-covs_traject_w_time[, , cause] %*% coefs_traject[, cause])
   etas <- covs_risk %*% coefs_risk |> drop()
   etas <- etas + mean_cond[1:n_causes]

   res_integral <- mixed_mult_logit_term(
   eta = as.matrix(etas), Sigma = M[1:n_causes, 1:n_causes],
   which_category = cause, weights = gl$w, nodes = gl$x)

   res + log(res_integral)
   }

   f(par) |> dput()

   gr <- numDeriv::grad(f, par)
   dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
   gr <- c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov)))
   dput(gr + .5)

# the censored case
   f_numerator <- cens_interior
   f(par) |> dput()

   gr <- numDeriv::grad(f, par)
   dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
   gr <- c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov)))
   dput(gr + .5)
   */
  test_that("mmcif_logLik works"){
    constexpr size_t n_causes{3}, n_cov_risk{4}, n_cov_traject{4};
    constexpr double covs_risk[]{0.912, 0.875, -0.524, -0.49},
                  covs_traject[]{0.646443476980867, 0.220780924937263, -0.219, -0.318, 1.29288695396173, 0.441561849874527, -0.095, -0.42, 1.9393304309426, 0.66234277481179, -0.099, 0.613},
                d_covs_traject[]{0.333333333333333, 0.171880628496774, 0, 0, 0.666666666666667, 0.343761256993548, 0, 0, 1, 0.515641885490321, 0, 0},
          covs_traject_delayed[]{0.323221738490433, -0.010268135249385, -0.219, -0.318, 0.646443476980867, -0.0205362704987699, -0.095, -0.42, 0.9696652154713, -0.0308044057481549, -0.099, 0.613},
                           par[]{0.355, -0.737, -0.475, -0.365, 0.353, -0.874, -0.871, 0.322, -0.791, 0.474, -0.85, 0.782, -0.5, -0.25, 0.78, -0.123, -0.5, -0.25, -0.81, 0.091, -0.5, -0.25, -0.198, -0.974, 0.981, 0.094, -0.223, 0.076, 0.991, -0.164, 0.094, 1.062, 0.124, 1.031, 0.031, 0.535, -0.223, 0.124, 0.719, -0.255, -0.875, -0.192, 0.076, 1.031, -0.255, 2.234, 0.431, 0.462, 0.991, 0.031, -0.875, 0.431, 1.673, -0.001, -0.164, 0.535, -0.192, 0.462, -0.001, 0.832};

    ghqCpp::simple_mem_stack<double> mem;
    param_indexer indexer{n_cov_risk, n_cov_traject, n_causes};

    {
      // the censored case with one as the probability of the trajectory
      mmcif_data dat{covs_traject, d_covs_traject, covs_risk, false, n_causes,
                     covs_traject_delayed};
      double res{mmcif_logLik(par, indexer, dat, mem, ghq_dat_use)};
      constexpr double truth{-0.75638712073972};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      constexpr double shift{1.5},
                   true_gr[]{1.30417464552152, 1.31211931462335, 1.61251369033354, 1.60521318390837, 1.35706958086227, 1.36286829248988, 1.58212230245337, 1.57679375633388, 1.39106833135557, 1.39548770820763, 1.56258793237091, 1.55852688335918, 1.45603681113413, 1.50139662617811, 1.52978740974213, 1.54325295147888, 1.42114092873247, 1.50250520150164, 1.51158896649099, 1.55123543066876, 1.41723227471345, 1.50262937202203, 1.50845034411252, 1.44767615175435, 1.46961863607627, 1.54786159214875, 1.52869003296089, 1.46169513224578, 1.51543180015817, 1.50852876006363, 1.54786159214875, 1.46513042705358, 1.52488017859353, 1.51344692003613, 1.46153433866332, 1.51134352256414, 1.52869003296089, 1.52488017859353, 1.48086610840423, 1.51035408064529, 1.50987644367992, 1.46999091146995, 1.46169513224578, 1.51344692003613, 1.51035408064529, 1.49131553300509, 1.5, 1.5, 1.51543180015817, 1.46153433866332, 1.50987644367992, 1.5, 1.49158328152462, 1.5, 1.50852876006363, 1.51134352256414, 1.46999091146995, 1.5, 1.5, 1.46973788053261};

      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, dat, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-6);
    }

    {
      // the observed case works
      constexpr unsigned cause{2L};
      mmcif_data dat{covs_traject, d_covs_traject, covs_risk, true, cause,
                     covs_traject_delayed};
      double res{mmcif_logLik(par, indexer, dat, mem, ghq_dat_use)};
      constexpr double truth{-3.51406816774873};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      constexpr double shift{.5},
                   true_gr[]{0.390031630027409, 0.394493065906349, 0.563183580906668, 0.559083883003887, 0.278633545214557, 0.287614420807614, 0.627188621071066, 0.618935923967804, 1.25578484956451, 1.22512252584262, 0.065755196017286, 0.0939313854551962, 0.456036811114918, 0.501396626229439, 0.529787409725591, 0.543252951522483, 0.421140928747214, 0.502505201471765, 0.511588966492165, 0.551235430738901, 0.925882330271407, 0.399505291528696, 0.401314682503823, 1.1110515082584, 0.461799467389021, 0.546557207397991, 0.426404591994878, 0.461695129594625, 0.515431800241106, 0.397527055904203, 0.546557207397991, 0.470686489348978, 0.403807070891252, 0.513446919898789, 0.461534341431069, 0.376272952248039, 0.426404591994878, 0.403807070891252, 0.708015133280971, 0.51035408168976, 0.509876443648801, 0.845831687340938, 0.461695129594625, 0.513446919898789, 0.51035408168976, 0.491315532821543, 0.500000000243033, 0.500000000056414, 0.515431800241106, 0.461534341431069, 0.509876443648801, 0.500000000243033, 0.49158328147481, 0.500000000006266, 0.397527055904203, 0.376272952248039, 0.845831687340938, 0.500000000056414, 0.500000000006266, 0.771566088602655};

      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, dat, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }

    // the censored case with a probability of the trajectory in (0, 1) works
    mmcif_data dat{covs_traject, d_covs_traject, covs_risk, true, n_causes,
                   covs_traject_delayed};
    double res{mmcif_logLik(par, indexer, dat, mem, ghq_dat_use)};
    constexpr double truth{-0.132306129417663};
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    double * gr{mem.get(indexer.n_par<false>())};
    constexpr double shift{.5},
                 true_gr[]{0.497181984164887, 0.497296311468174, 0.501619122885401, 0.501514065650893, 0.477507649841467, 0.47842016840757, 0.512923236320337, 0.512084705892214, 0.466960044148786, 0.468300481124533, 0.518983483511735, 0.517751730591615, 0.552870976012486, 0.53446856068444, 0.49698224904445, 0.495618061121128, 0.583941975913944, 0.558106916375921, 0.499626513456892, 0.498348796579891, 0.52203736530258, 0.538423630304778, 0.503100196780054, 0.480803833052062, 0.501661504951786, 0.502101814699636, 0.504060493885629, 0.504070583978153, 0.497619073110274, 0.50346713528096, 0.502101814699636, 0.49569041079235, 0.506560987725158, 0.497943028481525, 0.501452896617694, 0.503181030460858, 0.504060493885629, 0.506560987725158, 0.492149471930446, 0.499260543775127, 0.501086204445222, 0.489545613389447, 0.504070583978153, 0.497943028481525, 0.499260543775127, 0.506201780938144, 0.5, 0.5, 0.497619073110274, 0.501452896617694, 0.501086204445222, 0.5, 0.511761172437985, 0.5, 0.50346713528096, 0.503181030460858, 0.489545613389447, 0.5, 0.5, 0.498979365647439};

    std::fill(gr, gr + indexer.n_par<false>(), shift);
    res = mmcif_logLik_grad(par, gr, indexer, dat, mem, ghq_dat_use);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
  }
}

context("mmcif_logLik works as expected with bivariate data") {
  /*
   set.seed(321)
   n_cov_traject <- 4L
   n_cov_risk <- 2L
   n_causes <- 3L

   g <- \(x) cbind(x, log(x))
   d_g <- \(x) cbind(1, 1/x)

   vcov <-
   drop(rWishart(1, 2 * n_causes, diag(1/2/n_causes, 2 * n_causes))) |>
   drop() |> round(3)

   coefs_risk <- runif(n_causes * n_cov_risk, -1) |>
   round(3) |> matrix(n_cov_risk)
   coefs_traject <- runif(n_causes * n_cov_traject, -1) |>
   round(3) |> matrix(n_cov_traject)
   coefs_traject <- rbind(-.5, -.25, coefs_traject)
   c(coefs_risk, coefs_traject, vcov) |> dput()

# sample a plausible value
   u <- mvtnorm::rmvnorm(1, sigma = vcov) |> drop()

   g_w_covs <- \(ti, covs_traject)
   cbind(g(ti), rep(covs_traject, each = length(ti)) |> matrix(length(ti)))
   d_g_w_covs <- \(ti, covs_traject)
   cbind(d_g(ti), matrix(0., length(ti), length(covs_traject)))

   sample_obs <- \(){
   covs_risk <- runif(n_cov_risk, -1) |> round(3)
   covs_traject <- runif(n_cov_traject, -1) |> round(3)

   etas <- covs_risk %*% coefs_risk |> drop()
   etas <- etas + u[1:n_causes]
   phats <- c(exp(etas), 1) / (1 + sum(exp(etas)))
   cause <- sample(n_causes + 1, 1, prob = phats)
   stopifnot(cause < n_causes + 1) # we did not implement this case

   rng <- runif(1)
   obs_time <- uniroot(
   \(ti){
   lp <- -g_w_covs(ti, covs_traject) %*% coefs_traject[, cause] -
   u[cause + n_causes]
   pnorm(lp) - rng
   },
   c(1e-16, 1000))$root

   list(covs_risk = covs_risk,
   covs_traject_w_time = g_w_covs(obs_time, covs_traject) |> drop(),
   d_covs_traject_w_time = d_g_w_covs(obs_time, covs_traject) |> drop(),
   cause = cause)
   }

   dput(obs <- replicate(2, sample_obs(), simplify = FALSE))

   obs[[1]]$covs_traject_w_time |> rep(n_causes) |> dput()
   obs[[1]]$d_covs_traject_w_time |> rep(n_causes) |> dput()
   obs[[2]]$covs_traject_w_time |> rep(n_causes) |> dput()
   obs[[2]]$d_covs_traject_w_time |> rep(n_causes) |> dput()


   dput(gl <- fastGHQuad::gaussHermiteData(10L))
   */

  constexpr size_t n_cov_risk{2},
                n_cov_traject{6},
                     n_causes{3},
                      cause[]{0, 2};

  param_indexer const indexer{n_cov_risk, n_cov_traject, n_causes};

  constexpr double covs_risk1[]{-0.605, -0.876},
                   covs_risk2[]{0.584, 0.576},
                covs_traject1[]{0.85860256800553, -0.152449132274265, -0.734, 0.062, 0.957, 0.198, 0.85860256800553, -0.152449132274265, -0.734, 0.062, 0.957, 0.198, 0.85860256800553, -0.152449132274265, -0.734, 0.062, 0.957, 0.198},
                covs_traject2[]{4.78877056139744, 1.5662737107113, 0.304, 0.267, -0.669, 0.204, 4.78877056139744, 1.5662737107113, 0.304, 0.267, -0.669, 0.204, 4.78877056139744, 1.5662737107113, 0.304, 0.267, -0.669, 0.204},
              d_covs_traject1[]{1, 1.16468321580138, 0, 0, 0, 0, 1, 1.16468321580138, 0, 0, 0, 0, 1, 1.16468321580138, 0, 0, 0, 0},
              d_covs_traject2[]{1, 0.208821865065129, 0, 0, 0, 0, 1, 0.208821865065129, 0, 0, 0, 0, 1, 0.208821865065129, 0, 0, 0, 0},
                          par[]{-0.647, 0.076, -0.439, -0.98, -0.806, 0.324, -0.5, -0.25, 0.355, -0.737, -0.475, -0.365, -0.5, -0.25, 0.353, -0.874, -0.871, 0.322, -0.5, -0.25, -0.791, 0.474, -0.85, 0.782, 1.974, -0.235, 0.154, 0.195, 0.108, 0.059, -0.235, 0.401, 0.163, -0.161, 0.597, -0.155, 0.154, 0.163, 0.4, 0.026, 0.047, 0.179, 0.195, -0.161, 0.026, 0.506, -0.493, 0.018, 0.108, 0.597, 0.047, -0.493, 2.031, 0.052, 0.059, -0.155, 0.179, 0.018, 0.052, 0.578};

  test_that("mmcif_logLik works when both individuals are observed") {
    /*
     library(ghqCpp)
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)

     etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
     covs_traject <- sapply(
     obs,
     \(x, cause) -x$covs_traject_w_time %*% coefs_traject[, x$cause])
     d_covs_traject <- sapply(
     obs,
     \(x) -x$d_covs_traject_w_time %*% coefs_traject[, x$cause])

     V <- matrix(0, 2, 2 * n_causes)
     V[1, obs[[1]]$cause + n_causes] <- 1
     V[2, obs[[2]]$cause + n_causes] <- 1
     out <- sum(log(d_covs_traject)) +
     mvtnorm::dmvnorm(
     covs_traject, sigma = diag(2) + V %*% tcrossprod(vcov, V), log = TRUE)

     vcov_cond <- solve(crossprod(V) + solve(vcov))
     mean_cond <- vcov_cond %*% crossprod(V, covs_traject) |> drop()

     integral <- mixed_mult_logit_term(
     eta = etas + mean_cond[1:n_causes],
     Sigma = vcov_cond[1:n_causes, 1:n_causes],
     which_category = sapply(obs, `[[`, "cause"),
     weights = gl$w, nodes = gl$x)

     out + log(integral)
     }

     par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
     f(par) |> dput()
     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
     */
    ghqCpp::simple_mem_stack<double> mem;

    mmcif_data
      obs1{covs_traject1, d_covs_traject1, covs_risk1, true, cause[0],
           nullptr},
      obs2{covs_traject2, d_covs_traject2, covs_risk2, true, cause[1],
           nullptr};

    double res{mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
    constexpr double truth{-7.77301433778719};
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    double * gr{mem.get(indexer.n_par<false>())};
    constexpr double shift{-4},
                 true_gr[]{-4.60044096320519, -4.76143069955199, -3.88925107716226, -3.81874883303431, -3.44226495101285, -3.38684934060633, -4.63995658752879, -5.58289389453021, -4.53343755123284, -3.95494124232041, -3.30449627176941, -3.85610267649002, -4, -4, -4, -4, -4, -4, 0.276190040738317, -2.38723521835543, -3.61357900292289, -3.66061050597717, -4.85038041763269, -3.7406911732045, -4.08660377973573, -4.01372576629899, -3.9001876680933, -3.91005332184739, -4.00000000811773, -3.8691233090845, -4.01372576629899, -4.04876332825872, -4.03306286597394, -4.13643808454184, -3.99999997350787, -4.22974199345051, -3.9001876680933, -4.03306286597394, -4.02777117647809, -3.81863162075935, -4.00000061380221, -3.66503522108475, -3.91005332184739, -4.13643808454184, -3.81863162075935, -4.0715034184455, -4.00000001334417, -3.53643481460751, -4.00000000811773, -3.99999997350787, -4.00000061380221, -4.00000001334417, -4.0000000095682, -3.99999984574, -3.8691233090845, -4.22974199345051, -3.66503522108475, -3.53643481460751, -3.99999984574, -3.51292697279014};

    std::fill(gr, gr + indexer.n_par<false>(), shift);
    res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
    expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

    for(size_t i = 0; i < indexer.n_par<false>(); ++i)
      expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
  }

  test_that("mmcif_logLik works when one individual is observed and one is censored") {
    /*
     library(ghqCpp)
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)
     vcov <- (vcov + t(vcov)) / 2

     etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
     covs_traject <- sapply(
     obs,
     \(x, cause) -x$covs_traject_w_time %*% coefs_traject[, x$cause])
     d_covs_traject <- sapply(
     obs,
     \(x) -x$d_covs_traject_w_time %*% coefs_traject[, x$cause])

     v <- numeric(2 * n_causes)
     v[n_causes + obs[[1]]$cause] <- 1
     out <- log(d_covs_traject[1]) + dnorm(
     covs_traject[1], sd = sqrt(1 + v %*% vcov %*% v), log = TRUE)

     vcov_cond <- solve(tcrossprod(v) + solve(vcov))
     mean_cond <- drop(vcov_cond %*% v) * covs_traject[1]

     integral <- mixed_mult_logit_term(
     eta = etas + mean_cond[1:n_causes],
     Sigma = vcov_cond[1:n_causes, 1:n_causes],
     which_category = c(obs[[1]]$cause, 0L),
     weights = gl$w, nodes = gl$x)

     out + log(integral)
     }

     par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
     f(par) |> dput()
     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
     */

    ghqCpp::simple_mem_stack<double> mem;

    {
      mmcif_data
        obs1{covs_traject1, d_covs_traject1, covs_risk1, true, cause[0],
             nullptr},
        obs2{covs_traject2, d_covs_traject2, covs_risk2, false, n_causes,
             nullptr};

      double res{mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
      constexpr double truth{-4.5240025456571};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      constexpr double shift{-4},
                   true_gr[]{-4.60677825999459, -4.76876219981359, -3.86894022626248, -3.78371293952945, -4.02220331100695, -3.97886624482647, -4.62087217947148, -5.58628242493471, -4.54975238220723, -3.95356315005794, -3.28322475503409, -3.85170167375117, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4.08651843041873, -4.01607930345544, -3.99983074431344, -3.90823923485969, -3.99999986891621, -4.00000003552164, -4.01607930345544, -4.03105028895141, -3.88643373484883, -4.16983704586129, -4.00000001555981, -3.9999999372787, -3.99983074431344, -3.88643373484883, -4.05931545957518, -4.12982271375971, -3.99999956557467, -4.00000006439723, -3.90823923485969, -4.16983704586129, -4.12982271375971, -4.05536084376252, -4.00000000252952, -3.99999957702496, -3.99999986891621, -4.00000001555981, -3.99999956557467, -4.00000000252952, -3.99999999300296, -4.00000024008675, -4.00000003552164, -3.9999999372787, -4.00000006439723, -3.99999957702496, -4.00000024008675, -4.00000001062483};

      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }

    /*
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)
     vcov <- (vcov + t(vcov)) / 2

     etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
     covs_traject <- sapply(
     obs,
     \(x, cause) -x$covs_traject_w_time %*% coefs_traject[, x$cause])
     d_covs_traject <- sapply(
     obs,
     \(x) -x$d_covs_traject_w_time %*% coefs_traject[, x$cause])

     v <- numeric(2 * n_causes)
     v[n_causes + obs[[1]]$cause] <- 1
     out <- log(d_covs_traject[1]) + dnorm(
     covs_traject[1], sd = sqrt(1 + v %*% vcov %*% v), log = TRUE)

     vcov_cond <- solve(tcrossprod(v) + solve(vcov))
     mean_cond <- drop(vcov_cond %*% v) * covs_traject[1]

     integral <- mixed_mult_logit_term(
     eta = as.matrix(etas[, 1]) + mean_cond[1:n_causes],
     Sigma = vcov_cond[1:n_causes, 1:n_causes],
     which_category = obs[[1]]$cause, weights = gl$w, nodes = gl$x)

     integrals <- sapply(1:n_causes, \(cause){
     idx_cause <- cause + n_causes
     rng_coefs <- solve(vcov_cond[1:n_causes, 1:n_causes],
     vcov_cond[1:n_causes, idx_cause])

     s <- sqrt(1 + vcov_cond[idx_cause, idx_cause] -
     vcov_cond[idx_cause, 1:n_causes] %*% rng_coefs)
     shift <- -obs[[2]]$covs_traject_w_time %*% coefs_traject[, cause] -
     mean_cond[idx_cause]

     rng_coefs <- -rng_coefs

     mixed_mult_logit_n_probit_term(
     eta = etas + mean_cond[1:n_causes],
     which_category = c(obs[[1]]$cause, cause), s = s, eta_probit = shift,
     Sigma = vcov_cond[1:n_causes, 1:n_causes], z = rng_coefs,
     weights = gl$w, nodes = gl$x)
     })
     integral <- integral - sum(integrals)

     out + log(integral)
     }

     par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
     f(par) |> dput()
     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
     */

    {
      mmcif_data
        obs1{covs_traject1, d_covs_traject1, covs_risk1, true, cause[0],
             nullptr},
        obs2{covs_traject2, d_covs_traject2, covs_risk2, true, n_causes,
             nullptr};

      constexpr double truth{-4.38551973954109},
                       shift{-4},
                   true_gr[]{-4.5671761432415, -4.72543744207062, -3.85884139924293, -3.77625459152193, -3.9998547663936, -3.95693855135666, -4.034641265227, -5.37854908528959, -4.48343114496485, -3.9213991068482, -3.40487582437673, -3.83268709528386, -3.87120856084756, -3.95787590094769, -3.99182408151401, -3.99281917690393, -4.01799239946641, -3.99451352851197, -3.70694231274736, -3.90414897823354, -3.98139615681146, -3.98366044018079, -4.04094069464688, -3.9875158420464, -4.06985265337591, -4.01580871407255, -3.99979185548028, -3.86305499015696, -3.99623191508629, -3.99371522788762, -4.01580871407255, -4.03410448588676, -3.89887558953613, -4.16294785134668, -3.99471714867857, -4.01026172632862, -3.99979185548028, -3.89887558953613, -4.05858870479936, -4.12763313213162, -4.0044183991572, -3.98484286950757, -3.86305499015696, -4.16294785134668, -4.12763313213162, -3.94364666714109, -3.98520159014463, -3.97776221043803, -3.99623191508629, -3.99471714867857, -4.0044183991572, -3.98520159014463, -3.98865659435786, -4.00000027578256, -3.99371522788762, -4.01026172632862, -3.98484286950757, -3.97776221043803, -4.00000027578256, -3.96110568888788};

      {
        double res
          {mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        double * gr{mem.get(indexer.n_par<false>())};
        std::fill(gr, gr + indexer.n_par<false>(), shift);
        res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        for(size_t i = 0; i < indexer.n_par<false>(); ++i)
          expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
      }

      double res
        {mmcif_logLik(par, indexer, obs2, obs1, mem, ghq_dat_use)};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, obs2, obs1, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }
  }

  test_that("mmcif_logLik works when both individuals are censored") {
    /*
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)
     vcov <- (vcov + t(vcov)) / 2

     etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
     mixed_mult_logit_term(
     eta = etas, Sigma = vcov[1:n_causes, 1:n_causes],
     which_category = integer(2), weights = gl$w, nodes = gl$x) |>
     log()
     }

     par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
     f(par) |> dput()
     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
     */

    ghqCpp::simple_mem_stack<double> mem;

    {
      mmcif_data
        obs1{covs_traject1, d_covs_traject1, covs_risk1, false, n_causes,
             nullptr},
        obs2{covs_traject2, d_covs_traject2, covs_risk2, false, n_causes,
             nullptr};

      double res
        {mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
      constexpr double truth{-3.01479043790748};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      constexpr double shift{-4},
                   true_gr[]{-4.00600061167854, -3.95429338506848, -3.81463703103875, -3.68922104267058, -4.03296854671522, -3.98498043300022, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4.00651859808217, -3.86236765460961, -3.89687944480354, -4, -4, -4, -3.86236765460961, -3.95860579328351, -3.82086704477511, -4, -4, -4, -3.89687944480354, -3.82086704477511, -4.05656998194764, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4};

      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }

    /*
     f <- function(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)
     vcov <- (vcov + t(vcov)) / 2

     etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
     integral <- mixed_mult_logit_term(
     eta = as.matrix(etas[, 2]), Sigma = vcov[1:n_causes, 1:n_causes],
     which_category = 0L, weights = gl$w, nodes = gl$x)

     integrals <- sapply(1:n_causes, \(cause){
     lp_traject <- -obs[[1]]$covs_traject_w_time %*% coefs_traject[, cause] |>
     drop()
     idx_cause <- cause + n_causes
     rng_coefs <-
     solve(vcov[1:n_causes, 1:n_causes], vcov[1:n_causes, idx_cause])
     s <- sqrt(1 + vcov[idx_cause, idx_cause] -
     vcov[idx_cause, 1:n_causes] %*% rng_coefs)
     rng_coefs <- -rng_coefs
     mixed_mult_logit_n_probit_term(
     eta = etas, which_category = c(cause, 0L), s = s,
     eta_probit = lp_traject, Sigma = vcov[1:n_causes, 1:n_causes],
     z = rng_coefs, weights = gl$w, nodes = gl$x)
     })
     log(integral - sum(integrals))
     }

     par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
     f(par) |> dput()
     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) + 1.5) |> dput()
     */

    {
      mmcif_data
        obs1{covs_traject1, d_covs_traject1, covs_risk1, true, n_causes,
             nullptr},
        obs2{covs_traject2, d_covs_traject2, covs_risk2, false, n_causes,
             nullptr};

      constexpr double truth{-2.18417107496657},
                       shift{1.5},
                   true_gr[]{1.42886432212848, 1.45568701354807, 1.52143756531699, 1.57829984536793, 1.34545167726186, 1.34168374815065, 1.60455529481686, 1.4814356903032, 1.41061803300473, 1.50754997550706, 1.61653752380935, 1.52411121171035, 1.68542942744349, 1.46707608763835, 1.34148055836095, 1.51338992552404, 1.70667998038838, 1.54276137510894, 1.62477679072493, 1.47784526361077, 1.39333113149789, 1.50901017690837, 1.63907644033356, 1.52877443596108, 1.48077385432297, 1.58360118439227, 1.5560064124667, 1.51232498904552, 1.46513088846104, 1.47203468864447, 1.58360118439227, 1.47288847995647, 1.58987619120269, 1.47503865108853, 1.52039508343806, 1.45664598170391, 1.5560064124667, 1.58987619120269, 1.43986715799987, 1.47858474718355, 1.4588547390977, 1.53606382712226, 1.51232498904552, 1.47503865108853, 1.47858474718355, 1.54560315711551, 1.5, 1.5, 1.46513088846104, 1.52039508343806, 1.4588547390977, 1.5, 1.55038611784376, 1.5, 1.47203468864447, 1.45664598170391, 1.53606382712226, 1.5, 1.5, 1.51294882404452};
      {
        double res
          {mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        double * gr{mem.get(indexer.n_par<false>())};
        std::fill(gr, gr + indexer.n_par<false>(), shift);
        res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        for(size_t i = 0; i < indexer.n_par<false>(); ++i)
          expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
      }

      double res
        {mmcif_logLik(par, indexer, obs2, obs1, mem, ghq_dat_use)};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, obs2, obs1, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }

    /*
     library(ghqCpp)
     f <- \(x){
     coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
     matrix(NROW(coefs_risk))
     coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
     matrix(NROW(coefs_traject))
     vcov <- upper_to_full(x)
     vcov <- (vcov + t(vcov)) / 2

     vcov_sub <- vcov[1:n_causes, 1:n_causes]

     integrals_substract <- sapply(obs, \(obs_i){
     sapply(1:n_causes, \(cause){
     etas <- obs_i$covs_risk %*% coefs_risk |> drop()

     idx_cause <- cause + n_causes
     z <- -solve(vcov_sub, vcov[1:n_causes, idx_cause]) |> drop()
     s <- sqrt(1 + vcov[idx_cause, idx_cause] -
     sum(vcov[idx_cause, 1:n_causes] * (-z)))

     offset <- -obs_i$covs_traject_w_time %*% coefs_traject[, cause] |>
     drop()

     mixed_mult_logit_n_probit_term(
     eta = as.matrix(etas),
     which_category = cause, s = s, eta_probit = offset,
     Sigma = vcov_sub, z = z, weights = gl$w, nodes = gl$x)
     })
     })

     vcov_sub <- vcov[1:n_causes, 1:n_causes]

     etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
     rng_coefs <- solve(vcov_sub, vcov[1:n_causes, -(1:n_causes)]) |> t()
     vcov_cond <- vcov[1:n_causes + n_causes, 1:n_causes + n_causes] -
     vcov[1:n_causes + n_causes, 1:n_causes] %*% t(rng_coefs)

     integrals_adds <- sapply(
     1:n_causes,
     \(cause1) sapply(
     1:n_causes, \(cause2){
     V <- matrix(0, 2, n_causes)
     V[1, cause1] <- 1
     V[2, cause2] <- 1

     shift <- -c(obs[[1]]$covs_traject_w_time %*% coefs_traject[, cause1],
     obs[[2]]$covs_traject_w_time %*% coefs_traject[, cause2])

     rng_coefs <- -V %*% rng_coefs
     vcov_cond_pbvn <- diag(2) + V %*% tcrossprod(vcov_cond, V)

     mixed_mult_logit_n_cond_pbvn(
     eta = etas, which_category = c(cause1, cause2),
     eta_pbvn = -shift, Psi = vcov_cond_pbvn, V = -rng_coefs,
     Sigma = vcov_sub, weights = gl$w, nodes = gl$x)
     }))

     log(1 - sum(integrals_substract) + sum(integrals_adds))
     }

     par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
     f(par) |> dput()
     gr <- numDeriv::grad(f, par)
     dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
     (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) + 1.5) |> dput()
     */

    {
      mmcif_data
        obs1{covs_traject1, d_covs_traject1, covs_risk1, true, n_causes,
             nullptr},
        obs2{covs_traject2, d_covs_traject2, covs_risk2, true, n_causes,
             nullptr};

      double res
        {mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
      constexpr double truth{-2.00247380641032};
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      double * gr{mem.get(indexer.n_par<false>())};
      constexpr double shift{1.5},
                   true_gr[]{1.4392362543624, 1.4657164371737, 1.55664825291928, 1.60435190353244, 1.37028357120591, 1.36660932696277, 1.77073755216239, 1.53802079510158, 1.42522865934166, 1.51674277488534, 1.5877749109731, 1.53035913222007, 2.06304090224125, 1.59159588972652, 1.36729663448639, 1.5344107677658, 1.65140759199596, 1.5584698669516, 1.94148079104312, 1.58523847207786, 1.4203664235269, 1.52654397141941, 1.58536679406214, 1.54084713201152, 1.48091073070106, 1.56488498397801, 1.54783934446057, 1.52202302260823, 1.4560063687567, 1.46367963873689, 1.56488498397801, 1.49314850571281, 1.56749376537132, 1.47422099404467, 1.56126698068742, 1.45113517043605, 1.54783934446057, 1.56749376537132, 1.45156009955787, 1.47872440662632, 1.45122709058752, 1.56317435473337, 1.52202302260823, 1.47422099404467, 1.47872440662632, 1.58411611553288, 1.50274810184889, 1.50547469903672, 1.4560063687567, 1.56126698068742, 1.45122709058752, 1.50274810184889, 1.58144204720316, 1.50857653786166, 1.46367963873689, 1.45113517043605, 1.56317435473337, 1.50547469903672, 1.50857653786166, 1.5622719445094};

      std::fill(gr, gr + indexer.n_par<false>(), shift);
      res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
      expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

      for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-3);
    }
  }
}

context("mmcif_logLik works as expected with bivariate data with left-truncation and transition specific covariates") {
  /*
   set.seed(54321)
   n_cov_traject <- 1L
   n_cov_risk <- 4L
   n_causes <- 3L

   g <- \(x) cbind(x, log(x))
   d_g <- \(x) cbind(1, 1/x)

   vcov <-
   drop(rWishart(1, 2 * n_causes, diag(1/2/n_causes, 2 * n_causes))) |>
   drop() |> round(3)

   coefs_risk <- runif(n_causes * n_cov_risk, -1) |>
   round(3) |> matrix(n_cov_risk)
   coefs_traject <- runif(n_causes * n_cov_traject, -1) |>
   round(3) |> matrix(n_cov_traject)
   coefs_traject <- rbind(-.5, -.25, coefs_traject)
   c(coefs_risk, coefs_traject, vcov) |> dput()

# sample a plausible value
   u <- mvtnorm::rmvnorm(1, sigma = vcov) |> drop()

   g_w_covs <- \(ti, covs_traject)
   sapply(ti, \(ti_val)
   rbind(outer(g(ti_val) |> drop(), seq_len(n_causes) / n_causes),
   covs_traject),
   simplify = "array") |>
   aperm(c(3, 1, 2))

   d_g_w_covs <- \(ti, covs_traject)
   sapply(ti, \(ti_val)
   rbind(outer(d_g(ti_val) |> drop(), seq_len(n_causes) / n_causes),
   matrix(0, NROW(covs_traject), NCOL(covs_traject))),
   simplify = "array") |>
   aperm(c(3, 1, 2))

   sample_obs <- \(){
   covs_risk <- runif(n_cov_risk, -1) |> round(3)
   covs_traject <- runif(n_cov_traject * n_causes, -1) |>
   round(3) |> matrix(n_cov_traject)

   etas <- covs_risk %*% coefs_risk |> drop()
   etas <- etas + u[1:n_causes]
   phats <- c(exp(etas), 1) / (1 + sum(exp(etas)))
   cause <- sample(n_causes + 1, 1, prob = phats)
   stopifnot(cause < n_causes + 1) # we did not implement this case

   rng <- runif(1)
   obs_time <- uniroot(
   \(ti){
   lp <- -g_w_covs(ti, covs_traject)[, , cause] %*% coefs_traject[, cause] -
   u[cause + n_causes]
   pnorm(lp) - rng
   },
   c(1e-16, 1000))$root

   list(covs_risk = covs_risk,
   covs_traject_w_time = g_w_covs(obs_time, covs_traject),
   d_covs_traject_w_time = d_g_w_covs(obs_time, covs_traject),
   covs_traject_w_time_delay = g_w_covs(obs_time / 2, covs_traject),
   cause = cause)
   }

   dput(obs <- replicate(2, sample_obs(), simplify = FALSE))

   dput(obs[[1]]$covs_traject_w_time)
   dput(obs[[1]]$d_covs_traject_w_time)
   dput(obs[[1]]$covs_traject_w_time_delay)

   dput(obs[[2]]$covs_traject_w_time)
   dput(obs[[2]]$d_covs_traject_w_time)
   dput(obs[[2]]$covs_traject_w_time_delay)

   dput(gl <- fastGHQuad::gaussHermiteData(10L))

   cens_interior <- \(x, obs){
   coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
   matrix(NROW(coefs_risk))
   coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
   matrix(NROW(coefs_traject))
   vcov <- upper_to_full(x)
   vcov <- (vcov + t(vcov)) / 2

   vcov_sub <- vcov[1:n_causes, 1:n_causes]

   integrals_substract <- sapply(obs, \(obs_i){
   sapply(1:n_causes, \(cause){
   etas <- obs_i$covs_risk %*% coefs_risk |> drop()

   idx_cause <- cause + n_causes
   z <- -solve(vcov_sub, vcov[1:n_causes, idx_cause]) |> drop()
   s <- sqrt(1 + vcov[idx_cause, idx_cause] -
   sum(vcov[idx_cause, 1:n_causes] * (-z)))

   offset <- -obs_i$covs_traject_w_time[, , cause] %*%
   coefs_traject[, cause] |>
   drop()

   mixed_mult_logit_n_probit_term(
   eta = as.matrix(etas),
   which_category = cause, s = s, eta_probit = offset,
   Sigma = vcov_sub, z = z, weights = gl$w, nodes = gl$x)
   })
   })

   vcov_sub <- vcov[1:n_causes, 1:n_causes]

   etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
   rng_coefs <- solve(vcov_sub, vcov[1:n_causes, -(1:n_causes)]) |> t()
   vcov_cond <- vcov[1:n_causes + n_causes, 1:n_causes + n_causes] -
   vcov[1:n_causes + n_causes, 1:n_causes] %*% t(rng_coefs)

   integrals_adds <- sapply(
   1:n_causes,
   \(cause1) sapply(
   1:n_causes, \(cause2){
   V <- matrix(0, 2, n_causes)
   V[1, cause1] <- 1
   V[2, cause2] <- 1

   shift <- -c(obs[[1]]$covs_traject_w_time[, , cause1] %*%
   coefs_traject[, cause1],
   obs[[2]]$covs_traject_w_time[, , cause2] %*%
   coefs_traject[, cause2])

   rng_coefs <- -V %*% rng_coefs
   vcov_cond_pbvn <- diag(2) + V %*% tcrossprod(vcov_cond, V)

   mixed_mult_logit_n_cond_pbvn(
   eta = etas, which_category = c(cause1, cause2),
   eta_pbvn = -shift, Psi = vcov_cond_pbvn, V = -rng_coefs,
   Sigma = vcov_sub, weights = gl$w, nodes = gl$x)
   }))

   log(1 - sum(integrals_substract) + sum(integrals_adds))
   }
   */

  constexpr size_t n_cov_risk{4},
                n_cov_traject{3},
                     n_causes{3},
                      cause[]{0, 0};

   param_indexer const indexer{n_cov_risk, n_cov_traject, n_causes};

   constexpr double covs_risk1[]{0.805, 0.401, -0.893, 0.511},
                    covs_risk2[]{0.563, 0.107, -0.645, 0.952},
                 covs_traject1[]{2.77707120320329, 0.706669712332181, -0.69, 5.55414240640658, 1.41333942466436, -0.743, 8.33121360960987, 2.12000913699654, -0.777},
                 covs_traject2[]{0.000169283787912925, -2.52844058142144, 0.363, 0.000338567575825851, -5.05688116284287, 0.447, 0.000507851363738777, -7.58532174426431, 0.635},
               d_covs_traject1[]{0.333333333333333, 0.0400101772626308, 0, 0.666666666666667, 0.0800203545252617, 0, 1, 0.120030531787893, 0},
               d_covs_traject2[]{0.333333333333333, 656.360024081357, 0, 0.666666666666667, 1312.72004816271, 0, 1, 1969.08007224407, 0},
           covs_traject_delay1[]{1.38853560160164, 0.475620652145532, -0.69, 2.77707120320329, 0.951241304291064, -0.743, 4.16560680480493, 1.4268619564366, -0.777},
           covs_traject_delay2[]{8.46418939564627e-05, -2.75948964160808, 0.363, 0.000169283787912925, -5.51897928321617, 0.447, 0.000253925681869388, -8.27846892482425, 0.635},
                           par[]{0.245, 0.756, -0.801, 0.896, 0.041, 0.534, -0.022, -0.798, 0.036, 0.19, -0.593, -0.388, -0.5, -0.25, -0.8, -0.5, -0.25, -0.341, -0.5, -0.25, -0.401, 0.742, -0.58, 0.161, -0.033, 0.184, 0.471, -0.58, 0.868, -0.022, -0.006, -0.734, -0.071, 0.161, -0.022, 0.408, -0.373, 0.006, 0.489, -0.033, -0.006, -0.373, 0.878, 0.464, -0.226, 0.184, -0.734, 0.006, 0.464, 1.565, 0.098, 0.471, -0.071, 0.489, -0.226, 0.098, 1.659};

  test_that("mmcif_logLik works when both individuals are observed and one is left truncated") {
     /*
      library(ghqCpp)
      f_numerator <- \(x){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(NROW(coefs_risk))
      coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
      matrix(NROW(coefs_traject))
      vcov <- upper_to_full(x)

      etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
      covs_traject <- sapply(
      obs,
      \(x, cause) -x$covs_traject_w_time[, , x$cause] %*% coefs_traject[, x$cause])
      d_covs_traject <- sapply(
      obs,
      \(x) -x$d_covs_traject_w_time[, , x$cause] %*% coefs_traject[, x$cause])

      V <- matrix(0, 2, 2 * n_causes)
      V[1, obs[[1]]$cause + n_causes] <- 1
      V[2, obs[[2]]$cause + n_causes] <- 1
      out <- sum(log(d_covs_traject)) +
      mvtnorm::dmvnorm(
      covs_traject, sigma = diag(2) + V %*% tcrossprod(vcov, V), log = TRUE)

      vcov_cond <- solve(crossprod(V) + solve(vcov))
      mean_cond <- vcov_cond %*% crossprod(V, covs_traject) |> drop()

      integral <- mixed_mult_logit_term(
      eta = etas + mean_cond[1:n_causes],
      Sigma = vcov_cond[1:n_causes, 1:n_causes],
      which_category = sapply(obs, `[[`, "cause"),
      weights = gl$w, nodes = gl$x)

      out + log(integral)
      }

      cens_interior_one_obs <- \(x, covs_traject_w_time, covs_risk){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |> matrix(NROW(coefs_risk))
      coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
      matrix(NROW(coefs_traject))
      vcov <- upper_to_full(x)

      integrand <- 1
      etas <- covs_risk %*% coefs_risk |> drop()
      vcov_sub <- vcov[1:n_causes, 1:n_causes]

      for(cause in 1:n_causes){
      idx_cause <- cause + n_causes
      z <- -solve(vcov_sub, vcov[1:n_causes, idx_cause]) |> drop()
      s <- sqrt(1 + vcov[idx_cause, idx_cause] -
      sum(vcov[idx_cause, 1:n_causes] * (-z)))

      offset <- -covs_traject_w_time[, , cause] %*% coefs_traject[, cause] |> drop()

      integrand <- integrand - mixed_mult_logit_n_probit_term(
      eta = as.matrix(etas),
      which_category = cause, s = s, eta_probit = offset,
      Sigma = vcov_sub, z = z, weights = gl$w, nodes = gl$x)
      }
      log(integrand)
      }

      f <- \(x)
      f_numerator(x) -
      cens_interior_one_obs(x, obs[[1]]$covs_traject_w_time_delay, obs[[1]]$covs_risk)

      par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
      f(par) |> dput()
      gr <- numDeriv::grad(f, par)
      dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
      (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
      */

     ghqCpp::simple_mem_stack<double> mem;

     mmcif_data
        obs1{covs_traject1, d_covs_traject1, covs_risk1, true, cause[0],
             covs_traject_delay1},
        obs2{covs_traject2, d_covs_traject2, covs_risk2, true, cause[1],
             nullptr};

     auto run_test = [&](mmcif_data const &d1, mmcif_data const &d2){
        double res{mmcif_logLik(par, indexer, d1, d2, mem, ghq_dat_use)};
        constexpr double truth{0.542989641581263};
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        double * gr{mem.get(indexer.n_par<false>())};
        constexpr double shift{-4},
                     true_gr[]{-3.62737131454235, -3.8609306783251, -4.41885286087855, -3.60387864730677, -4.02417170665619, -4.00218433340843, -3.97202357630339, -4.04913335551094, -4.08363792443094, -4.02332736751856, -3.90505675416504, -4.11594976723039, -4.32227963878388, -6.40530649068431, -4.45572566744997, -4.12629452771508, -4.04326016971414, -3.96621014479668, -4.21584953700657, -4.07393580524171, -3.95973813703203, -3.97369312816559, -4.03407848401561, -4.03150864656869, -4.0038977092062, -3.99023214327137, -3.98609084620838, -4.03407848401561, -4.01789541220853, -3.98515445573609, -3.98783348006822, -4.01758544198206, -3.99667765787121, -4.03150864656869, -3.98515445573609, -4.03081431177226, -4.01531002788366, -3.99545916003458, -4.01946533861936, -4.0038977092062, -3.98783348006822, -4.01531002788366, -4.37452255585066, -3.99999999992245, -3.99999999983855, -3.99023214327137, -4.01758544198206, -3.99545916003458, -3.99999999992245, -4.01791509850034, -4.00000000040878, -3.98609084620838, -3.99667765787121, -4.01946533861936, -3.99999999983855, -4.00000000040878, -4.01952938367508};

        std::fill(gr, gr + indexer.n_par<false>(), shift);
        res = mmcif_logLik_grad(par, gr, indexer, d1, d2, mem, ghq_dat_use);
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        for(size_t i = 0; i < indexer.n_par<false>(); ++i)
           expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
     };

     run_test(obs1, obs2);
     run_test(obs2, obs1);
  }

  test_that("mmcif_logLik works when one individual is observed and one is censored and both are left-truncated"){
     /*
      library(ghqCpp)
      f_numerator <- \(x){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(NROW(coefs_risk))
      coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
      matrix(NROW(coefs_traject))
      vcov <- upper_to_full(x)
      vcov <- (vcov + t(vcov)) / 2

      etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
      covs_traject <- sapply(
      obs,
      \(x) -x$covs_traject_w_time[, , x$cause] %*% coefs_traject[, x$cause])
      d_covs_traject <- sapply(
      obs,
      \(x) -x$d_covs_traject_w_time[, , x$cause] %*% coefs_traject[, x$cause])

      v <- numeric(2 * n_causes)
      v[n_causes + obs[[1]]$cause] <- 1
      out <- log(d_covs_traject[1]) + dnorm(
      covs_traject[1], sd = sqrt(1 + v %*% vcov %*% v), log = TRUE)

      vcov_cond <- solve(tcrossprod(v) + solve(vcov))
      mean_cond <- drop(vcov_cond %*% v) * covs_traject[1]

      integral <- mixed_mult_logit_term(
      eta = etas + mean_cond[1:n_causes],
      Sigma = vcov_cond[1:n_causes, 1:n_causes],
      which_category = c(obs[[1]]$cause, 0L),
      weights = gl$w, nodes = gl$x)

      out + log(integral)
      }

      f <- \(x)
      f_numerator(x) - cens_interior(x, lapply(obs, \(z){
      z$covs_traject_w_time <- z$covs_traject_w_time_delay
      z
      }))

      par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
      f(par) |> dput()
      gr <- numDeriv::grad(f, par)
      dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
      (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
      */

     ghqCpp::simple_mem_stack<double> mem;

     {
        mmcif_data
         obs1{covs_traject1, d_covs_traject1, covs_risk1, true, cause[0],
              covs_traject_delay1},
         obs2{covs_traject2, d_covs_traject2, covs_risk2, false, n_causes,
              covs_traject_delay2};

        auto run_test = [&](mmcif_data const &d1, mmcif_data const &d2){
           double res{mmcif_logLik(par, indexer, d1, d2, mem, ghq_dat_use)};
           constexpr double truth{-4.73928636027638};
           expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

           double * gr{mem.get(indexer.n_par<false>())};
           constexpr double shift{-4},
                        true_gr[]{-4.04044963695258, -3.91817503120514, -3.943102936965, -4.37526124750824, -4.09905308188569, -4.03245312031374, -3.88812709886303, -4.12077483945365, -4.10755041900441, -4.03010584782988, -3.87792486682717, -4.14872647639028, -5.09035461089416, -3.52955708124907, -4.13398478349854, -4.13301663367855, -3.90687006021625, -3.97564584605795, -4.18445305333345, -3.90502971591088, -3.97772624925552, -4.13288009084767, -3.96454260905568, -3.93545502645238, -4.13104529335059, -3.98416726415066, -3.98033550659295, -3.96454260905568, -4.02458773865936, -3.96105413197834, -4.01994654294083, -4.02447142987163, -3.99458111464457, -3.93545502645238, -3.96105413197834, -4.03634035819296, -4.03707625091383, -3.99236512786331, -4.02535791351056, -4.13104529335059, -4.01994654294083, -4.03707625091383, -4.20867302991084, -4.00774894058896, -4.009290117243, -3.98416726415066, -4.02447142987163, -3.99236512786331, -4.00774894058896, -4.01404256576969, -4.00140059879922, -3.98033550659295, -3.99458111464457, -4.02535791351056, -4.009290117243, -4.00140059879922, -4.01067418804994};

           std::fill(gr, gr + indexer.n_par<false>(), shift);
           res = mmcif_logLik_grad(par, gr, indexer, d1, d2, mem, ghq_dat_use);
           expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

           for(size_t i = 0; i < indexer.n_par<false>(); ++i)
              expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
        };

        run_test(obs1, obs2);
        run_test(obs2, obs1);
     }

     /*
      library(ghqCpp)
      f_numerator <- \(x){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(NROW(coefs_risk))
      coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
      matrix(NROW(coefs_traject))
      vcov <- upper_to_full(x)
      vcov <- (vcov + t(vcov)) / 2

      etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
      covs_traject <- sapply(
      obs,
      \(x) -x$covs_traject_w_time[, , x$cause] %*% coefs_traject[, x$cause])
      d_covs_traject <- sapply(
      obs,
      \(x) -x$d_covs_traject_w_time[, , x$cause] %*% coefs_traject[, x$cause])

      v <- numeric(2 * n_causes)
      v[n_causes + obs[[1]]$cause] <- 1
      out <- log(d_covs_traject[1]) + dnorm(
      covs_traject[1], sd = sqrt(1 + v %*% vcov %*% v), log = TRUE)

      vcov_cond <- solve(tcrossprod(v) + solve(vcov))
      mean_cond <- drop(vcov_cond %*% v) * covs_traject[1]

      integral <- mixed_mult_logit_term(
      eta = as.matrix(etas[, 1]) + mean_cond[1:n_causes],
      Sigma = vcov_cond[1:n_causes, 1:n_causes],
      which_category = obs[[1]]$cause, weights = gl$w, nodes = gl$x)

      integrals <- sapply(1:n_causes, \(cause){
      idx_cause <- cause + n_causes
      rng_coefs <- solve(vcov_cond[1:n_causes, 1:n_causes],
      vcov_cond[1:n_causes, idx_cause])

      s <- sqrt(1 + vcov_cond[idx_cause, idx_cause] -
      vcov_cond[idx_cause, 1:n_causes] %*% rng_coefs)
      shift <- -obs[[2]]$covs_traject_w_time[, , cause] %*% coefs_traject[, cause] -
      mean_cond[idx_cause]

      rng_coefs <- -rng_coefs

      mixed_mult_logit_n_probit_term(
      eta = etas + mean_cond[1:n_causes],
      which_category = c(obs[[1]]$cause, cause), s = s, eta_probit = shift,
      Sigma = vcov_cond[1:n_causes, 1:n_causes], z = rng_coefs,
      weights = gl$w, nodes = gl$x)
      })
      integral <- integral - sum(integrals)

      out + log(integral)
      }

      f <- \(x)
      f_numerator(x) - cens_interior(x, lapply(obs, \(z){
      z$covs_traject_w_time <- z$covs_traject_w_time_delay
      z
      }))

      par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
      f(par) |> dput()
      gr <- numDeriv::grad(f, par)
      dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
      (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
      */

     mmcif_data
        obs1{covs_traject1, d_covs_traject1, covs_risk1, true, cause[0],
             covs_traject_delay1},
       obs2{covs_traject2, d_covs_traject2, covs_risk2, true, n_causes,
            covs_traject_delay2};

     constexpr double truth{-2.84076560046042},
                      shift{-4},
                  true_gr[]{-3.76022089195144, -3.88241668080383, -4.26621026491903, -3.84141793103858, -4.01605222382096, -4.00784669002208, -3.98217537468744, -4.0107022179164, -4.02426633131268, -4.01087448030666, -3.97293784887887, -4.01956381238645, -5.28649861914239, -4.1504745017571, -4.003264712467, -4.13300947473127, -4.01379616649902, -3.96619417660548, -4.18444356193859, -4.04679443995852, -3.96585851267297, -4.03913324921375, -4.01394113409549, -4.00899536830793, -3.96761846794513, -3.98390878472694, -3.98194902432223, -4.01394113409549, -4.01258552474101, -3.99753264830174, -4.00224560007332, -4.01893615934752, -3.99693691128344, -4.00899536830793, -3.99753264830174, -4.01212860886111, -4.00818091197356, -3.99519299230928, -4.01871115311185, -3.96761846794513, -4.00224560007332, -4.00818091197356, -4.1474387344325, -4.00156442349338, -4.00372180475973, -3.98390878472694, -4.01893615934752, -3.99519299230928, -4.00156442349338, -4.01817113654699, -4.00140059909066, -3.98194902432223, -3.99693691128344, -4.01871115311185, -4.00372180475973, -4.00140059909066, -4.01696898267555};

     auto run_test = [&](const mmcif_data &d1, const mmcif_data &d2){
        double res
        {mmcif_logLik(par, indexer, d1, d2, mem, ghq_dat_use)};
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        double * gr{mem.get(indexer.n_par<false>())};
        std::fill(gr, gr + indexer.n_par<false>(), shift);
        res = mmcif_logLik_grad(par, gr, indexer, d1, d2, mem, ghq_dat_use);
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        for(size_t i = 0; i < indexer.n_par<false>(); ++i)
           expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
     };

     run_test(obs1, obs2);
     run_test(obs2, obs1);
  }

  test_that("mmcif_logLik works when both individuals are censored") {
     /*
      library(ghqCpp)
      f_numerator <- \(x){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(NROW(coefs_risk))
      coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
      matrix(NROW(coefs_traject))
      vcov <- upper_to_full(x)
      vcov <- (vcov + t(vcov)) / 2

      etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
      mixed_mult_logit_term(
      eta = etas, Sigma = vcov[1:n_causes, 1:n_causes],
      which_category = integer(2), weights = gl$w, nodes = gl$x) |>
      log()
      }

      f <- \(x)
      f_numerator(x) - cens_interior(x, lapply(obs, \(z){
      z$covs_traject_w_time <- z$covs_traject_w_time_delay
      z
      }))

      par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
      f(par) |> dput()
      gr <- numDeriv::grad(f, par)
      dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
      (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
      */

     ghqCpp::simple_mem_stack<double> mem;

     {
        mmcif_data
          obs1{covs_traject1, d_covs_traject1, covs_risk1, false, n_causes,
               covs_traject_delay1},
          obs2{covs_traject2, d_covs_traject2, covs_risk2, false, n_causes,
               covs_traject_delay2};

        double res{mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
        constexpr double truth{-2.92278188576921};
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        double * gr{mem.get(indexer.n_par<false>())};
        constexpr double shift{-4},
                     true_gr[]{-4.67324108604375, -4.25469980712742, -3.24364959238269, -4.70389847147939, -4.18712744887662, -4.06651620536472, -3.78926785238538, -4.21031309773934, -4.15528722486231, -4.04799955064068, -3.82427553858158, -4.19920625210585, -4.54423974734381, -3.64423735495344, -3.80088248967629, -4.13301663367882, -3.90687006019993, -3.97564584605872, -4.18445305328212, -3.90502971591088, -3.97772624925552, -3.68832607589589, -3.81137246646169, -3.8014577382975, -4.08670748057833, -3.98416726366494, -3.98033550662996, -3.81137246646169, -4.00789694602223, -3.92198132028852, -3.9654688358974, -4.02447142993663, -3.99458111559739, -3.8014577382975, -3.92198132028852, -4.03362305729812, -3.95489619491066, -3.99236511935278, -4.02535791365926, -4.08670748057833, -3.9654688358974, -3.95489619491066, -4.05538348026938, -4.00774894057406, -4.00929011659253, -3.98416726366494, -4.02447142993663, -3.99236511935278, -4.00774894057406, -4.01404256578281, -4.00140059816802, -3.98033550662996, -3.99458111559739, -4.02535791365926, -4.00929011659253, -4.00140059816802, -4.01067418809389};

        std::fill(gr, gr + indexer.n_par<false>(), shift);
        res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
        expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

        for(size_t i = 0; i < indexer.n_par<false>(); ++i)
           expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
     }

     /*
      library(ghqCpp)
      f_numerator <- \(x){
      coefs_risk <- get_n_remove(x, length(coefs_risk)) |>
      matrix(NROW(coefs_risk))
      coefs_traject <- get_n_remove(x, length(coefs_traject)) |>
      matrix(NROW(coefs_traject))
      vcov <- upper_to_full(x)
      vcov <- (vcov + t(vcov)) / 2

      etas <- sapply(obs, \(x) x$covs_risk %*% coefs_risk)
      integral <- mixed_mult_logit_term(
      eta = as.matrix(etas[, 2]), Sigma = vcov[1:n_causes, 1:n_causes],
      which_category = 0L, weights = gl$w, nodes = gl$x)

      integrals <- sapply(1:n_causes, \(cause){
      lp_traject <- -obs[[1]]$covs_traject_w_time[, , cause] %*%
      coefs_traject[, cause] |>
      drop()
      idx_cause <- cause + n_causes
      rng_coefs <-
      solve(vcov[1:n_causes, 1:n_causes], vcov[1:n_causes, idx_cause])
      s <- sqrt(1 + vcov[idx_cause, idx_cause] -
      vcov[idx_cause, 1:n_causes] %*% rng_coefs)
      rng_coefs <- -rng_coefs
      mixed_mult_logit_n_probit_term(
      eta = etas, which_category = c(cause, 0L), s = s,
      eta_probit = lp_traject, Sigma = vcov[1:n_causes, 1:n_causes],
      z = rng_coefs, weights = gl$w, nodes = gl$x)
      })
      log(integral - sum(integrals))
      }

      f <- \(x)
      f_numerator(x) - cens_interior(x, lapply(obs, \(z){
      z$covs_traject_w_time <- z$covs_traject_w_time_delay
      z
      }))

      par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
      f(par) |> dput()
      gr <- numDeriv::grad(f, par)
      dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
      (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) - 4) |> dput()
      */

     {
        mmcif_data
         obs1{covs_traject1, d_covs_traject1, covs_risk1, true, n_causes,
              covs_traject_delay1},
         obs2{covs_traject2, d_covs_traject2, covs_risk2, false, n_causes,
              covs_traject_delay2};

        constexpr double truth{-2.21783930363746},
                         shift{-4},
                     true_gr[]{-4.36618964551978, -4.09077096341356, -3.58297261658434, -4.54661252201643, -4.13584193065743, -4.04564463126114, -3.84671111176923, -4.16172917675212, -4.12131170611729, -4.03501020529789, -3.8624291950585, -4.16414904707945, -3.2813204407283, -3.32286755320137, -4.11467146287165, -4.02701402330414, -3.87989601880855, -3.98982624196728, -4.15716306054059, -3.89808534565099, -3.98027141580723, -3.90528692710204, -3.89089383195705, -3.87143963283647, -4.13753587138598, -3.99258417463058, -3.9820983152209, -3.89089383195705, -4.01387234039915, -3.94585223918251, -4.0207558076828, -4.01829470598917, -3.99494398885712, -3.87143963283647, -3.94585223918251, -4.03392709928431, -4.01448520457304, -3.99600413818357, -4.02459165581253, -4.13753587138598, -4.0207558076828, -4.01448520457304, -3.94561260535514, -4.00774894056953, -4.00929011670638, -3.99258417463058, -4.01829470598917, -3.99600413818357, -4.00774894056953, -4.00095916962221, -4.00140059827682, -3.9820983152209, -3.99494398885712, -4.02459165581253, -4.00929011670638, -4.00140059827682, -4.00781218828552};
        auto run_test = [&](mmcif_data const &d1, mmcif_data const &d2){
           double res
           {mmcif_logLik(par, indexer, d1, d2, mem, ghq_dat_use)};
           expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

           double * gr{mem.get(indexer.n_par<false>())};
           std::fill(gr, gr + indexer.n_par<false>(), shift);
           res = mmcif_logLik_grad(par, gr, indexer, d1, d2, mem, ghq_dat_use);
           expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

           for(size_t i = 0; i < indexer.n_par<false>(); ++i)
              expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
        };

        run_test(obs1, obs2);
        run_test(obs2, obs1);
     }

     /*
      library(ghqCpp)
      f <- \(x)
      cens_interior(x, obs) - cens_interior(x, lapply(obs, \(z){
      z$covs_traject_w_time <- z$covs_traject_w_time_delay
      z
      }))

      par <- c(coefs_risk, coefs_traject, vcov[upper.tri(vcov, TRUE)])
      f(par) |> dput()
      gr <- numDeriv::grad(f, par)
      dim_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
      (c(head(gr, -dim_vcov), d_upper_to_full(tail(gr, dim_vcov))) + 1.5) |> dput()
      */

     mmcif_data
      obs1{covs_traject1, d_covs_traject1, covs_risk1, true, n_causes,
           covs_traject_delay1},
      obs2{covs_traject2, d_covs_traject2, covs_risk2, true, n_causes,
           covs_traject_delay2};

     double res{mmcif_logLik(par, indexer, obs1, obs2, mem, ghq_dat_use)};
     constexpr double truth{-0.462664346486468};
     expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

     double * gr{mem.get(indexer.n_par<false>())};
     constexpr double shift{1.5},
                  true_gr[]{1.45289424838312, 1.47542068460448, 1.55212380331434, 1.47391779253777, 1.46499741944719, 1.48300532578904, 1.53888100216066, 1.47626778143172, 1.47042706876714, 1.48638662979327, 1.53293760896, 1.47739492990714, 2.41336292423534, 1.76220979127637, 1.403644907404, 1.45163764433979, 1.46998550493305, 1.52582012590617, 1.35316691697156, 1.42890861292188, 1.53346915367078, 1.50886488587095, 1.51446439732359, 1.51631198869456, 1.5121521572626, 1.50562604333777, 1.51089145310745, 1.51446439732359, 1.48958107999042, 1.50682194834269, 1.48927734691668, 1.48793885512813, 1.50159160357042, 1.51631198869456, 1.50682194834269, 1.48638158347761, 1.48928601553624, 1.50190826918396, 1.48435885553185, 1.5121521572626, 1.48927734691668, 1.48928601553624, 1.62217391527812, 1.49875511279688, 1.49625760402066, 1.50562604333777, 1.48793885512813, 1.50190826918396, 1.49875511279688, 1.49146007389686, 1.49889936244429, 1.51089145310745, 1.50159160357042, 1.48435885553185, 1.49625760402066, 1.49889936244429, 1.48602620483938};

     std::fill(gr, gr + indexer.n_par<false>(), shift);
     res = mmcif_logLik_grad(par, gr, indexer, obs1, obs2, mem, ghq_dat_use);
     expect_true(std::abs(res - truth) < std::abs(truth) * 1e-8);

     for(size_t i = 0; i < indexer.n_par<false>(); ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-3);
  }
}
