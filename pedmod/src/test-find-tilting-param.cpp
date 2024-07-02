#include <testthat.h>
#include "find-tilting-param.h"
#include <limits.h>

namespace {
constexpr double Inf{std::numeric_limits<double>::infinity()};
}

context("find_tilting_param unit tests") {
  /*
   .check_input <- \(n, a, b, Sig)
   stopifnot(length(a) == n, length(b) == n, all(dim(Sig) == c(n, n)),
   all(a < b))

   psi <- \(x, a, b, Sig){
   n <- length(x) %/% 2L
   mu <- head(x, n)
   x <- tail(x, n)

   .check_input(n, a, b, Sig)

   ubs <- lbs <- numeric(n)
   C <- chol(Sig)
   lbs[1] <- a[1] / C[1, 1]
   ubs[1] <- b[1] / C[1, 1]

   for(i in seq_len(n - 1L) + 1L){
   lbs[i] <- (a[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   ubs[i] <- (b[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   }
   lbs[is.infinite(a)] <- -Inf
   ubs[is.infinite(b)] <-  Inf

   -sum(mu * x) + sum(mu^2) / 2 + sum(log(pnorm(ubs - mu) - pnorm(lbs - mu)))
   }

   psi_safe <- \(x, a, b, Sig){
   n <- length(x) %/% 2L
   mu <- head(x, n)
   x <- tail(x, n)

   .check_input(n, a, b, Sig)

   ubs <- lbs <- numeric(n)
   C <- chol(Sig)
   lbs[1] <- a[1] / C[1, 1]
   ubs[1] <- b[1] / C[1, 1]

   for(i in seq_len(n - 1L) + 1L){
   lbs[i] <- (a[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   ubs[i] <- (b[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   }

   lbs <- lbs - mu
   ubs <- ubs - mu

   pnrm_terms <- numeric(n)
   for(i in 1:n){
   pnrm_terms[i] <-
   if(lbs[i] > 0){
   pnrm_log_lb <- pnorm(lbs[i], lower.tail = FALSE, log.p = TRUE)
   pnrm_log_ub <- pnorm(ubs[i], lower.tail = FALSE, log.p = TRUE)

   pnrm_log_lb + log1p(-exp(pnrm_log_ub - pnrm_log_lb))
   } else if(ubs[i] < 0){
   pnrm_log_lb <- pnorm(lbs[i], lower.tail = TRUE, log.p = TRUE)
   pnrm_log_ub <- pnorm(ubs[i], lower.tail = TRUE, log.p = TRUE)

   pnrm_log_ub + log1p(-exp(pnrm_log_lb - pnrm_log_ub))
   } else {
   pnrm_lb <- pnorm(lbs[i], lower.tail = TRUE, log.p = FALSE)
   pnrm_ub <- pnorm(ubs[i], lower.tail = FALSE, log.p = FALSE)

   log1p(-pnrm_lb - pnrm_ub)
   }
   }

   -sum(mu * x) + sum(mu^2) / 2 + sum(pnrm_terms)
   }

   d_psi <- \(x, a, b, Sig){
   n <- length(x) %/% 2L
   mu <- head(x, n)
   x <- tail(x, n)

   .check_input(n, a, b, Sig)

   ubs <- lbs <- numeric(n)
   C <- chol(Sig)
   lbs[1] <- a[1] / C[1, 1]
   ubs[1] <- b[1] / C[1, 1]

   for(i in seq_len(n - 1L) + 1L){
   lbs[i] <- (a[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   ubs[i] <- (b[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   }

   lbs <- lbs - mu
   ubs <- ubs - mu

   denoms_log <- numeric(n)
   for(i in 1:n){
   denoms_log[i] <-
   if(lbs[i] > 0){
   pnrm_log_lb <- pnorm(lbs[i], lower.tail = FALSE, log.p = TRUE)
   pnrm_log_ub <- pnorm(ubs[i], lower.tail = FALSE, log.p = TRUE)

   pnrm_log_lb + log1p(-exp(pnrm_log_ub - pnrm_log_lb))
   } else if(ubs[i] < 0){
   pnrm_log_lb <- pnorm(lbs[i], lower.tail = TRUE, log.p = TRUE)
   pnrm_log_ub <- pnorm(ubs[i], lower.tail = TRUE, log.p = TRUE)

   pnrm_log_ub + log1p(-exp(pnrm_log_lb - pnrm_log_ub))
   } else {
   pnrm_lb <- pnorm(lbs[i], lower.tail = TRUE, log.p = FALSE)
   pnrm_ub <- pnorm(ubs[i], lower.tail = FALSE, log.p = FALSE)

   log1p(-pnrm_lb - pnrm_ub)
   }
   }

   dnrms_log_lbs <- dnorm(lbs, log = TRUE)
   dnrms_log_ubs <- dnorm(ubs, log = TRUE)

   ratio_lbs <- exp(dnrms_log_lbs - denoms_log)
   ratio_ubs <- exp(dnrms_log_ubs - denoms_log)

   derivs <- ratio_lbs - ratio_ubs
   C <- C %*% diag(diag(C)^-1)

   c(mu - x + derivs, -mu + (C - diag(n)) %*% derivs)
   }

   dd_psi <- \(x, a, b, Sig){
   n <- length(x) %/% 2L
   mu <- head(x, n)
   x <- tail(x, n)

   .check_input(n, a, b, Sig)

   ubs <- lbs <- numeric(n)
   C <- chol(Sig)
   lbs[1] <- a[1] / C[1, 1]
   ubs[1] <- b[1] / C[1, 1]

   for(i in seq_len(n - 1L) + 1L){
   lbs[i] <- (a[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   ubs[i] <- (b[i] - sum(C[seq_len(i - 1L), i] * x[seq_len(i - 1L)])) / C[i, i]
   }

   lbs <- lbs - mu
   ubs <- ubs - mu

   denoms_log <- numeric(n)
   for(i in 1:n){
   denoms_log[i] <-
   if(lbs[i] > 0){
   pnrm_log_lb <- pnorm(lbs[i], lower.tail = FALSE, log.p = TRUE)
   pnrm_log_ub <- pnorm(ubs[i], lower.tail = FALSE, log.p = TRUE)

   pnrm_log_lb + log1p(-exp(pnrm_log_ub - pnrm_log_lb))
   } else if(ubs[i] < 0){
   pnrm_log_lb <- pnorm(lbs[i], lower.tail = TRUE, log.p = TRUE)
   pnrm_log_ub <- pnorm(ubs[i], lower.tail = TRUE, log.p = TRUE)

   pnrm_log_ub + log1p(-exp(pnrm_log_lb - pnrm_log_ub))
   } else {
   pnrm_lb <- pnorm(lbs[i], lower.tail = TRUE, log.p = FALSE)
   pnrm_ub <- pnorm(ubs[i], lower.tail = FALSE, log.p = FALSE)

   log1p(-pnrm_lb - pnrm_ub)
   }
   }

   dnrms_log_lbs <- dnorm(lbs, log = TRUE)
   dnrms_log_ubs <- dnorm(ubs, log = TRUE)

   ratio_lbs <- exp(dnrms_log_lbs - denoms_log)
   ratio_ubs <- exp(dnrms_log_ubs - denoms_log)

   derivs_gr <- ratio_lbs - ratio_ubs

   derivs <-
   ifelse(is.infinite(lbs), 0, lbs * ratio_lbs) -
   ifelse(is.infinite(ubs), 0, ubs * ratio_ubs) -
   derivs_gr^2
   C <- C %*% diag(diag(C)^-1)
   diff_mat <- C - diag(n)

   out <- matrix(0., 2L * n, 2L * n)
   m1 <- diff_mat %*% diag(derivs)
   out[  1:n ,   1:n ] <- diag(n) + diag(derivs)
   out[-(1:n),   1:n ] <- m1 - diag(n)
   out[-(1:n), -(1:n)] <- tcrossprod(m1, diff_mat)
   out[upper.tri(out)] <- t(out)[upper.tri(out)]
   out
   }
   */

  test_that("find_tilting_param gives the same as an R implementation in a 5D example") {
    /*
     set.seed(111)
     n <- 5
     Sig <- matrix(1, n, n)
     while (cov2cor(Sig)[lower.tri(Sig)] |> abs() |> max() > .999)
     Sig <- rWishart(1, n, diag(n)) |> drop()
     a <- runif(n, -2, 0)
     b <- runif(n, 0, 2)
     type <- sample.int(3, n, replace = TRUE)
     a[type == 1] <- -Inf
     b[type == 2] <- Inf

     start <- local({
     C <- chol(Sig)
     start_org_scale <- (cbind(a, b) |> rowMeans()) / diag(C)
     start_org_scale[type == 1] <- b[type == 1] / diag(C)[type == 1] - 1
     start_org_scale[type == 2] <- a[type == 2] / diag(C)[type == 2] + 1
     start_org_scale <- start_org_scale * diag(C)
     solve(t(C), start_org_scale)
     })

     par <- local({
     mu <- numeric(n)
     C <- chol(Sig)
     for(i in seq_len(n - 1L) + 1L)
     mu[i] <- -C[seq_len(i - 1L), i] %*% start[seq_len(i - 1L)] / C[i, i]
     c(mu, start)
     })

# do we start of in an interior point?
     ptr <- crossprod(chol(Sig), tail(par, n))
     all(ptr > a)
     all(ptr < b)

     psi(par, a, b, Sig)
     all.equal(psi(par, a, b, Sig), psi_safe(par, a, b, Sig))
     psi <- psi_safe

     stopifnot(all.equal(d_psi(par, a, b, Sig),
     numDeriv::grad(psi, par, a = a, b = b, Sig = Sig)))

     stopifnot(all.equal(dd_psi(par, a, b, Sig),
     numDeriv::jacobian(d_psi, par, a = a, b = b, Sig = Sig)))

# finds a root
     root_finder <- \(x, a, b, Sig, abstol = 1e-2){
     f <- \(x) d_psi(x, a, b, Sig)^2 |> sum()
     d_f <- \(x){
     f_vals <- d_psi(x, a, b, Sig)
     grs <- dd_psi(x, a, b, Sig)
     2 * rowSums(grs %*% diag(f_vals))
     }

# sanity check as this is still experimental
     num_gr <- try(numDeriv::grad(f, x), silent = TRUE)
     if(!inherits(num_gr, "try-error")){
     is_equal <- all.equal(d_f(x), num_gr, tolerance = 1e-5)
     if(!isTRUE(is_equal))
     warning(paste0(capture.output(is_equal), collapse = "\n"))
     }

# find the root
     optim(x, fn = f, gr = d_f, method = "BFGS",
 control = list(reltol = 1e-8, abstol = abstol))
 }

 res <- root_finder(par, a, b, Sig, 0)
 rbind(Estimate = res$par, Start = par)
 d_psi(res$par, a, b, Sig) |> abs() |> sum() # ~ zero
 res$counts

# do we have an interior solution
 ptr <- crossprod(chol(Sig), tail(res$par, n))
 all(ptr > a)
 all(ptr < b)

 head(res$par, n) |> dput()
 C <- chol(Sig)
 dput(a / diag(C))
 dput(b / diag(C))
 dput((C %*% diag(diag(C)^-1))[upper.tri(C, TRUE)])
     */
       constexpr size_t dim{5};
       constexpr double lower_limits[]{-0.815837000441616, -0.288358708962961, -Inf, -Inf, -Inf},
                        upper_limits[]{Inf, Inf, 1.26198812572033, 0.42491765901508, 0.670519533487418},
                            cholesky[]{1, -0.137599036720516, 1, -0.895959210162758, 0.155032252997349, 1, -0.0641444623930221, -0.150172397352036, 0.681663684155795, 1, 0.729181873459802, -1.43240521262587, -0.0784937458067778, -0.328361932329834, 1},
                                tilt[]{-0.070383996090136, 0.273675444869523, -0.252692930348561, 0.0529469150197525, 0};

       auto res =  find_tilting_param
         (dim, lower_limits, upper_limits, cholesky, 1e-8);

       expect_true(res.success);
       expect_true(res.tilting_param.size() == dim);
       constexpr double eps{1e-5};
       for(size_t i = 0; i < dim; ++i)
         expect_true
           (std::abs(res.tilting_param[i] - tilt[i]) <
             (eps + std::abs(tilt[i])) * eps);

   }

   test_that("find_tilting_param gives the same as an R implementation in a particular 4D example") {
      /*
       n <- 4
       mu <- c(-0.626, 0.184, -0.836, 1.595)
       Sig <- structure(c(8.287, -0.848, -0.879, -1.788, -0.848, 3.581, 2.916, -3.957, -0.879, 2.916, 7.361, -0.648, -1.788, -3.957, -0.648, 11.735), .Dim = c(4L, 4L))
       l <- c(-Inf, -1, 1, -Inf)
       u <- c(1, Inf, 3, 2)

       a <- l - mu
       b <- u - mu
       type <- rep(3L, n)
       type[is.infinite(a)] <- 1L
       type[is.infinite(b)] <- 2L

       start <- local({
       C <- chol(Sig)
       start_org_scale <- (cbind(a, b) |> rowMeans()) / diag(C)
       start_org_scale[type == 1] <- b[type == 1] / diag(C)[type == 1] - 1
       start_org_scale[type == 2] <- a[type == 2] / diag(C)[type == 2] + 1
       start_org_scale <- start_org_scale * diag(C)
       solve(t(C), start_org_scale)
       })

       par <- local({
       mu <- numeric(n)
       C <- chol(Sig)
       for(i in seq_len(n - 1L) + 1L)
       mu[i] <- -C[seq_len(i - 1L), i] %*% start[seq_len(i - 1L)] / C[i, i]
       c(mu, start)
       })

# do we start of in an interior point?
       ptr <- crossprod(chol(Sig), tail(par, n))
       all(ptr > a)
       all(ptr < b)

       psi(par, a, b, Sig)
       all.equal(psi(par, a, b, Sig), psi_safe(par, a, b, Sig))
       psi <- psi_safe

       stopifnot(all.equal(d_psi(par, a, b, Sig),
       numDeriv::grad(psi, par, a = a, b = b, Sig = Sig)))

       stopifnot(all.equal(dd_psi(par, a, b, Sig),
       numDeriv::jacobian(d_psi, par, a = a, b = b, Sig = Sig)))

# finds a root
       root_finder <- \(x, a, b, Sig, abstol = 1e-2){
       f <- \(x) d_psi(x, a, b, Sig)^2 |> sum()
       d_f <- \(x){
       f_vals <- d_psi(x, a, b, Sig)
       grs <- dd_psi(x, a, b, Sig)
       2 * rowSums(grs %*% diag(f_vals))
       }

# sanity check as this is still experimental
       num_gr <- try(numDeriv::grad(f, x), silent = TRUE)
       if(!inherits(num_gr, "try-error")){
       is_equal <- all.equal(d_f(x), num_gr, tolerance = 1e-5)
       if(!isTRUE(is_equal))
       warning(paste0(capture.output(is_equal), collapse = "\n"))
       }

# find the root
       optim(x, fn = f, gr = d_f, method = "BFGS",
       control = list(reltol = 1e-8, abstol = abstol))
       }

       res <- root_finder(par, a, b, Sig, 0)
       rbind(Estimate = res$par, Start = par)
       d_psi(res$par, a, b, Sig) |> abs() |> sum() # ~ zero
       res$counts

# do we have an interior solution
       ptr <- crossprod(chol(Sig), tail(res$par, n))
       all(ptr > a)
       all(ptr < b)

       head(res$par, n) |> dput()
       C <- chol(Sig)
       dput(a / diag(C))
       dput(b / diag(C))
       dput((C %*% diag(diag(C)^-1))[upper.tri(C, TRUE)])
       */

      constexpr size_t dim{4};
      constexpr double lower_limits[]{-Inf, -0.633397384777738, 0.822556629383548, -Inf},
                       upper_limits[]{0.564835353630162, Inf, 1.71858781607587, 0.177964365756849},
                           cholesky[]{1, -0.157587492922657, 1, -0.136799130087002, 0.677326353411537, 1, -0.272927233463801, -0.97319344026522, 0.494264058473526, 1},
                               tilt[]{-0.00790443677427996, 0.8472618175305, -0.192800238203537, 0};

      auto res =  find_tilting_param
         (dim, lower_limits, upper_limits, cholesky, 1e-8);

      expect_true(res.success);
      expect_true(res.tilting_param.size() == dim);
      constexpr double eps{1e-5};
      for(size_t i = 0; i < dim; ++i)
         expect_true
         (std::abs(res.tilting_param[i] - tilt[i]) <
            (eps + std::abs(tilt[i])) * eps);

   }

   test_that("find_tilting_param gives the same as an R implementation in a 25D Fernandez et al. example") {
      /*
       n <- 25
       Sig <- solve(diag(.5, n) + .5)
       a <- rep(.5, n)
       b <- rep(1, n)
       type <- rep(3, n)

       start <- local({
       C <- chol(Sig)
       start_org_scale <- (cbind(a, b) |> rowMeans()) / diag(C)
       start_org_scale[type == 1] <- b[type == 1] / diag(C)[type == 1] - 1
       start_org_scale[type == 2] <- a[type == 2] / diag(C)[type == 2] + 1
       start_org_scale <- start_org_scale * diag(C)
       solve(t(C), start_org_scale)
       })

       par <- local({
       mu <- numeric(n)
       C <- chol(Sig)
       for(i in seq_len(n - 1L) + 1L)
       mu[i] <- -C[seq_len(i - 1L), i] %*% start[seq_len(i - 1L)] / C[i, i]
       c(mu, start)
       })

# do we start of in an interior point?
       ptr <- crossprod(chol(Sig), tail(par, n))
       all(ptr > a)
       all(ptr < b)

       psi(par, a, b, Sig)
       all.equal(psi(par, a, b, Sig), psi_safe(par, a, b, Sig))
       psi <- psi_safe

       stopifnot(all.equal(d_psi(par, a, b, Sig),
       numDeriv::grad(psi, par, a = a, b = b, Sig = Sig)))

       stopifnot(all.equal(dd_psi(par, a, b, Sig),
       numDeriv::jacobian(d_psi, par, a = a, b = b, Sig = Sig)))

# finds a root
       root_finder <- \(x, a, b, Sig, abstol = 1e-2){
       f <- \(x) d_psi(x, a, b, Sig)^2 |> sum()
       d_f <- \(x){
       f_vals <- d_psi(x, a, b, Sig)
       grs <- dd_psi(x, a, b, Sig)
       2 * rowSums(grs %*% diag(f_vals))
       }

# sanity check as this is still experimental
       num_gr <- try(numDeriv::grad(f, x), silent = TRUE)
       if(!inherits(num_gr, "try-error")){
       is_equal <- all.equal(d_f(x), num_gr, tolerance = 1e-5)
       if(!isTRUE(is_equal))
       warning(paste0(capture.output(is_equal), collapse = "\n"))
       }

# find the root
       optim(x, fn = f, gr = d_f, method = "BFGS",
       control = list(reltol = 1e-8, abstol = abstol))
       }

       res <- root_finder(par, a, b, Sig, 0)
       rbind(Estimate = res$par, Start = par)
       d_psi(res$par, a, b, Sig) |> abs() |> sum() # ~ zero
       res$counts

# do we have an interior solution
       ptr <- crossprod(chol(Sig), tail(res$par, n))
       all(ptr > a)
       all(ptr < b)

       head(res$par, n) |> dput()
       C <- chol(Sig)
       dput(a / diag(C))
       dput(b / diag(C))
       dput((C %*% diag(diag(C)^-1))[upper.tri(C, TRUE)])
       */
      constexpr size_t dim{25};
      constexpr double lower_limits[]{0.360555127546399, 0.360843918243516, 0.361157559257308, 0.361499402740611, 0.361873432227873, 0.362284418654736, 0.362738125055006, 0.36324157862839, 0.3638034375545, 0.364434493427831, 0.365148371670111, 0.3659625273557, 0.366899692852672, 0.367990036096994, 0.369274472937998, 0.370809924354783, 0.372677996249965, 0.375, 0.377964473009227, 0.381881307912987, 0.387298334620742, 0.395284707521047, 0.408248290463863, 0.433012701892219, 0.5},
                       upper_limits[]{0.721110255092798, 0.721687836487032, 0.722315118514615, 0.722998805481221, 0.723746864455746, 0.724568837309472, 0.725476250110012, 0.726483157256779, 0.727606875108999, 0.728868986855663, 0.730296743340221, 0.7319250547114, 0.733799385705343, 0.735980072193987, 0.738548945875997, 0.741619848709566, 0.74535599249993, 0.75, 0.755928946018454, 0.763762615825973, 0.774596669241484, 0.790569415042095, 0.816496580927726, 0.866025403784439, 1},
                           cholesky[]{1, -0.0400320384512713, 1, -0.0400668337976504, -0.0417028828114147, 1, -0.0401047579271038, -0.041742355496836, -0.0435194139889244, 1, -0.0401462527730996, -0.0417855447018672, -0.0435644418478277, -0.0455015755193289, 1, -0.040191847623425, -0.0418330013267037, -0.0436139187994323, -0.0455532525098839, -0.0476731294622796, 1, -0.0402421818292766, -0.0418853908291695, -0.0436685386314792, -0.0456103010638724, -0.0477328328412939, -0.0500626174321758, 1, -0.0402980349884659, -0.041943524640393, -0.0437291473194001, -0.0456736047738154, -0.0477990824676822, -0.0501321006265198, -0.0527046276694729, 1, -0.0403603676397787, -0.0420084025208403, -0.0437967871855338, -0.0457442523098896, -0.0478730176742346, -0.0502096445253434, -0.052786150730834, -0.0556414884074657, 1, -0.0404303770031319, -0.0420812705765086, -0.0438727573851896, -0.045823600595561, -0.0479560585305292, -0.0502967385101848, -0.0528777139405529, -0.0557380045049948, -0.0589255650988789, 1, -0.0405095746833467, -0.0421637021355785, -0.0439586982263859, -0.0459133628766789, -0.0480499980104298, -0.050395263067897, -0.0529812942826018, -0.0558471877722261, -0.0590409923693398, -0.062622429108515, 1, -0.0405998971470575, -0.0422577127364258, -0.0440567110531401, -0.0460157339354887, -0.0481571330330887, -0.0505076272276105, -0.0530994244053591, -0.0559717078549556, -0.0591726335413939, -0.0627620556566792, -0.0668153104781061, 1, -0.0407038663240706, -0.0423659272868161, -0.0441695325209754, -0.0461335721154623, -0.0482804549585267, -0.0506369683541833, -0.0532354026674541, -0.0561150415284528, -0.0593241642211074, -0.0629227782134541, -0.0669864127057083, -0.0716114874039432, 1, -0.0408248290463863, -0.0424918292799399, -0.0443007944225996, -0.0462706707003683, -0.0484239335956647, -0.050787450018337, -0.053393606293099, -0.0562818027921642, -0.059500462274618, -0.0631097705371751, -0.0671854812358212, -0.0718243006142779, -0.0771516749810459, 1, -0.0409673245199351, -0.0426401432711221, -0.0444554224474387, -0.0464321744050227, -0.0485929530749862, -0.0509647191437626, -0.053579971977682, -0.0564782494724905, -0.0597081434026532, -0.0633300496381123, -0.0674199862463242, -0.0720749970156447, -0.0774209661138763, -0.0836242010007091, 1, -0.0411376675603721, -0.0428174419288838, -0.0446402690761202, -0.0466252404120157, -0.0487950036474267, -0.051176631571916, -0.0538027586848971, -0.0567130872815601, -0.0599564111820418, -0.0635933773836461, -0.0677003200386331, -0.0723746864455746, -0.0777428842014242, -0.0839719122759632, -0.0912870929175278, 1, -0.0413449115297362, -0.0430331482911935, -0.0448651585048471, -0.0468601297596278, -0.0490408238613749, -0.051434449987364, -0.0540738070435875, -0.0569987973380641, -0.0602584605051822, -0.0639137490706142, -0.0680413817439772, -0.0727392967453308, -0.0781345384897492, -0.0843949472569722, -0.0917469804271967, -0.100503781525921, 1, -0.0416025147168922, -0.043301270189222, -0.0451446949071634, -0.0471520960096449, -0.0493463771219826, -0.0517549169506765, -0.0544107187582509, -0.0573539334676404, -0.0606339062590832, -0.0643119694284408, -0.0684653196881457, -0.0731925054711399, -0.0786213627541438, -0.0849207775608447, -0.0923186182344995, -0.101129979369486, -0.111803398874989, 1, -0.0419313934688768, -0.0436435780471986, -0.045501575519329, -0.0475248456521758, -0.0497364731301994, -0.0521640530957301, -0.0548408497107082, -0.0578073313016081, -0.0611132331352141, -0.0648203723552165, -0.0690065559342355, -0.0737711113563318, -0.0792428851750327, -0.0855920985021826, -0.0930484210398471, -0.101929438287525, -0.112687233963802, -0.125988157669742, 1, -0.0423659272868162, -0.0440958551844099, -0.0459731070306142, -0.0480173442533384, -0.0502518907629606, -0.052704627669473, -0.0554091638503641, -0.0584063870042052, -0.0617465478029123, -0.065492103999448, -0.0697216688778396, -0.0745355992499929, -0.0800640769025435, -0.0864790869437951, -0.0940126791362944, -0.102985730108887, -0.113855008510662, -0.127293769304329, -0.144337567297406, 1, -0.042966892442366, -0.0447213595499958, -0.0466252404120156, -0.0486984753557674, -0.0509647191437625, -0.0534522483824848, -0.0561951486949017, -0.0592348877759093, -0.0626224291085149, -0.0664211164155071, -0.0707106781186548, -0.0755928946018454, -0.0811997942941149, -0.0877058019307029, -0.0953462589245593, -0.104446593573419, -0.115470053837925, -0.129099444873581, -0.14638501094228, -0.169030850945703, 1, -0.0438529009653515, -0.0456435464587639, -0.04758668672668, -0.0497026732804715, -0.0520156486610299, -0.0545544725589981, -0.0573539334676405, -0.0604563541758343, -0.0639137490706142, -0.06779076806833, -0.0721687836487033, -0.0771516749810459, -0.0828741930164744, -0.0895143592549291, -0.0973123680201904, -0.106600358177805, -0.117851130197758, -0.131761569173682, -0.149403576166799, -0.172516389835589, -0.204124145231931, 1, -0.045291081365784, -0.0471404520791033, -0.049147318718299, -0.0513327002339346, -0.0537215309350254, -0.0563436169819011, -0.0592348877759092, -0.0624390541054463, -0.066009836198445, -0.0700140042014005, -0.074535599249993, -0.0796819072889596, -0.0855920985021826, -0.0924500327042049, -0.100503781525921, -0.110096376512636, -0.121716123890037, -0.136082763487954, -0.154303349962092, -0.17817416127495, -0.210818510677892, -0.258198889747161, 1, -0.0480384461415262, -0.0500000000000001, -0.0521286035142686, -0.0544465506480471, -0.056980288229819, -0.0597614304667196, -0.0628280862437543, -0.0662266178532522, -0.0700140042014005, -0.0742610657232505, -0.0790569415042095, -0.0845154254728516, -0.0907841299003203, -0.098058067569092, -0.106600358177805, -0.116774841624228, -0.129099444873581, -0.144337567297406, -0.163663417676994, -0.188982236504614, -0.223606797749979, -0.273861278752583, -0.353553390593274, 1, -0.055470019622523, -0.0577350269189627, -0.0601929265428845, -0.0628694613461932, -0.0657951694959769, -0.0690065559342354, -0.0725476250110011, -0.0764719112901873, -0.0808452083454443, -0.0857492925712544, -0.0912870929175277, -0.0975900072948532, -0.104828483672192, -0.11322770341446, -0.123091490979333, -0.134839972492648, -0.149071198499986, -0.166666666666667, -0.188982236504614, -0.218217890235992, -0.258198889747161, -0.316227766016838, -0.408248290463863, -0.577350269189626, 1},
                               tilt[]{-10.6483138756461, -10.6213187665947, -10.5920329239539, -10.5601517771226, -10.5253142434974, -10.4870889799495, -10.4449564138964, -10.3982849604358, -10.3462991048695, -10.2880359067919, -10.2222847075013, -10.147501948114, -10.0616882171421, -9.96220641262307, -9.84550522923516, -9.70668492697523, -9.53878924436385, -9.33159790706846, -9.06945304084566, -8.72707589444235, -8.26080059966575, -7.58801467925103, -6.53070310690829, -4.61787887814806, 0};

      auto res =  find_tilting_param
         (dim, lower_limits, upper_limits, cholesky, 1e-8);

      expect_true(res.success);
      expect_true(res.tilting_param.size() == dim);
      constexpr double eps{1e-5};
      for(size_t i = 0; i < dim; ++i)
         expect_true
         (std::abs(res.tilting_param[i] - tilt[i]) <
            (eps + std::abs(tilt[i])) * eps);
   }
}
