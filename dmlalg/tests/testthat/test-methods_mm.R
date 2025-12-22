####################
# set parameters
####################

set.seed(19)
N <- 10
n <- 5
beta0 <- 0.2
params <- NULL
xx_name1 <- c("x1")
xx_name2 <- c("x1", "x2")
s <- 10
ww_name <- sapply(1:s, function(a) paste("w", a, sep = ""))
zz_name <- c("id", "cask")
zz_formula <- "(1|id) + (1|cask:id)"
group <- "id"
yy_name <- "resp"

K <- 2L
params <- NULL
level <- 0.95
S <- 3L
parallel <- "no"
ncpus <- 1L
cl <- NULL
level <- 0.95
nr_random_eff <- 2
nr_res <- 2
cond_method <- rep("ols", 2)


####################
# generate data
####################

datafun_ranef <- function(N, n) {
  # balanced groups of about the same size
  nT <- n + sample(-3:3, N, replace = TRUE)
  id <- unlist(sapply(1:N, function(x) rep(x, nT[x])))
  list(zz = data.frame(id = id), nT = nT)
}

gW_Y <- function(ww) {
  1 / 4 * (1 / 2 * (ww[, 1] >= 0) * (ww[, 3] >= 0) * (ww[, 4] >= 0) +
             1 / 4 * 1 / 3 * (ww[, 4] >= 0) * (ww[, 6] >= 0) * ((ww[, 7] >= 0) + 2 * (ww[, 8] >= 0) * (ww[, 9] >= 0) * (ww[, 10] >= 0)) +
             1 / 4 * 1 / 4 * (ww[, 7] >= 0) * (ww[, 6] >= 0.5) * (ww[, 1] >= 0.5) * ((ww[, 4] <= 0.5) - (ww[, 10] <= -1) - 2 * (ww[, 8] >= 0.5) +
                                                                                       0.5 * (ww[, 1] >= -0.5) * (ww[, 2] >= -0.5) * (ww[, 6] >= -1)) +
             1 / 2 * 1 / 3 * (ww[, 1] >= 0.25) * (ww[, 4] <= -0.25) * (ww[, 8] <= 0) * ((ww[, 7] >= 0.5) * (ww[, 10] <= -0.25) - 2 * (ww[, 4] <= -1) + 3 * (ww[, 3] <= -0.25)) +
             1 / 3 * (ww[, 2] >= 0.25) * (ww[, 3] >= 0.25) * (ww[, 9] <= -0.25) +
             1 / 3 * (ww[, 7] >= 0.25) * (ww[, 9] >= 0.25) * (2 * (ww[, 5] <= 0.5) - (ww[, 8] <= -0.25) * (ww[, 9] >= 0.5) + (ww[, 4] <= -0.25) * (ww[, 5] >= 0.25))
  )
}

gW_X <- function(ww) {
  1 / 4 * (ww[, 1] <= 0) * (ww[, 2] >= 0) * ((ww[, 5] >= 0) + (ww[, 7] <= 0) * (ww[, 8] >= 0)) +
    1 / 2 * (ww[, 8] <= 0) * (ww[, 2] >= 0) * (ww[, 9] <= 0.5) * (ww[, 3] >= 0)
}

Sigma0 <- function() {
  list(sigma0 = 0.1,
       theta_factor0 = 0.05)
}

return_values <- function(xx, yy, ww, zz, nT, beta) {
  cask_factor <- unlist(sapply(1:N, function(x) c(rep(1, floor(nT[x] / 2)), rep(2, ceiling(nT[x] / 2)))))
  ww_data_frame <- as.data.frame(ww)
  colnames(ww_data_frame) <- sapply(seq_len(ncol(ww_data_frame)), function(a) paste("w", a, sep = ""))
  data_interm <- data.frame(x1 = xx[, 1],
                            resp = yy, data.frame(id = as.factor(zz$id), cask = as.factor(cask_factor)),
                            ww_data_frame)
  if (length(beta) == 1) {
    data_interm
  } else if (length(beta) == 2) {
    cbind(data_interm, data.frame(x2 = xx[, 2]))
  }
}

datafun <- function(N, nT, zz, beta0, return_theta_sigma = FALSE) {
  sigma_theta <- Sigma0()
  sigma0 <- sigma_theta$sigma0
  theta0 <- sigma_theta$theta_factor0 * c(1, 1) / sigma0
  if (return_theta_sigma) {
    return(list(theta0 = theta0, sigma0 = sigma0))
  }

  ranef <- rnorm(N, 0, theta0[1] * sigma0)
  cask1 <- rnorm(N, 0, theta0[2] * sigma0)
  cask2 <- rnorm(N, 0, theta0[2] * sigma0)
  n <- sum(nT)
  b <- unlist(sapply(1:N, function(x) rep(ranef[x], nT[x])))
  cask <- unlist(sapply(1:N, function(x) c(rep(cask1[x], floor(nT[x] / 2)), rep(cask2[x], ceiling(nT[x] / 2)))))

  ww <- do.call(cbind, lapply(1:10, function(x) rnorm(n, 0, 1)))
  xx <- cbind(gW_X(ww) + rnorm(n, 0, 0.5), rnorm(n, 0, 1))
  yy <- xx %*% beta0 + gW_Y(ww) + cask + b  + rnorm(n, 0, sigma0)

  return_values(xx = xx, yy = yy, ww = ww, zz = zz, nT = nT, beta = beta0[beta0 != 0])
}

res_ranef <- datafun_ranef(N, n)
nT <- res_ranef$nT
Ntot <- sum(nT)

data1 <- datafun(N = N, nT = res_ranef$nT, zz = res_ranef$zz, beta0 = rbind(beta0, 0))
data2 <- datafun(N = N, nT = res_ranef$nT, zz = res_ranef$zz, beta0 = cbind(rep(beta0, 2)))

fit1 <- suppressWarnings(suppressMessages(
  mmdml(w = ww_name, x = xx_name1, y = yy_name, z = zz_name, data = data1,
        z_formula = zz_formula, group = group,
        K = K, cond_method = cond_method, params = params, S = S,
        parallel = parallel, ncpus = ncpus, cl = cl,
        nr_random_eff = nr_random_eff, nr_res = nr_res)))

fit2 <- suppressWarnings(suppressMessages(
  mmdml(w = ww_name, x = xx_name2, y = yy_name, z = zz_name, data = data2,
        z_formula = zz_formula, group = group,
        K = K, cond_method = cond_method, params = params, S = S,
        parallel = parallel, ncpus = ncpus, cl = cl,
        nr_random_eff = nr_random_eff, nr_res = nr_res)))


####################
# perform tests
####################

test_that("VarCorr.mmdml outputs right formats", {
  do_tests <- function(res) {
    expect_equal(class(res), "VarCorr.merMod")
  }

  do_tests(VarCorr(fit1))
  do_tests(VarCorr(fit2))
})

test_that("vcov.mmdml outputs right formats", {
  do_tests <- function(res, xx_name) {
    d <- length(xx_name)
    expect_equal(dim(res), c(d, d))
    expect_equal(class(res)[1], "dpoMatrix")
    expect_equal(res@Dimnames, list(xx_name, xx_name))
  }

  do_tests(res = vcov(fit1), xx_name = xx_name1)
  do_tests(res = vcov(fit2), xx_name = xx_name2)
})

test_that("fixef.mmdml outputs right formats", {
  do_tests <- function(res, xx_name) {
    d <- length(xx_name)
    expect_equal(class(res), "numeric")
    expect_length(res, d)
    expect_equal(names(res), xx_name)
  }

  do_tests(res = fixef(fit1), xx_name = xx_name1)
  do_tests(res = fixef(fit2), xx_name = xx_name2)
})

test_that("print.mmdml outputs right formats", {
  do_tests <- function(fit) {
    res <- capture.output(print(fit))
    expect_length(res, 11)
  }

  do_tests(fit1)
  do_tests(fit2)
})

test_that("summary.mmdml outputs right formats", {
  do_tests <- function(fit) {
    expect_error(summary(fit, nr_res = 1000))
    res <- summary(fit)
    expect_equal(class(res), "summary.mmdml")
    expect_length(res, 13)
    expect_equal(names(res),
                 c("methTitle", "objClass", "ngrps", "nobs", "coefficients",
                   "sigma", "vcov", "varcor", "residuals", "fitMsgs", "optinfo",
                   "nr_res", "correlation"))
    expect_equal(res$methTitle, fit$methTitle)
    expect_equal(res$objClass, "mmdml")
    expect_equal(res$nobs, Ntot)
    expect_equal(res$sigma, sigma(fit))
    expect_equal(res$vcov, vcov(fit))
    expect_equal(res$varcor, VarCorr(fit))
    expect_equal(res$residuals, fit$residuals)
    expect_equal(res$fitMsgs, fit$fitMsgs)
    expect_equal(res$optinfo, fit$optinfo)
  }

  do_tests(fit1)
  do_tests(fit2)
})

test_that("print.summary.mmdml outputs right formats", {
  do_tests <- function(fit, len) {
    res <- capture.output(print.summary.mmdml(summary.mmdml(fit)))
    expect_length(res, len)
  }

  do_tests(fit1, 20)
  do_tests(fit2, 25)
})

test_that("residuals.mmdml outputs right formats", {
  do_tests <- function(fit) {
    res <- residuals(fit, scaled = TRUE)
    expect_length(res, nr_res)

    res <- residuals(fit, scaled = FALSE)
    expect_length(res, nr_res)
  }

  do_tests(fit1)
  do_tests(fit2)
})

test_that("sigma.mmdml outputs right formats", {
  do_tests <- function(fit) {
    res <- sigma(fit)
    expect_equal(class(res), "numeric")
    expect_length(res, 1)
  }

  do_tests(fit1)
  do_tests(fit2)
})

test_that("confint.mmdml outputs right formats", {
  do_tests <- function(fit, level, xx_name) {
    d <- length(xx_name)
    res <- confint(fit, level = level)
    alpha <- 1 - level
    nms <- c(paste((alpha / 2) * 100, "%", sep = ""),
             paste((1 - alpha / 2) * 100, "%", sep = ""))

    expect_equal(class(res), c("matrix", "array"))
    expect_equal(dim(res), c(d, 2))
    expect_equal(colnames(res), nms)
  }

  do_tests(fit1, level = 0.95, xx_name = xx_name1)
  do_tests(fit1, level = 0.7, xx_name = xx_name1)
  do_tests(fit2, level = 0.95, xx_name = xx_name2)
  do_tests(fit2, level = 0.7, xx_name = xx_name2)
})
