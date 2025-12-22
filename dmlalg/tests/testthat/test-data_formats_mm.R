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
ww_name <- sapply(seq_len(s), function(a) paste("w", a, sep = ""))
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
  id <- unlist(sapply(seq_len(N), function(x) rep(x, nT[x])))
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
  cask_factor <- do.call(c, sapply(seq_len(N), function(x) c(rep(1, floor(nT[x] / 2)), rep(2, ceiling(nT[x] / 2)))))
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
  b <- do.call(c, sapply(seq_len(N), function(x) rep(ranef[x], nT[x])))
  cask <- do.call(c, sapply(seq_len(N), function(x) c(rep(cask1[x], floor(nT[x] / 2)), rep(cask2[x], ceiling(nT[x] / 2)))))

  ww <- do.call(cbind, lapply(seq_len(10), function(x) rnorm(n, 0, 1)))
  xx <- cbind(gW_X(ww) + rnorm(n, 0, 0.5), rnorm(n, 0, 1))
  yy <- xx %*% beta0 + gW_Y(ww) + cask + b  + rnorm(n, 0, sigma0)

  return_values(xx = xx, yy = yy, ww = ww, zz = zz, nT = nT, beta = beta0[beta0 != 0])
}

res_ranef <- datafun_ranef(N, n)
nT <- res_ranef$nT
Ntot <- sum(nT)

data1 <- datafun(N = N, nT = res_ranef$nT, zz = res_ranef$zz, beta0 = rbind(beta0, 0))
data2 <- datafun(N = N, nT = res_ranef$nT, zz = res_ranef$zz, beta0 = cbind(rep(beta0, 2)))


####################
# perform tests
####################

test_that("check_data_mm outputs right formats", {
  do_tests <- function(res) {
    expect_true(is.data.frame(res$zz))
    expect_true(is.data.frame(res$ww))
    expect_true(is.matrix(res$xx))
    expect_true(is.matrix(res$yy))
  }

  res <- check_data_mm(zz = as.matrix(data1["id", "cask"]),
                       ww = data1[c("w1", "w2", "w3")],
                       xx = as.data.frame(data1$x1),
                       yy = data1$resp)
  do_tests(res)

  res <- check_data_mm(zz = data1["id"],
                       ww = as.matrix(data1[c("w1", "w2", "w3")]),
                       xx = as.matrix(data1$x1),
                       yy = data1$resp)
  do_tests(res)
})

test_that("DML_reg_error_message_mm outputs a string", {
  res <- DML_reg_error_message_mm()
  expect_type(res, "character")
  expect_length(res, 1)
})

test_that("beta_crossfit_NA_return_mm returns object of correct dimension", {
  do_tests <- function(d) {
    res <- beta_crossfit_NA_return_mm(d = d)
    expect_length(res, 14)
    expect_equal(dim(res$beta), c(d, 1))
    expect_equal(names(res),
                 c("random_eff", "beta", "theta", "sigma", "vcov", "residuals",
                   "ngrps", "nobs", "fitMsgs", "cnms", "nc", "nms", "useSc",
                   "optinfo"))
  }

 do_tests(d = 1)
 do_tests(d = 3)
})

test_that("batch_correction_mm returns object of correct dimension", {
  do_parallel <- FALSE
  parallel <- "no"

  do_tests <- function(data, xx_name) {
    zz <- data[, zz_name, drop = FALSE]
    xx <- data[, xx_name, drop = FALSE]
    yy <- data[, yy_name, drop = FALSE]
    ww <- data[, ww_name, drop = FALSE]
    mat_data <- check_data_mm(zz = zz, ww = ww, xx = xx, yy = yy)
    zz <- mat_data$zz
    xx <- mat_data$xx
    yy <- mat_data$yy
    ww <- mat_data$ww

    res <- suppressMessages(batch_correction_mm(zz = zz, ww = ww, xx = xx, yy = yy,
                                                zz_formula = zz_formula, group = group, K = K,
                                                cond_method = cond_method, params = params,
                                                do_parallel = do_parallel, parallel = parallel,
                                                S = S, ncpus = ncpus, cl = cl))
    expect_length(res, S)
    resone <- res[[1]]$value

    target_object <- beta_crossfit_NA_return_mm(d = ncol(xx))
    expect_equal(names(resone), names(target_object))
    expect_length(resone, length(target_object))
    expect_true(is.vector(resone$beta))
    expect_length(resone$beta, ncol(xx))
  }

  do_tests(data = data1, xx_name = xx_name1)
  do_tests(data = data2, xx_name = xx_name2)
})

test_that("get_batch_correction_mm returns object of correct dimension", {
  do_parallel <- FALSE
  parallel <- "no"

  do_tests <- function(data, xx_name, nr_random_eff, nr_res) {
    zz <- data[, zz_name, drop = FALSE]
    xx <- data[, xx_name, drop = FALSE]
    yy <- data[, yy_name, drop = FALSE]
    ww <- data[, ww_name, drop = FALSE]
    mat_data <- check_data_mm(zz = zz, ww = ww, xx = xx, yy = yy)
    zz <- mat_data$zz
    xx <- mat_data$xx
    yy <- mat_data$yy
    ww <- mat_data$ww
    d <- length(xx_name)

    beta_all <- suppressMessages(batch_correction_mm(zz = zz, ww = ww, xx = xx, yy = yy,
                                                     zz_formula = zz_formula, group = group,
                                                     K = K,
                                                     cond_method = cond_method,
                                                     params = params,
                                                     do_parallel = do_parallel,
                                                     parallel = parallel,
                                                     S = S, ncpus = ncpus, cl = cl))
    beta_all <- do.call(rbind, beta_all)
    beta_all_unlist <- do.call(rbind, beta_all[, "value"])
    res <- get_batch_correction_mmDML(beta_all_unlist = beta_all_unlist,
                                      xx_colnames = xx_name,
                                      nr_random_eff = nr_random_eff,
                                      nr_res = nr_res)
    expect_equal(class(res), c("mmdml", "list"))
    expect_length(res, 17)

    expect_type(res$methTitle, "character")
    expect_length(res$methTitle, 1)
    expect_length(res$residuals, nr_res)
    expect_length(res$residuals[[1]], length(yy))
    expect_length(res$random_eff_all, nr_random_eff)

    expect_equal(names(res$beta), xx_name)
    expect_length(res$sigma, 1)
    expect_equal(dim(res$vcov), c(d, d))
    expect_equal(colnames(res$vcov), xx_name)
    expect_equal(rownames(res$vcov), xx_name)

    expect_equal(res$nobs, length(yy))
  }

  do_tests(data = data1, xx_name = xx_name1, nr_random_eff = 3, nr_res = 2)
  do_tests(data = data1, xx_name = xx_name1, nr_random_eff = 1, nr_res = 2)
})

test_that("get_condexp_funcs_mm returns object of correct dimension", {
  res <- get_condexp_funcs_mm(cond_method = rep("spline", 2),
                              params = list(list(alpha = 1), list(alpha = 1)))
  expect_true(res$params_identical)
  expect_equal(res$all_condexp_spline, c(TRUE, TRUE))

  res <- get_condexp_funcs_mm(cond_method = rep("spline", 2),
                              params = NULL)
  expect_true(res$params_identical)
  expect_equal(res$all_condexp_spline, c(TRUE, TRUE))

  res <- get_condexp_funcs_mm(cond_method = rep("spline", 2),
                              params = list(list(alpha = 1), list(alpha = 2)))
  expect_false(res$params_identical)
  expect_equal(res$all_condexp_spline, c(TRUE, TRUE))

  res <- get_condexp_funcs_mm(cond_method = c("spline", "forest"),
                              params = list(list(alpha = 1), list(alpha = 1)))
  expect_false(res$params_identical)
  expect_equal(res$all_condexp_spline, c(TRUE, FALSE))

  res <- get_condexp_funcs_mm(cond_method = c("forest", "forest"),
                              params = list(list(alpha = 1), list(alpha = 1)))
  expect_false(res$params_identical)
  expect_equal(res$all_condexp_spline, c(FALSE, FALSE))
})

test_that("residuals_samplesplit_mm returns object of correct dimension", {
  params <- NULL
  cond_func_all <- get_condexp_funcs_mm(cond_method = rep("spline", 2),
                                        params = params)

  do_tests <- function(data, xx_name) {
    zz <- data[, zz_name, drop = FALSE]
    xx <- data[, xx_name, drop = FALSE]
    yy <- data[, yy_name, drop = FALSE]
    ww <- data[, ww_name, drop = FALSE]
    mat_data <- check_data_mm(zz = zz, ww = ww, xx = xx, yy = yy)
    zz <- mat_data$zz
    xx <- mat_data$xx
    yy <- mat_data$yy
    ww <- mat_data$ww
    d <- length(xx_name)

    I <- 1:floor(N / 2)
    Ic <- setdiff(1:N, I)
    I_full <- c(1:Ntot)[zz[[group]] %in% I]
    Ic_full <- c(1:Ntot)[zz[[group]] %in% Ic]

    res <- suppressWarnings(residuals_samplesplit_mm(ww = ww, xx = xx, yy = yy,
                                                     I = I_full, Ic = Ic_full,
                                                     cond_func_all = cond_func_all,
                                                     params = params))
    rY <- res$rY
    rX <- res$rX
    expect_true(is.matrix(rY))
    expect_true(is.matrix(rX))
    expect_equal(colnames(rY), "resp")
    expect_equal(colnames(rX), xx_name)
    expect_equal(dim(rY), c(length(I_full), 1))
    expect_equal(dim(rX), c(length(I_full), d))
  }

  do_tests(data = data1, xx_name = xx_name1)
  do_tests(data = data2, xx_name = xx_name2)
})

test_that("get_beta_mmdml returns object of correct dimension and variance matrix", {
  params <- NULL
  cond_func_all <- get_condexp_funcs_mm(cond_method = rep("ols", 2),
                                        params = params)

  do_tests <- function(data, xx_name) {
    zz <- data[, zz_name, drop = FALSE]
    xx <- data[, xx_name, drop = FALSE]
    yy <- data[, yy_name, drop = FALSE]
    ww <- data[, ww_name, drop = FALSE]
    mat_data <- check_data_mm(zz = zz, ww = ww, xx = xx, yy = yy)
    zz <- mat_data$zz
    xx <- mat_data$xx
    yy <- mat_data$yy
    ww <- mat_data$ww
    d <- length(xx_name)

    expect_warning(suppressMessages(
      get_beta_mmdml(zz = zz, ww = ww, xx = xx, yy = yy,
                     zz_formula = zz_formula, group = group, K = 1,
                     cond_func_all = cond_func_all, params = params)))
    res <- suppressWarnings(suppressMessages(
      get_beta_mmdml(zz = zz, ww = ww, xx = xx, yy = yy,
                     zz_formula = zz_formula, group = group, K = 1,
                     cond_func_all = cond_func_all, params = params)))
    target_object <- beta_crossfit_NA_return_mm(d = ncol(xx))
    expect_equal(names(res), names(target_object))
    expect_length(res, length(target_object))
    expect_true(is.vector(res$beta))
    expect_length(res$beta, ncol(xx))
    expect_length(res$random_eff$id, N)
  }

  do_tests(data = data1, xx_name = xx_name1)
  do_tests(data = data2, xx_name = xx_name2)
})

test_that("example_data_mmdml returns object of correct dimension", {
  do_tests <- function(beta0) {
    res <- example_data_mmdml(beta0 = beta0)
    expect_true(is.data.frame(res))
    expect_equal(ncol(res), 6 + length(beta0))
    xx_coln <- sapply(seq_len(length(beta0)), function(a) paste("x", a, sep = ""))
    coln_expect <- c(xx_coln, "resp", "id", "cask", "w1", "w2", "w3")
    expect_true(setequal(colnames(res), coln_expect))
  }

  do_tests(beta0 = 1)
  do_tests(beta0 = c(1, 1))

  expect_error(example_data_mmdml(beta0 = rep(1, 3)))
  expect_error(example_data_mmdml(beta0 = 1, n = 4))
})
