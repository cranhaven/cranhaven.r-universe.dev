set.seed(19)
n <- 40
s <- 6
params <- NULL


####################
# generate data
####################

ww_frame <- as.data.frame(matrix(rnorm(n * s, 0, 1), ncol = s))
ww_frameone <- as.data.frame(matrix(rnorm(n * 1, 0, 1), ncol = 1))
ww_mat <- matrix(rnorm(n * s, 0, 1), ncol = s)
ww_matone <- matrix(rnorm(n * 1, 0, 1), ncol = 1)
ww_vec <- rnorm(n, 0, 1)

yy_vec <- rnorm(n, 0, 1)
yy_frame <- as.data.frame(rnorm(n, 0, 1), ncol = 1)
yy_mat <- matrix(rnorm(n, 0, 1), ncol = 1)

aa_frame <- as.data.frame(cbind(rnorm(n, 0, 1), rnorm(n, 0, 1), rnorm(n, 0, 1)))
aa_frameone <- as.data.frame(matrix(rnorm(n, 0, 1), ncol = 1))
aa_mat <- cbind(rnorm(n, 0, 1), rnorm(n, 0, 1), rnorm(n, 0, 1))

aa_matone <- matrix(rnorm(n, 0, 1), ncol = 1)
aa_vec <- rnorm(n, 0, 1)

xx_frame <- as.data.frame(cbind(rnorm(n, 0, 1), rnorm(n, 0, 1)))
xx_frameone <- as.data.frame(matrix(rnorm(n, 0, 1), ncol = 1))
xx_mat <- cbind(rnorm(n, 0, 1), rnorm(n, 0, 1))
xx_matone <- matrix(rnorm(n, 0, 1), ncol = 1)
xx_vec <- rnorm(n, 0, 1)


####################
# perform tests
####################

test_that("set_print_vars behaves the right way", {
  ###
  set.seed(22)
  res <- suppressWarnings(regsdml(a = aa_frameone, x = xx_matone, y = yy_mat,
                                  w = ww_matone,
                                  cond_method = c("spline", "spline", "spline"),
                                  gamma = exp(seq(-4, 10, length.out = 6)),
                                  DML = "DML2", S = 4, do_regsDML = TRUE,
                                  do_safety = TRUE, do_DML = TRUE,
                                  do_regDML = TRUE, do_regDML_all_gamma = TRUE))
  print_regsDML <- NULL
  print_safety <- NULL
  print_DML <- NULL
  print_regDML <- NULL
  print_regDML_all_gamma <- NULL
  parms <- set_print_vars(object = res, print_regsDML = print_regsDML,
                          print_safety = print_safety, print_DML = print_DML,
                          print_regDML = print_regDML,
                          print_regDML_all_gamma = print_regDML_all_gamma)
  expect_true(all.equal(parms, c(TRUE, FALSE, FALSE, FALSE, FALSE), check.names = FALSE))

  print_DML <- print_regDML <- TRUE
  parms <- set_print_vars(object = res, print_regsDML = print_regsDML,
                          print_safety = print_safety, print_DML = print_DML,
                          print_regDML = print_regDML,
                          print_regDML_all_gamma = print_regDML_all_gamma)
  expect_true(all.equal(parms, c(TRUE, FALSE, TRUE, TRUE, FALSE), check.names = FALSE))

  ###
  set.seed(22)
  res <- suppressWarnings(regsdml(a = aa_frameone, x = xx_matone, y = yy_mat,
                                  w = ww_matone,
                                  cond_method = c("spline", "spline", "spline"),
                                  gamma = exp(seq(-4, 10, length.out = 6)),
                                  DML = "DML2", S = 4, do_regsDML = FALSE,
                                  do_safety = TRUE, do_DML = TRUE,
                                  do_regDML = TRUE, do_regDML_all_gamma = TRUE,
                                  safety_factor = 10))
  print_regsDML <- NULL
  print_safety <- NULL
  print_DML <- NULL
  print_regDML <- NULL
  print_regDML_all_gamma <- NULL
  parms <- set_print_vars(object = res, print_regsDML = print_regsDML,
                          print_safety = print_safety, print_DML = print_DML,
                          print_regDML = print_regDML,
                          print_regDML_all_gamma = print_regDML_all_gamma)
  expect_true(all.equal(parms, c(FALSE, FALSE, TRUE, TRUE, FALSE), check.names = FALSE))
  v <- summary(res)
  expect_true(all.equal(names(v), c("DML", "regDML")))
  w <- confint(res)
  expect_true(all.equal(names(w), c("DML", "regDML")))
  z <- coef(res)
  expect_true(all.equal(colnames(z), c("DML", "regDML")))
  varcov <- vcov(res)
  expect_true(all.equal(names(varcov), c("DML", "regDML")))

  expect_error(coef(res, print_safety = TRUE))

  ###
  set.seed(22)
  res <- suppressWarnings(regsdml(a = aa_frameone, x = xx_matone, y = yy_mat,
                                  w = ww_matone,
                                  cond_method = c("spline", "spline", "spline"),
                                  gamma = exp(seq(-4, 10, length.out = 6)),
                                  DML = "DML2", S = 4, do_regsDML = FALSE,
                                  do_safety = TRUE, do_DML = TRUE,
                                  do_regDML = TRUE, do_regDML_all_gamma = TRUE,
                                  safety_factor = 0.001))
  print_regsDML <- NULL
  print_safety <- NULL
  print_DML <- NULL
  print_regDML <- NULL
  print_regDML_all_gamma <- NULL
  parms <- set_print_vars(object = res, print_regsDML = print_regsDML,
                          print_safety = print_safety, print_DML = print_DML,
                          print_regDML = print_regDML,
                          print_regDML_all_gamma = print_regDML_all_gamma)
  expect_true(all.equal(parms, c(FALSE, TRUE, TRUE, TRUE, FALSE), check.names = FALSE))

  print_regsDML <- FALSE
  expect_error(set_print_vars(object = res, print_regsDML = print_regsDML,
                              print_safety = print_safety, print_DML = print_DML,
                              print_regDML = print_regDML,
                              print_regDML_all_gamma = print_regDML_all_gamma))
})

test_that("data input argument in regsdml works", {
  do_comparison <- function(res1, res2) {
    res1.DML <- res1$DML_statistics
    res2.DML <- res2$DML_statistics
    res1.gamma <- res1$regsDML_statistics
    res2.gamma <- res2$regsDML_statistics

    for (method in c("DML", "gamma")) {
      res1.analyze <- get(paste("res1.", method, sep = ""))
      res2.analyze <- get(paste("res2.", method, sep = ""))
      expect_equal(sum(res1.analyze$beta_DML - res2.analyze$beta_DML), 0)
      expect_equal(sum(res1.analyze$sd_DML - res2.analyze$sd_DML), 0)
      expect_equal(sum(res1.analyze$var_DML - res2.analyze$var_DML), 0)
      expect_equal(sum(res1.analyze$pval_DML - res2.analyze$pval_DML), 0)
      expect_equal(sum(res1.analyze$CI_DML - res2.analyze$CI_DML), 0)
    }
  }

  aa <- aa_frame
  xx <- xx_mat
  yy <- yy_mat
  ww <- ww_frame
  seed <- 5

  set.seed(seed)
  res1 <- regsdml(a = aa, x = xx, y = yy, w = ww,
                  cond_method = rep("ols", 3),
                  gamma = exp(seq(-4, 10, length.out = 20)),
                  S = 10)

  data <- data.frame(x1name = xx[, 1], x2name = xx[, 2],
                     a1 = aa[, 1], a2 = aa[, 2], a3 = aa[, 3],
                     yy = yy,
                     w1 = ww[, 1], w2 = ww[, 2], w3 = ww[, 3], w4 = ww[, 4],
                     w5 = ww[, 5], w6 = ww[, 6])
  set.seed(seed)
  res2 <- regsdml(a = c("a1", "a2", "a3"),
                  x = c("x1name", "x2name"),
                  y = "yy",
                  w = c("w1", "w2", "w3", "w4", "w5", "w6"),
                  cond_method = rep("ols", 3),
                  gamma = exp(seq(-4, 10, length.out = 20)),
                  S = 10,
                  data = data)
  expect_true(all.equal(rownames(res2$DML_statistics$beta_DML), c("x1name", "x2name")))
  do_comparison(res1, res2)
})

test_that("function check_data outputs right data formats", {
  check_is_matrix <- function(res) {
    expect_true(is.matrix(res))
  }
  check_is_data_frame <- function(res) {
    expect_true(is.data.frame(res))
  }
  check_data_format <- function(aa, ww, xx, yy) {
    res <- check_data(aa, ww, xx, yy)
    check_is_matrix(res$aa)
    check_is_matrix(res$xx)
    check_is_matrix(res$yy)
    check_is_data_frame(res$ww)
  }

  # check aa
  check_data_format(aa_frame, ww_frame, xx_frame, yy_frame)
  check_data_format(aa_frameone, ww_frame, xx_frame, yy_frame)
  check_data_format(aa_mat, ww_frame, xx_frame, yy_frame)
  check_data_format(aa_matone, ww_frame, xx_frame, yy_frame)
  check_data_format(aa_vec, ww_frame, xx_frame, yy_frame)

  # check ww
  check_data_format(aa_frame, ww_frameone, xx_frame, yy_frame)
  check_data_format(aa_frame, ww_mat, xx_frame, yy_frame)
  check_data_format(aa_frame, ww_matone, xx_frame, yy_frame)
  check_data_format(aa_frame, ww_vec, xx_frame, yy_frame)

  # check xx
  check_colnames_xx <- function(aa, ww, xx, yy) {
    xx_test <- check_data(aa, ww, xx, yy)$xx
    expect_true(is.matrix(xx_test))
    expect_true(all.equal(colnames(xx_test), colnames(xx)))
  }
  check_colnames_xx(aa_frame, ww_frame, xx_frame, yy_frame)
  check_colnames_xx(aa_frame, ww_frame, xx_frameone, yy_frame)
  check_colnames_xx(aa_frame, ww_frame, xx_mat, yy_frame)
  xx_new <- xx_mat
  colnames(xx_new) <- c("one", "two")
  check_colnames_xx(aa_frame, ww_frame, xx_new, yy_frame)
  check_colnames_xx(aa_frame, ww_frame, xx_matone, yy_frame)
  xx_new <- xx_matone
  colnames(xx_new) <- c("one")
  check_colnames_xx(aa_frame, ww_frame, xx_new, yy_frame)
  check_colnames_xx(aa_frame, ww_frame, xx_vec, yy_frame)

  # check yy
  check_data_format(aa_frame, ww_frame, xx_frame, yy_mat)
  check_data_format(aa_frame, ww_frame, xx_frame, yy_vec)
  expect_true(is.matrix(check_data(aa_frame, ww_frame, xx_frame, yy_frame)$yy))
})

test_that("spline conditional expectation estimator outputs right data formats", {
  # check aa
  expect_true(is.matrix(condexp_spline(aa_fit = aa_mat, yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)$eAgW_hat))
  expect_true(is.matrix(condexp_spline(aa_fit = aa_matone, yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)$eAgW_hat))
  expect_true(is.matrix(condexp_spline(aa_fit = aa_mat, yy_fit = yy_mat,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)$eAgW_hat))
  expect_true(is.matrix(condexp_spline(aa_fit = aa_matone, yy_fit = yy_mat,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)$eAgW_hat))

  # check yy
  expect_true(is.matrix(condexp_spline(yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_spline(yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))

  # check xx
  expect_true(is.matrix(condexp_spline(xx_fit = xx_mat, yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)$eXgW_hat))
  expect_true(is.matrix(condexp_spline(xx_fit = xx_matone, yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)$eXgW_hat))
  expect_true(is.matrix(condexp_spline(xx_fit = xx_mat, yy_fit = yy_mat,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)$eXgW_hat))
  expect_true(is.matrix(condexp_spline(xx_fit = xx_matone, yy_fit = yy_mat,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)$eXgW_hat))
})

test_that("random forest conditional expectation estimator outputs right data formats", {
  # check aa
  expect_true(is.matrix(condexp_forest(yy_fit = aa_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_forest(yy_fit = aa_matone,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_forest(yy_fit = aa_mat,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)))
  expect_true(is.matrix(condexp_forest(yy_fit = aa_matone,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)))

  # check yy
  expect_true(is.matrix(condexp_forest(yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_forest(yy_fit = yy_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))

  # check xx
  expect_true(is.matrix(condexp_forest(yy_fit = xx_mat,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_forest(yy_fit = xx_matone,
                                       ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_forest(yy_fit = xx_mat,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)))
  expect_true(is.matrix(condexp_forest(yy_fit = xx_matone,
                                       ww_fit = ww_frameone, ww_predict = ww_frameone)))
})

test_that("ols conditional expectation estimator outputs right data formats", {
  # check aa
  expect_true(is.matrix(condexp_ols(yy_fit = aa_mat,
                                    ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_ols(yy_fit = aa_matone,
                                    ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_ols(yy_fit = aa_mat,
                                    ww_fit = ww_frameone, ww_predict = ww_frameone)))
  expect_true(is.matrix(condexp_ols(yy_fit = aa_matone,
                                    ww_fit = ww_frameone, ww_predict = ww_frameone)))

  # check yy
  expect_true(is.matrix(condexp_ols(yy_fit = yy_mat,
                                    ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_ols(yy_fit = yy_mat,
                                    ww_fit = ww_frame, ww_predict = ww_frame)))

  # check xx
  expect_true(is.matrix(condexp_ols(yy_fit = xx_mat,
                                    ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_ols(yy_fit = xx_matone,
                                    ww_fit = ww_frame, ww_predict = ww_frame)))
  expect_true(is.matrix(condexp_ols(yy_fit = xx_mat,
                                    ww_fit = ww_frameone, ww_predict = ww_frameone)))
  expect_true(is.matrix(condexp_ols(yy_fit = xx_matone,
                                    ww_fit = ww_frameone, ww_predict = ww_frameone)))
})

test_that("elasticnet conditional expectation estimator outputs right data formats", {
  # check aa
  expect_true(is.matrix(condexp_elasticnet(yy_fit = aa_mat,
                                           ww_fit = ww_frame, ww_predict = ww_frame,
                                           params = list(nfolds = 4))))
  expect_true(is.matrix(condexp_elasticnet(yy_fit = aa_matone,
                                           ww_fit = ww_frame, ww_predict = ww_frame,
                                           params = list(nfolds = 4))))
  expect_error(condexp_elasticnet(yy_fit = aa_mat,
                                  ww_fit = ww_frameone, ww_predict = ww_frameone,
                                  params = list(nfolds = 4)))

  # check yy
  expect_true(is.matrix(condexp_elasticnet(yy_fit = yy_mat,
                                           ww_fit = ww_frame, ww_predict = ww_frame,
                                           params = list(nfolds = 4))))
  expect_true(is.matrix(condexp_elasticnet(yy_fit = yy_mat,
                                           ww_fit = ww_frame, ww_predict = ww_frame,
                                           params = list(nfolds = 4))))

  # check xx
  expect_true(is.matrix(condexp_elasticnet(yy_fit = xx_mat,
                                           ww_fit = ww_frame, ww_predict = ww_frame,
                                           params = list(nfolds = 4))))
  expect_true(is.matrix(condexp_elasticnet(yy_fit = xx_matone,
                                           ww_fit = ww_frame, ww_predict = ww_frame,
                                           params = list(nfolds = 4))))
})

test_that("lasso conditional expectation estimator outputs right data formats", {
  # check aa
  expect_true(is.matrix(condexp_lasso(yy_fit = aa_mat,
                                      ww_fit = ww_frame, ww_predict = ww_frame,
                                      params = list(nfolds = 4))))
  expect_true(is.matrix(condexp_lasso(yy_fit = aa_matone,
                                      ww_fit = ww_frame, ww_predict = ww_frame,
                                      params = list(nfolds = 4))))
  expect_error(condexp_lasso(yy_fit = aa_mat,
                             ww_fit = ww_frameone, ww_predict = ww_frameone,
                             params = list(nfolds = 4)))

  # check yy
  expect_true(is.matrix(condexp_lasso(yy_fit = yy_mat,
                                      ww_fit = ww_frame, ww_predict = ww_frame,
                                      params = list(nfolds = 4))))
  expect_true(is.matrix(condexp_lasso(yy_fit = yy_mat,
                                      ww_fit = ww_frame, ww_predict = ww_frame,
                                      params = list(nfolds = 4))))

  # check xx
  expect_true(is.matrix(condexp_lasso(yy_fit = xx_mat,
                                      ww_fit = ww_frame, ww_predict = ww_frame,
                                      params = list(nfolds = 4))))
  expect_true(is.matrix(condexp_lasso(yy_fit = xx_matone,
                                      ww_fit = ww_frame, ww_predict = ww_frame,
                                      params = list(nfolds = 4))))
})

test_that("residuals_samplesplit and beta_samplesplit_gamma output
          right data formats", {
            check_residual_form <- function(res) {
              expect_true(is.matrix(res$rX))
              expect_true(is.matrix(res$rX_gW))
              expect_true(is.matrix(res$rY))
              expect_true(is.matrix(res$rY_gW))
              expect_true(is.matrix(res$rA))
            }

            I <- sample(1:n, n / 2, replace = FALSE)
            Ic <- setdiff(1:n, I)
            params <- NULL
            res <- suppressWarnings(residuals_samplesplit(a = aa_matone, w = ww_frame,
                                                          x = xx_matone, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(c("ols", "forest", "spline"),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_matone, w = ww_frame,
                                                          x = xx_matone, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = list("ols", "forest", condexp_ols),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_matone, w = ww_frame,
                                                          x = xx_matone, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = list("spline", "spline", condexp_ols),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_matone, w = ww_frame,
                                                          x = xx_matone, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = list(condexp_ols, "spline", condexp_ols),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_matone, w = ww_frame,
                                                          x = xx_matone, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = list(condexp_ols, condexp_ols, condexp_ols),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_mat, w = ww_frame,
                                                          x = xx_mat, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = c("ols", "forest", "spline"),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_mat, w = ww_frame,
                                                          x = xx_mat, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = c("lasso", "ridge", "elasticnet"),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_mat, w = ww_frame,
                                                          x = xx_mat, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)

            res <- suppressWarnings(residuals_samplesplit(a = aa_matone, w = ww_frame,
                                                          x = xx_matone, y = yy_mat,
                                                          I = I, Ic = Ic,
                                                          cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                                            params = params),
                                                          params = params))
            check_residual_form(res)
            rX <- res$rX
            rX_gW <- res$rX_gW
            rY <- res$rY
            rY_gW <- res$rY_gW
            rA <- res$rA

            res_gamma <- get_mat_vec_regDML2(rX = rX, rX_gW = rX_gW,
                                             rY = rY, rY_gW = rY_gW,
                                             gamma = 3, n = length(rY))
            expect_true(is.matrix(res_gamma$mat))
            expect_true(is.matrix(res_gamma$vec))
          })

test_that("beta_crosfit outputs right data formats", {
  res <- suppressWarnings(beta_crossfit(a = aa_mat, w = ww_frame,
                                        x = xx_mat, y = yy_mat, K = 2, gamma = 1:4,
                                        DML = "DML1", do_DML = TRUE, do_regDML = TRUE,
                                        cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                          params = NULL),
                                        params = params))
  expect_true(is.matrix(res$beta_DML))
  expect_true(is.matrix(res$beta_gamma))
  expect_true(is.matrix(res$as_var_DML))
  expect_true(is.array(res$as_var_gamma))

  expect_warning(regsdml(a = aa_mat, w = ww_frame,
                         x = xx_mat, y = yy_mat, K = 1, gamma = 1:4,
                         DML = "DML1", do_DML = TRUE, do_regDML = TRUE,
                         cond_method = c("spline", "spline", "spline"), S = 1))
})

test_that("sigma2_DML outputs right data formats", {
  check_sigma2_DML_formats <- function(res) {
    expect_true(is.matrix(res$beta_DML))
    expect_true(is.matrix(res$beta_gamma))
    expect_true(is.matrix(res$as_var_DML))
    expect_true(is.array(res$as_var_gamma))
  }

  res <- suppressWarnings(beta_crossfit(a = aa_mat, w = ww_frame,
                                        x = xx_mat, y = yy_mat,
                                        K = 2,
                                        gamma = 1:4,
                                        DML = "DML1",
                                        do_DML = TRUE,
                                        do_regDML = TRUE,
                                        cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                          params = NULL),
                                        params = params))
  check_sigma2_DML_formats(res)

  res <- suppressWarnings(beta_crossfit(a = aa_matone, w = ww_frame,
                                        x = xx_matone, y = yy_mat, K = 2, gamma = 1:4,
                                        DML = "DML1", do_DML = TRUE, do_regDML = TRUE,
                                        cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                          params = NULL),
                                        params = params))
  check_sigma2_DML_formats(res)

  res <- suppressWarnings(beta_crossfit(a = aa_matone, w = ww_frameone,
                                        x = xx_matone, y = yy_mat, K = 2, gamma = 1:4,
                                        DML = "DML1", do_DML = TRUE, do_regDML = TRUE,
                                        cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                          params = NULL),
                                        params = params))
  check_sigma2_DML_formats(res)

  res <- suppressWarnings(beta_crossfit(a = aa_mat, w = ww_frame,
                                        x = xx_mat, y = yy_mat, K = 2, gamma = 1:4,
                                        DML = "DML2", do_DML = TRUE, do_regDML = TRUE,
                                        cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                          params = NULL),
                                        params = params))
  check_sigma2_DML_formats(res)

  res <- suppressWarnings(beta_crossfit(a = aa_matone, w = ww_frame,
                                        x = xx_matone, y = yy_mat, K = 2, gamma = 1:4,
                                        DML = "DML2", do_DML = TRUE, do_regDML = TRUE,
                                        cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                          params = NULL),
                                        params = params))
  check_sigma2_DML_formats(res)

  res <- suppressWarnings(beta_crossfit(a = aa_matone, w = ww_frameone,
                                        x = xx_matone, y = yy_mat, K = 2, gamma = 1:4,
                                        DML = "DML2", do_DML = TRUE, do_regDML = TRUE,
                                        cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                          params = NULL),
                                        params = params))
  check_sigma2_DML_formats(res)
})

test_that("sigma2_DML_stable outputs right data formats", {
  check_sigma2_DML_formats_stable <- function(all_matrices, d) {
    sigma2 <- suppressWarnings(sigma2_DML_stable(all_residuals = all_matrices$all_residuals,
                                                 betahat = as.matrix(rep(1, d))))
    expect_true(is.matrix(sigma2))
    expect_true(all.equal(dim(sigma2), rep(d, 2)))
  }

  xx <- xx_mat
  all_matrices <- suppressWarnings(beta_get_matrices(a = aa_mat, w = ww_frame,
                                                     x = xx, y = yy_mat,
                                                     K = 2,
                                                     gamma = 1:4,
                                                     DML = "DML1",
                                                     do_DML = TRUE,
                                                     do_regDML = TRUE,
                                                     cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                                       params = NULL),
                                                     params = params))
  check_sigma2_DML_formats_stable(all_matrices = all_matrices, d = ncol(as.matrix(xx)))

  res <- suppressWarnings(beta_get_matrices(a = aa_mat, w = ww_frame,
                                            x = xx, y = yy_mat,
                                            K = 2,
                                            gamma = 1:4,
                                            DML = "DML1",
                                            do_DML = TRUE,
                                            do_regDML = TRUE,
                                            cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                              params = NULL),
                                            params = params))
  check_sigma2_DML_formats_stable(res, ncol(as.matrix(xx)))

  xx <- xx_matone
  res <- suppressWarnings(beta_get_matrices(a = aa_matone, w = ww_frame,
                                            x = xx, y = yy_mat, K = 2, gamma = 1:4,
                                            DML = "DML1", do_DML = TRUE, do_regDML = TRUE,
                                            cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                              params = NULL),
                                            params = params))
  check_sigma2_DML_formats_stable(res, ncol(as.matrix(xx)))

  res <- suppressWarnings(beta_get_matrices(a = aa_matone, w = ww_frameone,
                                            x = xx, y = yy_mat, K = 2, gamma = 1:4,
                                            DML = "DML1", do_DML = TRUE, do_regDML = TRUE,
                                            cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                              params = NULL),
                                            params = params))
  check_sigma2_DML_formats_stable(res, ncol(as.matrix(xx)))

  xx <- xx_mat
  res <- suppressWarnings(beta_get_matrices(a = aa_mat, w = ww_frame,
                                            x = xx, y = yy_mat, K = 2, gamma = 1:4,
                                            DML = "DML2", do_DML = TRUE, do_regDML = TRUE,
                                            cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                              params = NULL),
                                            params = params))
  check_sigma2_DML_formats_stable(res, ncol(as.matrix(xx)))

  xx <- xx_matone
  res <- suppressWarnings(beta_get_matrices(a = aa_matone, w = ww_frame,
                                            x = xx, y = yy_mat, K = 2, gamma = 1:4,
                                            DML = "DML2", do_DML = TRUE, do_regDML = TRUE,
                                            cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                              params = NULL),
                                            params = params))
  check_sigma2_DML_formats_stable(res, ncol(as.matrix(xx)))

  res <- suppressWarnings(beta_get_matrices(a = aa_matone, w = ww_frameone,
                                            x = xx, y = yy_mat, K = 2, gamma = 1:4,
                                            DML = "DML2", do_DML = TRUE, do_regDML = TRUE,
                                            cond_func_all = get_condexp_funcs(cond_method = c("spline", "spline", "forest"),
                                                                              params = NULL),
                                            params = params))
  check_sigma2_DML_formats_stable(res, ncol(as.matrix(xx)))
})

test_that("regsdml output right data formats", {
  check_regsdml_formats <- function(res) {
    expect_true(identical(class(res), c("regsdml", "list")))

    do_DML <- !is.null(res$DML_statistics)
    do_regDML <- !is.null(res$regDML_all_gamma_statistics)
    do_safety <- (!is.null(res$regDML_safety_statistics)) &&
      (res$regDML_safety_statistics$message_safety == "safety device applicable")
    do_regsDML <- !is.null(res$regsDML_statistics)

    if (do_DML) {
      res_DML <- res$DML_statistics
      d <- nrow(res_DML$beta_DML)

      expect_equal(nrow(res_DML$sd_DML), d)
      expect_equal(nrow(res_DML$var_DML), d)
      expect_equal(ncol(res_DML$var_DML), d)
      expect_equal(nrow(res_DML$pval_DML), d)
      expect_equal(nrow(res_DML$beta_DML), d)
      expect_equal(nrow(res_DML$CI_DML), d)
      expect_equal(ncol(res_DML$CI_DML), 2)

      expect_true(is.matrix(res_DML$beta_DML))
      expect_true(is.matrix(res_DML$sd_DML))
      expect_true(is.matrix(res_DML$var_DML))
      expect_true(is.matrix(res_DML$pval_DML))
      expect_true(is.matrix(res_DML$CI_DML))
    }

    if (do_regsDML) {
      res_sel <- res$regsDML_statistic
      d2 <- d2 <- nrow(res_sel$beta_regsDML)
      if (do_DML) {
        expect_equal(d, d2)
      }
      d <- d2
      expect_equal(nrow(res_sel$sd_regsDML), d)
      expect_equal(nrow(res_sel$var_regsDML), d)
      expect_equal(ncol(res_sel$var_regsDML), d)
      expect_equal(nrow(res_sel$beta_regsDML), d)
      expect_equal(nrow(res_sel$pval_regsDML), d)
      expect_equal(nrow(res_sel$CI_regsDML), d)
      expect_equal(ncol(res_sel$CI_regsDML), 2)

      expect_true(is.matrix(res_sel$beta_regsDML))
      expect_true(is.matrix(res_sel$sd_regsDML))
      expect_true(is.matrix(res_sel$var_regsDML))
      expect_true(is.matrix(res_sel$pval_regsDML))
      expect_true(is.matrix(res_sel$CI_regsDML))
    }

    if (do_safety) {
      res_safe <- res$regDML_safety_statistics
      d <- nrow(res_safe$beta_safety)
      expect_equal(nrow(res_safe$sd_safety), d)
      expect_equal(nrow(res_safe$var_safety), d)
      expect_equal(ncol(res_safe$var_safety), d)
      expect_equal(nrow(res_safe$pval_safety), d)
      expect_equal(nrow(res_safe$beta_safety), d)
      expect_equal(nrow(res_safe$CI_safety), d)
      expect_equal(ncol(res_safe$CI_safety), 2)

      expect_true(is.matrix(res_safe$beta_safety))
      expect_true(is.matrix(res_safe$sd_safety))
      expect_true(is.matrix(res_safe$var_safety))
      expect_true(is.matrix(res_safe$pval_safety))
      expect_true(is.matrix(res_safe$CI_safety))
    }

    if (do_regDML) {
      gammalen <- length(res$regDML_all_gamma_statistics)
      expect_equal(length(attributes(res)$gamma), gammalen)
      res_gamma <- res$regDML_all_gamma_statistics[[1]]

      expect_equal(nrow(res_gamma$sd_regDML_all_gamma), d)
      expect_equal(nrow(res_gamma$var_regDML_all_gamma), d)
      expect_equal(ncol(res_gamma$var_regDML_all_gamma), d)
      expect_equal(nrow(res_gamma$pval_regDML_all_gamma), d)
      expect_equal(nrow(res_gamma$beta_regDML_all_gamma), d)
      expect_equal(nrow(res_gamma$CI_regDML_all_gamma), d)
      expect_equal(ncol(res_gamma$CI_regDML_all_gamma), 2)

      expect_true(is.matrix(res_gamma$beta_regDML_all_gamma))
      expect_true(is.matrix(res_gamma$var_regDML_all_gamma))
      expect_true(is.matrix(res_gamma$sd_regDML_all_gamma))
      expect_true(is.matrix(res_gamma$pval_regDML_all_gamma))
      expect_true(is.matrix(res_gamma$CI_regDML_all_gamma))
    }

  }

  res <- suppressWarnings(regsdml(a = aa_mat, w = ww_frame, x = xx_mat,
                                  y = yy_mat,
                                  DML = "DML2", K = 2,
                                  cond_method = rep("spline", 3),
                                  S = 10,
                                  gamma = exp(seq(-4, 10, length.out = 20)),
                                  do_safety = TRUE,
                                  do_regDML_all_gamma = TRUE))
  check_regsdml_formats(res)

  res <- suppressWarnings(regsdml(a = aa_matone, w = ww_frameone,
                                  x = xx_matone, y = yy_mat,
                                  DML = "DML2", K = 2,
                                  cond_method = rep("spline", 3),
                                  S = 10,
                                  gamma = exp(seq(-4, 10, length.out = 20)),
                                  do_safety = TRUE,
                                  do_regDML_all_gamma = TRUE))
  check_regsdml_formats(res)

  res <- suppressWarnings(regsdml(a = aa_mat, w = ww_frame, x = xx_mat,
                                  y = yy_mat,
                                  DML = "DML1", K = 2,
                                  cond_method = rep("spline", 3),
                                  S = 10,
                                  gamma = exp(seq(-4, 10, length.out = 20)),
                                  do_safety = TRUE))
  check_regsdml_formats(res)
  res <- suppressWarnings(regsdml(a = aa_matone, w = ww_frameone,
                                  x = xx_matone, y = yy_mat,
                                  DML = "DML1", K = 2,
                                  cond_method = rep("spline", 3),
                                  S = 10,
                                  gamma = exp(seq(-4, 10, length.out = 20)),
                                  do_safety = TRUE,
                                  do_regDML_all_gamma = TRUE))
  check_regsdml_formats(res)
})

test_that("confint.regsdml, summary.regsdml,  print.regsdml, vcov.regsdml,
          and coef.regsdml output right data formats", {
            # check different DML outputs
            check_variable_dml_outputs <- function(xx_new,
                                                   do_safety,
                                                   safety_factor,
                                                   do_DML,
                                                   do_regDML_all_gamma,
                                                   do_regsDML,
                                                   do_regDML,
                                                   print_gamma = FALSE,
                                                   parm = NULL,
                                                   expect_length_res,
                                                   expect_length_method_summary,
                                                   expect_length_method_ci,
                                                   expect_names_res,
                                                   expect_names_summary,
                                                   expect_names_ci,
                                                   expect_names_coef,
                                                   correlation = FALSE,
                                                   xx_colnames = NULL,
                                                   level = 0.95) {

              print_DML <- do_DML
              print_regDML <- do_regDML
              print_regsDML <- do_regsDML
              print_regDML_all_gamma <- do_regDML_all_gamma
              print_safety <- do_safety

              set.seed(22)
              res <- suppressWarnings(do.call(regsdml,
                                              c(params,
                                                list(x = xx_new,
                                                     do_safety = do_safety,
                                                     safety_factor = safety_factor,
                                                     do_DML = do_DML,
                                                     do_regDML_all_gamma = do_regDML_all_gamma,
                                                     do_regsDML = do_regsDML,
                                                     do_regDML = do_regDML,
                                                     level = level))))

              # check output length of regsdml
              expect_length(res, expect_length_res)
              expect_true(all.equal(names(res), expect_names_res))

              # check summary
              v <- summary(res, parm = parm, correlation = correlation,
                           print_DML = print_DML, print_regsDML = print_regsDML,
                           print_regDML = print_regDML, print_regDML_all_gamma = print_regDML_all_gamma,
                           print_safety = print_safety, print_gamma = print_gamma)
              expect_length(v, expect_length_method_summary)
              expect_true(all.equal(names(v), expect_names_summary))

              # check confidence intervals
              w <- confint(res, level = level, parm = parm, correlation = correlation,
                           print_DML = print_DML, print_regsDML = print_regsDML,
                           print_regDML = print_regDML, print_regDML_all_gamma = print_regDML_all_gamma,
                           print_safety = print_safety, print_gamma = print_gamma)
              expect_length(w, expect_length_method_ci)
              expect_true(all.equal(names(w), expect_names_ci))

              # check coefficients
              z <- coef(res, parm = parm,
                        print_DML = print_DML, print_regsDML = print_regsDML,
                        print_regDML = print_regDML, print_regDML_all_gamma = print_regDML_all_gamma,
                        print_safety = print_safety, print_gamma = print_gamma)
              dim_coef_mat <- c(attr(res, "d") + print_gamma, print_DML + print_regsDML + print_regDML + print_safety + print_regDML_all_gamma * length(parm))
              expect_true(all.equal(dim(z), dim_coef_mat))
              expect_true(all.equal(colnames(z), expect_names_coef))

              # check variance-covariance matrices
              varcov <- vcov(res, parm = parm,
                             print_DML = print_DML, print_regsDML = print_regsDML,
                             print_regDML = print_regDML, print_regDML_all_gamma = print_regDML_all_gamma,
                             print_safety = print_safety, print_gamma = print_gamma)
              expect_equal(length(varcov), dim_coef_mat[2])
              summary_full <- summary(res, parm = parm, correlation = TRUE,
                                      print_DML = print_DML, print_regsDML = print_regsDML,
                                      print_regDML = print_regDML, print_regDML_all_gamma = print_regDML_all_gamma,
                                      print_safety = print_safety, print_gamma = print_gamma)
              expect_length(v, expect_length_method_summary)
              expect_true(all.equal(names(varcov), names(summary_full)[(1 + length(summary_full) / 2):length(summary_full)]))

              # check names of summary and confidence intervals and variance-covariance matrices
              if (!is.null(xx_colnames)) {
                for (i in 1:expect_length_method_summary) {
                  expect_true(all.equal(rownames(v[[i]]), xx_colnames))
                }
                for (i in 1:expect_length_method_ci) {
                  expect_true(all.equal(rownames(w[[i]]), xx_colnames))
                }
                for (i in 1:expect_length_method_ci) {
                  expect_true(all.equal(rownames(varcov[[i]]), colnames(varcov[[i]]),
                                        xx_colnames))
                }
                if (print_gamma) {
                  expect_true(all.equal(rownames(z), c("gamma", xx_colnames)))
                } else {
                  expect_true(all.equal(rownames(z), xx_colnames))
                }
              }
            }

            params <- list(a = aa_mat, w = ww_frame,
                           y = yy_mat,
                           DML = "DML2", K = 2,
                           cond_method = rep("spline", 3),
                           S = 10,
                           gamma = exp(seq(-4, 10, length.out = 20)))

            xx_new <- xx_mat
            xx_colnames <- as.character(seq(from = 10, to = ncol(xx_new) + 9))
            colnames(xx_new) <- xx_colnames
            check_variable_dml_outputs(xx_new = xx_new,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       print_gamma = FALSE,
                                       expect_length_res = 2,
                                       expect_length_method_summary =  4,
                                       expect_length_method_ci = 2,
                                       level = 0.8,
                                       expect_names_res = c("regDML_safety_statistics", "DML_statistics"),
                                       expect_names_summary = c("safety-device",
                                                                "DML",
                                                                "safety-device",
                                                                "DML"),
                                       expect_names_ci = c("safety-device", "DML"),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("safety-device", "DML"))

            check_variable_dml_outputs(xx_new = xx_new,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       print_gamma = TRUE,
                                       expect_length_res = 2,
                                       expect_length_method_summary =  4,
                                       expect_length_method_ci = 2,
                                       level = 0.8,
                                       expect_names_res = c("regDML_safety_statistics", "DML_statistics"),
                                       expect_names_summary = c("safety-device (factor = 0.001, gamma = 1.83e-02)", "DML",
                                                                "safety-device (factor = 0.001, gamma = 1.83e-02)", "DML"),
                                       expect_names_ci = c("safety-device (factor = 0.001, gamma = 1.83e-02)", "DML"),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("safety-device", "DML"))

            xx_new <- xx_mat
            xx_colnames <- as.character(seq(from = 11, to = ncol(xx_new) + 10))
            colnames(xx_new) <- xx_colnames
            check_variable_dml_outputs(xx_new = xx_new,
                                       expect_length_res = 1,
                                       expect_length_method_summary =  2,
                                       expect_length_method_ci = 1,
                                       level = 0.8,
                                       do_safety = FALSE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       print_gamma = FALSE,
                                       expect_names_res = c("DML_statistics"),
                                       expect_names_summary = c("DML", "DML"),
                                       expect_names_ci = c("DML"),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       expect_length_res = 1,
                                       expect_length_method_summary =  2,
                                       expect_length_method_ci = 1,
                                       level = 0.8,
                                       do_safety = FALSE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       print_gamma = TRUE,
                                       expect_names_res = c("DML_statistics"),
                                       expect_names_summary = c("DML", "DML"),
                                       expect_names_ci = c("DML"),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML"))

            expect_error(suppressWarnings(do.call(regsdml,
                                                  c(params,
                                                    list(x = xx_new,
                                                         do_safety = TRUE,
                                                         safety_factor = 0.0001,
                                                         do_DML = FALSE,
                                                         do_regDML_all_gamma = TRUE,
                                                         do_regsDML = FALSE)))))
            expect_error(suppressWarnings(do.call(regsdml,
                                                  c(params,
                                                    list(x = xx_new,
                                                         do_safety = FALSE,
                                                         safety_factor = 0.0001,
                                                         do_DML = FALSE,
                                                         do_regDML_all_gamma = FALSE,
                                                         do_regDML = TRUE)))))
            expect_error(suppressWarnings(do.call(regsdml,
                                                  c(params,
                                                    list(x = xx_new,
                                                         do_safety = TRUE,
                                                         safety_factor = 0.0001,
                                                         do_DML = FALSE,
                                                         do_regDML_all_gamma = FALSE,
                                                         do_regsDML = FALSE)))))
            xx_new <- xx_matone
            xx_colnames <- as.character(seq(from = 10, to = ncol(xx_new) + 9))
            colnames(xx_new) <- xx_colnames
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = FALSE,
                                       do_safety = TRUE,
                                       safety_factor = 0.0001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       expect_length_res = 3,
                                       expect_length_method_summary = 8,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("regDML_safety_statistics", "DML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("safety-device", "DML", "regDMLall", "regDMLall",
                                                                "safety-device", "DML", "regDMLall", "regDMLall"),
                                       expect_names_ci = c("safety-device", "DML", "regDMLall", "regDMLall"),
                                       parm = c(1, 20),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("safety-device", "DML", "regDMLall", "regDMLall"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = TRUE,
                                       do_safety = TRUE,
                                       safety_factor = 0.0001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       expect_length_res = 3,
                                       expect_length_method_summary = 8,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("regDML_safety_statistics", "DML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("safety-device (factor = 1e-04, gamma = 1.83e-02)", "DML",
                                                                "regDMLall (1.83e-02)", "regDMLall (2.20e+04)",
                                                                "safety-device (factor = 1e-04, gamma = 1.83e-02)", "DML",
                                                                "regDMLall (1.83e-02)", "regDMLall (2.20e+04)"),
                                       expect_names_ci = c("safety-device (factor = 1e-04, gamma = 1.83e-02)", "DML",
                                                           "regDMLall (1.83e-02)", "regDMLall (2.20e+04)"),
                                       parm = c(1, 20),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("safety-device", "DML", "regDMLall", "regDMLall"))

            xx_new <- xx_matone
            xx_colnames <- as.character(seq(from = 10, to = ncol(xx_new) + 9))
            colnames(xx_new) <- xx_colnames
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = FALSE,
                                       do_safety = FALSE,
                                       safety_factor = 0.0001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       expect_length_res = 2,
                                       expect_length_method_summary = 6,
                                       expect_length_method_ci = 3,
                                       expect_names_res = c("DML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("DML", "regDMLall",
                                                                "regDMLall", "DML",
                                                                "regDMLall", "regDMLall"),
                                       expect_names_ci = c("DML", "regDMLall", "regDMLall"),
                                       parm = c(1, 20),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML", "regDMLall", "regDMLall"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = TRUE,
                                       do_safety = FALSE,
                                       safety_factor = 0.0001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       expect_length_res = 2,
                                       expect_length_method_summary = 6,
                                       expect_length_method_ci = 3,
                                       expect_names_res = c("DML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("DML",
                                                                "regDMLall (1.83e-02)",
                                                                "regDMLall (2.20e+04)",
                                                                "DML", "regDMLall (1.83e-02)",
                                                                "regDMLall (2.20e+04)"),
                                       expect_names_ci = c("DML", "regDMLall (1.83e-02)",
                                                           "regDMLall (2.20e+04)"),
                                       parm = c(1, 20),
                                       correlation = TRUE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML", "regDMLall", "regDMLall"))

            xx_new <- xx_mat
            xx_colnames <- as.character(seq(from = 10, to = ncol(xx_new) + 9))
            colnames(xx_new) <- xx_colnames
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = FALSE,
                                       do_safety = FALSE,
                                       safety_factor = 0.0001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = FALSE,
                                       do_regDML = TRUE,
                                       expect_length_res = 2,
                                       expect_length_method_summary = 4,
                                       expect_length_method_ci = 2,
                                       expect_names_res = c("DML_statistics", "regDML_statistics"),
                                       expect_names_summary = c("DML", "regDML", "DML", "regDML"),
                                       expect_names_ci = c("DML", "regDML"),
                                       correlation = TRUE, xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML", "regDML"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = TRUE,
                                       do_safety = FALSE,
                                       safety_factor = 0.0001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = FALSE,
                                       do_regDML = TRUE,
                                       expect_length_res = 2,
                                       expect_length_method_summary = 4,
                                       expect_length_method_ci = 2,
                                       expect_names_res = c("DML_statistics", "regDML_statistics"),
                                       expect_names_summary = c("DML", "regDML (1.52e+00)",
                                                                "DML", "regDML (1.52e+00)"),
                                       expect_names_ci = c("DML", "regDML (1.52e+00)"),
                                       correlation = TRUE, xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML", "regDML"))

            xx_new <- xx_mat
            xx_colnames <- as.character(seq(from = 11, to = ncol(xx_new) + 10))
            colnames(xx_new) <- xx_colnames
            res <- suppressWarnings(do.call(regsdml,
                                            c(params,
                                              list(x = xx_new,
                                                   do_safety = TRUE,
                                                   safety_factor = 0.001,
                                                   do_DML = TRUE,
                                                   do_regDML_all_gamma = FALSE,
                                                   do_regsDML = TRUE))))
            expect_error(summary(res, parm = c(1, 2)))
            expect_error(confint(res, parm = c(1, 2)))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = FALSE,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = TRUE,
                                       do_regDML = FALSE,
                                       expect_length_res = 3,
                                       expect_length_method_summary = 6,
                                       expect_length_method_ci = 3,
                                       expect_names_res = c("regsDML_statistics", "regDML_safety_statistics", "DML_statistics"),
                                       expect_names_summary = c("regsDML", "safety-device", "DML", "regsDML",
                                                                "safety-device", "DML"),
                                       expect_names_ci = c("regsDML", "safety-device", "DML"),
                                       correlation = TRUE, xx_colnames = xx_colnames,
                                       expect_names_coef = c("regsDML", "safety-device", "DML"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = TRUE,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = TRUE,
                                       do_regDML = FALSE,
                                       expect_length_res = 3,
                                       expect_length_method_summary = 6,
                                       expect_length_method_ci = 3,
                                       expect_names_res = c("regsDML_statistics", "regDML_safety_statistics", "DML_statistics"),
                                       expect_names_summary = c("regsDML (1.52e+00)",
                                                                "safety-device (factor = 0.001, gamma = 1.83e-02)", "DML",
                                                                "regsDML (1.52e+00)",
                                                                "safety-device (factor = 0.001, gamma = 1.83e-02)", "DML"),
                                       expect_names_ci = c("regsDML (1.52e+00)",
                                                           "safety-device (factor = 0.001, gamma = 1.83e-02)", "DML"),
                                       correlation = TRUE, xx_colnames = xx_colnames, parm = NULL,
                                       expect_names_coef = c("regsDML", "safety-device", "DML"))

            xx_new <- xx_matone
            xx_colnames <- as.character(seq(from = 10, to = ncol(xx_new) + 9))
            colnames(xx_new) <- xx_colnames
            res <- suppressWarnings(do.call(regsdml,
                                            c(params,
                                              list(x = xx_new,
                                                   do_safety = TRUE,
                                                   safety_factor = 0.001,
                                                   do_DML = TRUE,
                                                   do_regDML_all_gamma = FALSE,
                                                   do_regsDML = TRUE,
                                                   do_regDML = TRUE))))
            expect_error(summary(res, parm = c(1, 2)))
            expect_error(confint(res, parm = c(1, 2)))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = FALSE,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = TRUE,
                                       do_regDML = TRUE,
                                       expect_length_res = 4,
                                       expect_length_method_summary = 8,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("regsDML_statistics", "regDML_safety_statistics", "DML_statistics", "regDML_statistics"),
                                       expect_names_summary = c("regsDML", "safety-device", "DML", "regDML", "regsDML",
                                                                "safety-device", "DML", "regDML"),
                                       expect_names_ci = c("regsDML", "safety-device", "DML", "regDML"),
                                       correlation = TRUE, xx_colnames = xx_colnames,
                                       expect_names_coef = c("regsDML", "safety-device", "DML", "regDML"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = TRUE,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = FALSE,
                                       do_regsDML = TRUE,
                                       do_regDML = TRUE,
                                       expect_length_res = 4,
                                       expect_length_method_summary = 8,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("regsDML_statistics", "regDML_safety_statistics", "DML_statistics", "regDML_statistics"),
                                       expect_names_summary = c("regsDML (6.65e+00)", "safety-device (factor = 0.001, gamma = 1.83e-02)",
                                                                "DML", "regDML (6.65e+00)",
                                                                "regsDML (6.65e+00)", "safety-device (factor = 0.001, gamma = 1.83e-02)",
                                                                "DML", "regDML (6.65e+00)"),
                                       expect_names_ci = c("regsDML (6.65e+00)", "safety-device (factor = 0.001, gamma = 1.83e-02)",
                                                           "DML", "regDML (6.65e+00)"),
                                       correlation = TRUE, xx_colnames = xx_colnames,
                                       expect_names_coef = c("regsDML", "safety-device", "DML", "regDML"))

            xx_new <- xx_matone
            xx_colnames <- as.character(seq(from = 10, to = ncol(xx_new) + 9))
            colnames(xx_new) <- xx_colnames
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = FALSE,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       level = 0.99, expect_length_res = 3,
                                       expect_length_method_summary = 4,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("regDML_safety_statistics", "DML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("safety-device", "DML", "regDMLall", "regDMLall"),
                                       expect_names_ci = c("safety-device", "DML", "regDMLall", "regDMLall"),
                                       parm = c(1, 20),
                                       xx_colnames = xx_colnames,
                                       correlation = FALSE,
                                       expect_names_coef = c("safety-device", "DML", "regDMLall", "regDMLall"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = TRUE,
                                       do_safety = TRUE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = FALSE,
                                       level = 0.99, expect_length_res = 3,
                                       expect_length_method_summary = 4,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("regDML_safety_statistics", "DML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("safety-device (factor = 0.001, gamma = 1.83e-02)", "DML", "regDMLall (1.83e-02)",
                                                                "regDMLall (2.20e+04)"),
                                       expect_names_ci = c("safety-device (factor = 0.001, gamma = 1.83e-02)", "DML", "regDMLall (1.83e-02)",
                                                           "regDMLall (2.20e+04)"),
                                       parm = c(1, 20),
                                       xx_colnames = xx_colnames,
                                       correlation = FALSE,
                                       expect_names_coef = c("safety-device", "DML", "regDMLall", "regDMLall"))

            xx_new <- xx_mat
            xx_colnames <- as.character(seq(from = 10, to = ncol(xx_new) + 9))
            colnames(xx_new) <- xx_colnames
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = FALSE,
                                       do_safety = FALSE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = TRUE,
                                       level = 0.99, expect_length_res = 3,
                                       expect_length_method_summary = 4,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("DML_statistics", "regDML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("DML", "regDML", "regDMLall", "regDMLall"),
                                       expect_names_ci = c("DML", "regDML", "regDMLall", "regDMLall"),
                                       parm = c(1, 20), correlation = FALSE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML", "regDML", "regDMLall", "regDMLall"))
            check_variable_dml_outputs(xx_new = xx_new,
                                       print_gamma = TRUE,
                                       do_safety = FALSE,
                                       safety_factor = 0.001,
                                       do_DML = TRUE,
                                       do_regDML_all_gamma = TRUE,
                                       do_regsDML = FALSE,
                                       do_regDML = TRUE,
                                       level = 0.99, expect_length_res = 3,
                                       expect_length_method_summary = 4,
                                       expect_length_method_ci = 4,
                                       expect_names_res = c("DML_statistics", "regDML_statistics", "regDML_all_gamma_statistics"),
                                       expect_names_summary = c("DML", "regDML (1.52e+00)", "regDMLall (1.83e-02)", "regDMLall (2.20e+04)"),
                                       expect_names_ci = c("DML", "regDML (1.52e+00)", "regDMLall (1.83e-02)", "regDMLall (2.20e+04)"),
                                       parm = c(1, 20), correlation = FALSE,
                                       xx_colnames = xx_colnames,
                                       expect_names_coef = c("DML", "regDML", "regDMLall", "regDMLall"))
            expect_error(suppressWarnings(regsdml(a = aa_mat, w = ww_frame, x = xx_mat,
                                                  y = yy_mat,
                                                  DML = "DML2", K = 2,
                                                  cond_method = rep("spline", 3),
                                                  S = 10,
                                                  gamma = exp(seq(-4, 10, length.out = 20)),
                                                  do_safety = TRUE,
                                                  safety_factor = 0.001,
                                                  do_DML = FALSE,
                                                  do_regDML_all_gamma = FALSE,
                                                  do_regsDML = TRUE)))
          })

test_that("get_CI_DML outputs right data formats", {
  check_CI_format <- function(res) {
    alpha <- 0.05
    CI <- get_CI_DML(beta = res$DML_statistics$beta_DML,
                     sd = res$DML_statistics$sd_DML,
                     alpha = alpha)
    expect_true(is.matrix(CI))
    expect_true(all.equal(colnames(CI), c(paste((alpha / 2) * 100, " %", sep = ""),
                                          paste((1 - alpha / 2) * 100, " %", sep = ""))))
    expect_true(all.equal(rownames(CI), rownames(res$DML_statistics$beta_DML)))
  }

  xx <- xx_matone
  res <- suppressWarnings(regsdml(a = aa_matone, w = ww_frameone,
                                  x = xx, y = yy_mat, S = 1))
  check_CI_format(res)

  xx <- xx_mat
  res <- suppressWarnings(regsdml(a = aa_mat, w = ww_frameone,
                                  x = xx, y = yy_mat, S = 1))
  check_CI_format(res)
})

test_that("return_results outputs right data formats", {
  check_return_results_format <- function(beta, var, xx_colnames, method = "test") {
    alpha <- 0.03
    results <- return_results(beta = beta,
                              var = var,
                              alpha = alpha,
                              xx_colnames = xx_colnames,
                              method = method)
    d <- length(beta)
    expect_true(is.matrix(results$beta_test))
    expect_true(is.matrix(results$sd_test))
    expect_true(is.matrix(results$var_test))
    expect_true(is.matrix(results$pval_test))
    expect_true(is.matrix(results$CI_test))

    expect(nrow(results$beta_test), d)
    expect(ncol(results$beta_test), 1)
    expect(nrow(results$sd_test), d)
    expect(ncol(results$sd_test), 1)
    expect(nrow(results$var_test), d)
    expect(ncol(results$var_test), d)
    expect(nrow(results$pval_test), d)
    expect(ncol(results$pval_test), 1)
    expect(nrow(results$CI_test), d)
    expect(ncol(results$CI_test), 2)

    expect_true(all.equal(rownames(results$beta_test), xx_colnames))
    expect_true(all.equal(rownames(results$sd_test), xx_colnames))
    expect_true(all.equal(rownames(results$var_test), xx_colnames))
    expect_true(all.equal(colnames(results$var_test), xx_colnames))
    expect_true(all.equal(rownames(results$pval_test), xx_colnames))
    expect_true(all.equal(rownames(results$CI_test), xx_colnames))
    expect_true(all.equal(colnames(results$CI_test), c(paste((alpha / 2) * 100, " %", sep = ""),
                                                       paste((1 - alpha / 2) * 100, " %", sep = ""))))
  }

  beta <- c(1, 2, 3)
  var <- matrix(rnorm(9) ^ 2, ncol = 3, nrow = 3)
  xx_colnames <- c("one", "two", "three")
  check_return_results_format(beta = beta, var = var, xx_colnames = xx_colnames)

  beta <- 3
  var <- 5
  xx_colnames <- "one"
  check_return_results_format(beta = beta, var = var, xx_colnames = xx_colnames)
})

test_that("initial_setup outputs right data formats", {
  init <- initial_setup(parallel = "no",
                        ncpus = 1L,
                        cl = NULL)
  expect_equal(init, FALSE)

  init <- initial_setup(parallel = "snow",
                        ncpus = 1L,
                        cl = 1)
  expect_equal(init, TRUE)
  expect_error(regsdml(a = aa_mat, x = xx_mat, w = ww_frame, y = yy_mat,
                       DML = "D"))
  expect_error(regsdml(a = aa_mat, x = xx_mat, w = ww_frame, y = yy_mat,
                       parallel = "C"))

  if (.Platform$OS.type == "windows") {
    expect_error(initial_setup(parallel = "multicore",
                               ncpus = 3L, cl = NULL))
  }
})

test_that("batch_correction outputs right data formats", {
  batch_check <- function(aa, ww, xx, yy, K, gamma, DML, cond_method,
                          do_parallel, parallel, S, ncpus, cl, params = NULL) {
    batch <- batch_correction(aa, ww, xx, yy, K, gamma, DML,
                              do_DML = TRUE,
                              do_regsDML = FALSE,
                              do_regDML = FALSE,
                              do_regDML_all_gamma = TRUE,
                              safety = FALSE,
                              cond_method = cond_method,
                              params = params,
                              do_parallel = do_parallel,
                              parallel = parallel, S = S,
                              ncpus = ncpus, cl = cl)
    expect_equal(length(batch), S)
    batch1 <- batch[[1]]$value
    expect_true(is.matrix(batch1$beta_DML))
    expect_true(is.matrix(batch1$beta_gamma))
    expect_true(is.matrix(batch1$as_var_DML))
    expect_true(is.array(batch1$as_var_gamma))

    d <- ncol(xx)
    gammalen <- length(gamma)
    expect_true(all.equal(dim(batch1$beta_DML), c(d, 1)))
    expect_true(all.equal(dim(batch1$beta_gamma), c(d, gammalen)))
    expect_true(all.equal(dim(batch1$as_var_DML), c(d, d)))
    expect_true(all.equal(dim(batch1$as_var_gamma), c(d, d, gammalen)))
  }

  if (.Platform$OS.type != "windows") {
    aa <- aa_mat
    xx <- xx_matone
    ww <- ww_frame
    yy <- yy_mat
    K <- 2L
    gamma <- 1:4
    DML <- "DML1"
    cond_method <- rep("spline", 3)
    do_parallel <- TRUE
    S <- 6L
    ncpus <- 2L
    parallel <- "multicore"
    cl <- NULL
    batch_check(aa, ww, xx, yy, K, gamma, DML, cond_method,
                do_parallel, parallel, S, ncpus, cl)

    xx <- xx_mat
    batch_check(aa, ww, xx, yy, K, gamma, DML, cond_method,
                do_parallel, parallel, S, ncpus, cl)
  }
})

test_that("params setups", {
  test_params_setup <- function(cond_method, params) {
    expect_error(suppressWarnings(regsdml(a = aa_mat, w = ww_frame,
                                          x = xx_mat, y = yy_mat,
                                          DML = "DML2",
                                          K = 2L,
                                          cond_method = cond_method,
                                          params = params,
                                          S = 3L,
                                          gamma = exp(seq(-4, 2, length.out = 10)),
                                          do_safety = FALSE,
                                          safety_factor = 0.7,
                                          parallel = "no",
                                          ncpus = 1L, cl = NULL)),
                 NA)
  }

  cond_method <- rep("spline", 3)
  params <- list(NULL, NULL, list(degree = 1))
  test_params_setup(cond_method, params)

  params <- list(list(degree = 2), list(degree = 2), NULL)
  test_params_setup(cond_method, params)

  params <- list(list(degree = 2), list(degree = 2), list(degree = 2))
  test_params_setup(cond_method, params)

  cond_method <- c("forest", "spline", "forest")
  params <- list(list(ntree = 3), NULL, list(nodesize = 5))
  test_params_setup(cond_method, params)

  cond_method <- c("spline", "ols", "spline")
  params <- list(list(degree = 2), NULL, list(degree = 2))
  test_params_setup(cond_method, params)
})
