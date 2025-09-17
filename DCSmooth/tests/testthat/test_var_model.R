################################################################################
#                                                                              #
#                     Test if variance estimation is correct                   #
#                                                                              #
################################################################################


context("Variance Estimation")

test_that("model order is actually used", {
  set.seed(123)
  ar = c(1, -0.3) %*% t(c(1, 0.2))
  ma = c(1, 0.5) %*% t(c(1, -0.1))
  Y = y.norm1 + rnorm(101^2)
  model_order_test = list(ar = c(2, 1), ma = c(1, 3))
  dcs_iid = dcs(Y, set.options(var_model = "iid",
                               model_order = model_order_test))
  Y = y.norm1 + sarma.sim(101, 101, model = list(ar = ar, ma = ma, sigma = 1))$Y
  dcs_qarma = dcs(Y, set.options(var_model = "sarma_HR", 
                                 model_order = model_order_test))
  dcs_sarma = dcs(Y, set.options(var_model = "sarma_sep", 
                                 model_order = model_order_test))
  Y = y.norm1 + sfarima.sim(101, 101, model = list(ar = ar, ma = ma, 
                                                   d = c(0.1, 0.1), sigma = 1))$Y
  dcs_lm = dcs(Y, set.options(var_model = "sfarima_RSS",
                              model_order = model_order_test))
  
  expect_equal(dim(dcs_iid$var_est$model$ar), NULL)
  expect_equal(dim(dcs_iid$var_est$model$ma), NULL)
  expect_equal(dim(dcs_qarma$var_est$model$ar), c(3, 2))
  expect_equal(dim(dcs_qarma$var_est$model$ma), c(2, 4))
  expect_equal(dim(dcs_sarma$var_est$model$ar), c(3, 2))
  expect_equal(dim(dcs_sarma$var_est$model$ma), c(2, 4))
  expect_equal(dim(dcs_lm$var_est$model$ar), c(3, 2))
  expect_equal(dim(dcs_lm$var_est$model$ma), c(2, 4))
})

test_that("pure SAR and SMA models can be estimated", {
  set.seed(123)
  order_ar = list(ar = c(1, 1), ma = c(0, 0))
  order_ma = list(ar = c(0, 0), ma = c(1, 1))
  ar = c(1, -0.3) %*% t(c(1, 0.2))
  ma = c(1, 0.5) %*% t(c(1, -0.1))
  Y_SAR = y.norm1 + sarma.sim(101, 101, 
                              model = list(ar = ar, ma = 1, sigma = 1))$Y
  Y_SMA = y.norm1 + sarma.sim(101, 101, 
                              model = list(ar = 1, ma = ma, sigma = 1))$Y
  
  SAR_HR = sarma.est(Y_SAR, method = "HR", model_order = order_ar)
  SAR_sep = sarma.est(Y_SAR, method = "sep", model_order = order_ar)
  SAR_RSS = sarma.est(Y_SAR, method = "RSS", model_order = order_ar)
  
  SMA_HR = sarma.est(Y_SMA, method = "HR", model_order = order_ma)
  SMA_sep = sarma.est(Y_SMA, method = "sep", model_order = order_ma)
  SMA_RSS = sarma.est(Y_SMA, method = "RSS", model_order = order_ma)
  
  expect_equal(dim(SAR_HR$model$ar), c(2, 2))
  expect_true(is.numeric(SAR_HR$model$ar))
  expect_equal(dim(SAR_HR$model$ma), c(1, 1))
  expect_equal(SAR_HR$model$ma[1], 1)
  expect_true(all(is.numeric(SAR_HR$innov)))
  
  expect_equal(dim(SAR_sep$model$ar), c(2, 2))
  expect_true(is.numeric(SAR_sep$model$ar))
  expect_equal(dim(SAR_sep$model$ma), c(1, 1))
  expect_equal(SAR_sep$model$ma[1], 1)
  expect_true(all(is.numeric(SAR_sep$innov)))
  
  expect_equal(dim(SAR_RSS$model$ar), c(2, 2))
  expect_true(is.numeric(SAR_RSS$model$ar))
  expect_equal(dim(SAR_RSS$model$ma), c(1, 1))
  expect_equal(SAR_RSS$model$ma[1], 1)
  expect_true(all(is.numeric(SAR_RSS$innov)))
  
  expect_equal(dim(SMA_HR$model$ma), c(2, 2))
  expect_true(is.numeric(SMA_HR$model$ma))
  expect_equal(dim(SMA_HR$model$ar), c(1, 1))
  expect_equal(SMA_HR$model$ar[1], 1)
  expect_true(all(is.numeric(SMA_HR$innov)))
  
  expect_equal(dim(SMA_sep$model$ma), c(2, 2))
  expect_true(is.numeric(SMA_sep$model$ma))
  expect_equal(dim(SMA_sep$model$ar), c(1, 1))
  expect_equal(SMA_sep$model$ar[1], 1)
  expect_true(all(is.numeric(SMA_sep$innov)))
  
  expect_equal(dim(SMA_RSS$model$ma), c(2, 2))
  expect_true(is.numeric(SMA_RSS$model$ma))
  expect_equal(dim(SMA_RSS$model$ar), c(1, 1))
  expect_equal(SMA_RSS$model$ar[1], 1)
  expect_true(all(is.numeric(SMA_HR$innov)))
})

test_that("pure SFAR and SFMA models can be estimated", {
  set.seed(123)
  order_ar = list(ar = c(1, 1), ma = c(0, 0))
  order_ma = list(ar = c(0, 0), ma = c(1, 1))
  ar = c(1, -0.3) %*% t(c(1, 0.2))
  ma = c(1, 0.5) %*% t(c(1, -0.1))
  Y_SFAR = y.norm1 + sfarima.sim(101, 101, model = list(ar = ar, ma = 1,
                                        d = c(0.1, 0.2), sigma = 1))$Y
  Y_SFMA = y.norm1 + sfarima.sim(101, 101, model = list(ar = 1, ma = ma,
                                        d = c(0.1, 0.2), sigma = 1))$Y
  
  SFAR = sfarima.est(Y_SFAR, model_order = order_ar)
  SFMA = sfarima.est(Y_SFMA, model_order = order_ma)
  
  expect_equal(dim(SFAR$model$ar), c(2, 2))
  expect_true(is.numeric(SFAR$model$ar))
  expect_equal(dim(SFAR$model$ma), c(1, 1))
  expect_equal(SFAR$model$ma[1], 1)
  expect_true(all(is.numeric(SFAR$innov)))
  
  expect_equal(dim(SFMA$model$ma), c(2, 2))
  expect_true(is.numeric(SFMA$model$ma))
  expect_equal(dim(SFMA$model$ar), c(1, 1))
  expect_equal(SFMA$model$ar[1], 1)
  expect_true(all(is.numeric(SFMA$innov)))
})

# Test if smoothing works correctly
context("DCS works correctly (smoothing)")

test_that("given bandwidths are used", {
  set.seed(123)
  Y = matrix(rnorm(101^2), nrow = 101, ncol = 101)
  dcs_KR = dcs(Y, h = c(0.1, 0.1), dcs_options = set.options(type = "KR"))
  dcs_LP = dcs(Y, h = c(0.1, 0.1), dcs_options = set.options(type = "LP"))
  expect_equal(dcs_KR$h, c(0.1, 0.1))
  expect_equal(is.numeric(dcs_KR$M), TRUE)
  expect_equal(dim(dcs_KR$M), c(101, 101))
  expect_equal(dcs_LP$h, c(0.1, 0.1))
  expect_equal(is.numeric(dcs_LP$M), TRUE)
  expect_equal(dim(dcs_LP$M), c(101, 101))
})

test_that("error term model is estimated under given bandwidths", {
  set.seed(123)
  Y = matrix(rnorm(101^2), nrow = 101, ncol = 101)
  dcs_KR = dcs(Y, h = c(0.1, 0.1), dcs_options = set.options(type = "KR"))
  dcs_LP = dcs(Y, h = c(0.1, 0.1), dcs_options = set.options(type = "LP"))
  expect_equal(length(dcs_KR$var_est$model$sigma), 1)
  expect_equal(is.numeric(dcs_KR$var_est$model$sigma), TRUE)
  expect_equal(length(dcs_LP$var_est$model$sigma), 1)
  expect_equal(is.numeric(dcs_LP$var_est$model$sigma), TRUE)
  
  ar_mat = matrix(c(1, 0.1, -0.3, -0.2, 0.1, 0.1), 3, 2)
  ma_mat = matrix(c(1, 0.1, 0.1, 0.1), 2, 2)
  Y = y.norm1 + sarma.sim(101, 101, model = list(ar = ar_mat, ma = ma_mat, 
                                                 sigma = 1))$Y
  dcs_KR = dcs(Y, h = c(0.1, 0.1), dcs_options = set.options(type = "KR", 
                               var_model = "sarma_HR",
                               model_order = list(ar = c(2, 1), ma = c(1, 1))))
  dcs_LP = dcs(Y, h = c(0.1, 0.1), dcs_options = set.options(type = "LP", 
                               var_model = "sarma_HR",
                               model_order = list(ar = c(2, 1), ma = c(1, 1))))
  expect_equal(length(dcs_KR$var_est$model$sigma), 1)
  expect_equal(dim(dcs_KR$var_est$model$ar), c(3, 2))
  expect_equal(dim(dcs_KR$var_est$model$ma), c(2, 2))
  expect_equal(is.numeric(dcs_KR$var_est$model$sigma), TRUE)
  expect_equal(is.numeric(dcs_KR$var_est$model$ar), TRUE)
  expect_equal(is.numeric(dcs_KR$var_est$model$ma), TRUE)
  expect_equal(dcs_KR$var_est$stnry, TRUE)
  expect_equal(length(dcs_LP$var_est$model$sigma), 1)
  expect_equal(dim(dcs_LP$var_est$model$ar), c(3, 2))
  expect_equal(dim(dcs_LP$var_est$model$ma), c(2, 2))
  expect_equal(is.numeric(dcs_LP$var_est$model$sigma), TRUE)
  expect_equal(is.numeric(dcs_LP$var_est$model$ar), TRUE)
  expect_equal(is.numeric(dcs_LP$var_est$model$ma), TRUE)
  expect_equal(dcs_LP$var_est$stnry, TRUE)
})

context("Order Selection")

test_that("order selection works for SARMA", {
  set.seed(123)
  Y = matrix(rnorm(101^2), nrow = 101, ncol = 101)
  
  dcs_aic = dcs(Y, set.options(var_model = "sarma_sep", model_order = "aic"))
  dcs_bic = dcs(Y, set.options(var_model = "sarma_sep", model_order = "bic"))
  dcs_gpac = dcs(Y, set.options(var_model = "sarma_sep", model_order = "gpac"))
  
  expect_equal(class(dcs_aic$var_est), "sarma")
  expect_equal(class(dcs_bic$var_est), "sarma")
  expect_equal(class(dcs_gpac$var_est), "sarma")
  expect_false(all(is.na(dcs_aic$M)))
  expect_false(all(is.na(dcs_bic$M)))
  expect_false(all(is.na(dcs_gpac$M)))
})

test_that("order selection works for SFARIMA", {
  set.seed(123)
  Y = y.norm1 + sfarima.sim(101, 101, 
                  model = list(ar = 1, ma = 1, d = c(0.1, 0.2), sigma = 1))$Y
  
  dcs_aic = dcs(Y, set.options(var_model = "sfarima_RSS", model_order = "aic"))
  dcs_bic = dcs(Y, set.options(var_model = "sfarima_RSS", model_order = "bic"))
  
  expect_equal(class(dcs_aic$var_est), "sfarima")
  expect_equal(class(dcs_bic$var_est), "sfarima")
  expect_false(all(is.na(dcs_aic$M)))
  expect_false(all(is.na(dcs_bic$M)))
})