################################################################################
#                                                                              #
#                      Test if results of dcs() are correct                    #
#                                                                              #
################################################################################

### Tests for kernel regression
  context("Kernel Regression")
  
# 1
  test_that("kernel Regression for iid errors work", {
    set.seed(123)
    Y = y.norm1 + rnorm(101^2)
    dcs_iid = dcs(Y, set.options(type = "KR"))
    
    expect_equal(dim(dcs_iid$Y), c(101, 101))
    expect_true(is.numeric(dcs_iid$Y))
    expect_equal(dim(dcs_iid$M), c(101, 101))
    expect_true(is.numeric(dcs_iid$M))
    expect_equal(dim(dcs_iid$R), c(101, 101))
    expect_true(is.numeric(dcs_iid$R))
    
    expect_true(dcs_iid$var_est$stnry)
    
    expect_equal(length(dcs_iid$h), 2)
    expect_true(is.numeric(dcs_iid$h), 2)
    expect_true(all(dcs_iid$h > 0))
  })

# 2
  test_that("kernel Regression for sarma_HR errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    dcs_sarma = dcs(Y, set.options(type = "KR", var_model = "sarma_HR"))
    
    expect_equal(dim(dcs_sarma$Y), c(101, 101))
    expect_true(is.numeric(dcs_sarma$Y))
    expect_equal(dim(dcs_sarma$M), c(101, 101))
    expect_true(is.numeric(dcs_sarma$M))
    expect_equal(dim(dcs_sarma$R), c(101, 101))
    expect_true(is.numeric(dcs_sarma$R))
    
    expect_true(dcs_sarma$var_est$stnry)
    expect_equal(class(dcs_sarma$var_est), "sarma")
    expect_true(is.numeric(unlist(dcs_sarma$var_est$model)))
    
    expect_equal(length(dcs_sarma$h), 2)
    expect_true(is.numeric(dcs_sarma$h), 2)
    expect_true(all(dcs_sarma$h > 0))
  })
  
# 3
  test_that("kernel Regression for sarma_sep errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    dcs_sarma = dcs(Y, set.options(type = "KR", var_model = "sarma_sep"))
    
    expect_equal(dim(dcs_sarma$Y), c(101, 101))
    expect_true(is.numeric(dcs_sarma$Y))
    expect_equal(dim(dcs_sarma$M), c(101, 101))
    expect_true(is.numeric(dcs_sarma$M))
    expect_equal(dim(dcs_sarma$R), c(101, 101))
    expect_true(is.numeric(dcs_sarma$R))
    
    expect_true(dcs_sarma$var_est$stnry)
    expect_equal(class(dcs_sarma$var_est), "sarma")
    expect_true(is.numeric(unlist(dcs_sarma$var_est$model)))
    
    expect_equal(length(dcs_sarma$h), 2)
    expect_true(is.numeric(dcs_sarma$h), 2)
    expect_true(all(dcs_sarma$h > 0))
  })

# 4
  test_that("kernel Regression for sarma_RSS errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    dcs_sarma = dcs(Y, set.options(type = "KR", var_model = "sarma_RSS"))
    
    expect_equal(dim(dcs_sarma$Y), c(101, 101))
    expect_true(is.numeric(dcs_sarma$Y))
    expect_equal(dim(dcs_sarma$M), c(101, 101))
    expect_true(is.numeric(dcs_sarma$M))
    expect_equal(dim(dcs_sarma$R), c(101, 101))
    expect_true(is.numeric(dcs_sarma$R))
    
    expect_true(dcs_sarma$var_est$stnry)
    expect_equal(class(dcs_sarma$var_est), "sarma")
    expect_true(is.numeric(unlist(dcs_sarma$var_est$model)))
    
    expect_equal(length(dcs_sarma$h), 2)
    expect_true(is.numeric(dcs_sarma$h), 2)
    expect_true(all(dcs_sarma$h > 0))
  })
  
# 5
  test_that("kernel Regression for sfarima_RSS errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, d = c(0.1, 0.2), sigma = 1)
    
    Y = y.norm1 + sfarima.sim(101, 101, model = model)$Y
    dcs_sfarima = dcs(Y, set.options(type = "KR", var_model = "sfarima_RSS"))
    
    expect_equal(dim(dcs_sfarima$Y), c(101, 101))
    expect_true(is.numeric(dcs_sfarima$Y))
    expect_equal(dim(dcs_sfarima$M), c(101, 101))
    expect_true(is.numeric(dcs_sfarima$M))
    expect_equal(dim(dcs_sfarima$R), c(101, 101))
    expect_true(is.numeric(dcs_sfarima$R))
    
    expect_true(dcs_sfarima$var_est$stnry)
    expect_equal(class(dcs_sfarima$var_est), "sfarima")
    expect_true(is.numeric(unlist(dcs_sfarima$var_est$model)))
    
    expect_equal(length(dcs_sfarima$h), 2)
    expect_true(is.numeric(dcs_sfarima$h), 2)
    expect_true(all(dcs_sfarima$h > 0))
  })

### Tests for local polynomial regression
  context("Local Polynomial Regression")
  
  # 1
  test_that("local polynomial regression for iid errors work", {
    set.seed(123)
    Y = y.norm1 + rnorm(101^2)
    dcs_iid = dcs(Y, set.options(type = "LP"))
    
    expect_equal(dim(dcs_iid$Y), c(101, 101))
    expect_true(is.numeric(dcs_iid$Y))
    expect_equal(dim(dcs_iid$M), c(101, 101))
    expect_true(is.numeric(dcs_iid$M))
    expect_equal(dim(dcs_iid$R), c(101, 101))
    expect_true(is.numeric(dcs_iid$R))
    
    expect_true(dcs_iid$var_est$stnry)
    
    expect_equal(length(dcs_iid$h), 2)
    expect_true(is.numeric(dcs_iid$h), 2)
    expect_true(all(dcs_iid$h > 0))
  })
  
  # 2
  test_that("local polynomial regression for sarma_HR errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    dcs_sarma = dcs(Y, set.options(type = "LP", var_model = "sarma_HR"))
    
    expect_equal(dim(dcs_sarma$Y), c(101, 101))
    expect_true(is.numeric(dcs_sarma$Y))
    expect_equal(dim(dcs_sarma$M), c(101, 101))
    expect_true(is.numeric(dcs_sarma$M))
    expect_equal(dim(dcs_sarma$R), c(101, 101))
    expect_true(is.numeric(dcs_sarma$R))
    
    expect_true(dcs_sarma$var_est$stnry)
    expect_equal(class(dcs_sarma$var_est), "sarma")
    expect_true(is.numeric(unlist(dcs_sarma$var_est$model)))
    
    expect_equal(length(dcs_sarma$h), 2)
    expect_true(is.numeric(dcs_sarma$h), 2)
    expect_true(all(dcs_sarma$h > 0))
  })
  
  # 3
  test_that("local polynomial regression for sarma_sep errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    dcs_sarma = dcs(Y, set.options(type = "LP", var_model = "sarma_sep"))
    
    expect_equal(dim(dcs_sarma$Y), c(101, 101))
    expect_true(is.numeric(dcs_sarma$Y))
    expect_equal(dim(dcs_sarma$M), c(101, 101))
    expect_true(is.numeric(dcs_sarma$M))
    expect_equal(dim(dcs_sarma$R), c(101, 101))
    expect_true(is.numeric(dcs_sarma$R))
    
    expect_true(dcs_sarma$var_est$stnry)
    expect_equal(class(dcs_sarma$var_est), "sarma")
    expect_true(is.numeric(unlist(dcs_sarma$var_est$model)))
    
    expect_equal(length(dcs_sarma$h), 2)
    expect_true(is.numeric(dcs_sarma$h), 2)
    expect_true(all(dcs_sarma$h > 0))
  })
  
  # 4
  test_that("local polynomial regression for sarma_RSS errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    dcs_sarma = dcs(Y, set.options(type = "LP", var_model = "sarma_RSS"))
    
    expect_equal(dim(dcs_sarma$Y), c(101, 101))
    expect_true(is.numeric(dcs_sarma$Y))
    expect_equal(dim(dcs_sarma$M), c(101, 101))
    expect_true(is.numeric(dcs_sarma$M))
    expect_equal(dim(dcs_sarma$R), c(101, 101))
    expect_true(is.numeric(dcs_sarma$R))
    
    expect_true(dcs_sarma$var_est$stnry)
    expect_equal(class(dcs_sarma$var_est), "sarma")
    expect_true(is.numeric(unlist(dcs_sarma$var_est$model)))
    
    expect_equal(length(dcs_sarma$h), 2)
    expect_true(is.numeric(dcs_sarma$h), 2)
    expect_true(all(dcs_sarma$h > 0))
  })
  
  # 5
  test_that("local polynomial regression for sfarima_RSS errors work", {
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, d = c(0.1, 0.2), sigma = 1)
    
    Y = y.norm1 + sfarima.sim(101, 101, model = model)$Y
    dcs_sfarima = dcs(Y, set.options(type = "LP", var_model = "sfarima_RSS"))
    
    expect_equal(dim(dcs_sfarima$Y), c(101, 101))
    expect_true(is.numeric(dcs_sfarima$Y))
    expect_equal(dim(dcs_sfarima$M), c(101, 101))
    expect_true(is.numeric(dcs_sfarima$M))
    expect_equal(dim(dcs_sfarima$R), c(101, 101))
    expect_true(is.numeric(dcs_sfarima$R))
    
    expect_true(dcs_sfarima$var_est$stnry)
    expect_equal(class(dcs_sfarima$var_est), "sfarima")
    expect_true(is.numeric(unlist(dcs_sfarima$var_est$model)))
    
    expect_equal(length(dcs_sfarima$h), 2)
    expect_true(is.numeric(dcs_sfarima$h), 2)
    expect_true(all(dcs_sfarima$h > 0))
  })

context("derivative estimation")

test_that("derivatives are correctly estimated by KR", {
  set.seed(123)
  Y = y.norm1 + rnorm(101^2)
  opt_1 = set.options(type = "KR", drv = c(1, 0),
                        kerns = c("MW_321", "MW_220"))
  opt_2 = set.options(type = "KR", drv = c(0, 2),
                      kerns = c("MW_220", "MW_422"))
  opt_3 = set.options(type = "KR", drv = c(1, 1),
                      kerns = c("MW_321", "MW_321"))
  dcs_1 = dcs(Y, opt_1)
  dcs_2 = dcs(Y, opt_2)
  dcs_3 = dcs(Y, opt_3)
  
  expect_equal(dcs_1$dcs_options$drv, c(1, 0))
  expect_equal(dim(dcs_1$Y), c(101, 101))
  expect_true(is.numeric(dcs_1$Y))
  expect_equal(dim(dcs_1$M), c(101, 101))
  expect_true(is.numeric(dcs_1$M))
  expect_equal(dim(dcs_1$R), c(101, 101))
  expect_true(is.numeric(dcs_1$R))
  expect_equal(length(dcs_1$h), 2)
  expect_true(is.numeric(dcs_1$h), 2)
  expect_true(all(dcs_1$h > 0))
  
  expect_equal(dcs_2$dcs_options$drv, c(0, 2))
  expect_equal(dim(dcs_2$Y), c(101, 101))
  expect_true(is.numeric(dcs_2$Y))
  expect_equal(dim(dcs_2$M), c(101, 101))
  expect_true(is.numeric(dcs_2$M))
  expect_equal(dim(dcs_2$R), c(101, 101))
  expect_true(is.numeric(dcs_2$R))
  expect_equal(length(dcs_2$h), 2)
  expect_true(is.numeric(dcs_2$h), 2)
  expect_true(all(dcs_2$h > 0))
  
  expect_equal(dcs_3$dcs_options$drv, c(1, 1))
  expect_equal(dim(dcs_3$Y), c(101, 101))
  expect_true(is.numeric(dcs_3$Y))
  expect_equal(dim(dcs_3$M), c(101, 101))
  expect_true(is.numeric(dcs_3$M))
  expect_equal(dim(dcs_3$R), c(101, 101))
  expect_true(is.numeric(dcs_3$R))
  expect_equal(length(dcs_3$h), 2)
  expect_true(is.numeric(dcs_3$h), 2)
  expect_true(all(dcs_3$h > 0))
})

test_that("derivatives are correctly estimated by LP", {
  set.seed(123)
  Y = y.norm1 + rnorm(101^2)
  opt_1 = set.options(type = "LP", drv = c(1, 0),
                      kerns = c("MW_321", "MW_220"))
  opt_2 = set.options(type = "LP", drv = c(0, 2),
                      kerns = c("MW_220", "MW_422"))
  opt_3 = set.options(type = "LP", drv = c(1, 1),
                      kerns = c("MW_321", "MW_321"))
  dcs_1 = dcs(Y, opt_1)
  dcs_2 = dcs(Y, opt_2)
  dcs_3 = dcs(Y, opt_3)
  
  expect_equal(dcs_1$dcs_options$drv, c(1, 0))
  expect_equal(dim(dcs_1$Y), c(101, 101))
  expect_true(is.numeric(dcs_1$Y))
  expect_equal(dim(dcs_1$M), c(101, 101))
  expect_true(is.numeric(dcs_1$M))
  expect_equal(dim(dcs_1$R), c(101, 101))
  expect_true(is.numeric(dcs_1$R))
  expect_equal(length(dcs_1$h), 2)
  expect_true(is.numeric(dcs_1$h), 2)
  expect_true(all(dcs_1$h > 0))
  
  expect_equal(dcs_2$dcs_options$drv, c(0, 2))
  expect_equal(dim(dcs_2$Y), c(101, 101))
  expect_true(is.numeric(dcs_2$Y))
  expect_equal(dim(dcs_2$M), c(101, 101))
  expect_true(is.numeric(dcs_2$M))
  expect_equal(dim(dcs_2$R), c(101, 101))
  expect_true(is.numeric(dcs_2$R))
  expect_equal(length(dcs_2$h), 2)
  expect_true(is.numeric(dcs_2$h), 2)
  expect_true(all(dcs_2$h > 0))
  
  expect_equal(dcs_3$dcs_options$drv, c(1, 1))
  expect_equal(dim(dcs_3$Y), c(101, 101))
  expect_true(is.numeric(dcs_3$Y))
  expect_equal(dim(dcs_3$M), c(101, 101))
  expect_true(is.numeric(dcs_3$M))
  expect_equal(dim(dcs_3$R), c(101, 101))
  expect_true(is.numeric(dcs_3$R))
  expect_equal(length(dcs_3$h), 2)
  expect_true(is.numeric(dcs_3$h), 2)
  expect_true(all(dcs_3$h > 0))
})

test_that("derivatives are correctly for different errors", {
  set.seed(123)
  ar = c(1, -0.3) %*% t(c(1, 0.2))
  ma = c(1, 0.5) %*% t(c(1, -0.1))
  model = list(ar = ar, ma = ma, d = c(0.1, 0.2), sigma = 1)
  Y = y.norm1 + sfarima.sim(101, 101, model = model)$Y
  
  opt_1 = set.options(type = "LP", drv = c(2, 0),
                      kerns = c("MW_422", "MW_220"), var_model = "sarma_sep")
  opt_2 = set.options(type = "LP", drv = c(2, 0),
                      kerns = c("MW_422", "MW_220"), var_model = "sfarima_RSS")#, infl_par = c(1, 1), infl_exp = c(0.5, 0.5))
  
  dcs_1 = suppressWarnings(dcs(Y, opt_1))
  dcs_2 = dcs(Y, opt_2)
  
  expect_equal(dcs_1$dcs_options$drv, c(2, 0))
  expect_equal(dim(dcs_1$Y), c(101, 101))
  expect_true(is.numeric(dcs_1$Y))
  expect_equal(dim(dcs_1$M), c(101, 101))
  expect_true(is.numeric(dcs_1$M))
  expect_equal(dim(dcs_1$R), c(101, 101))
  expect_true(is.numeric(dcs_1$R))
  expect_equal(length(dcs_1$h), 2)
  expect_true(is.numeric(dcs_1$h), 2)
  expect_true(all(dcs_1$h > 0))
  
  expect_equal(dcs_2$dcs_options$drv, c(2, 0))
  expect_equal(dim(dcs_2$Y), c(101, 101))
  expect_true(is.numeric(dcs_2$Y))
  expect_equal(dim(dcs_2$M), c(101, 101))
  expect_true(is.numeric(dcs_2$M))
  expect_equal(dim(dcs_2$R), c(101, 101))
  expect_true(is.numeric(dcs_2$R))
  expect_equal(length(dcs_2$h), 2)
  expect_true(is.numeric(dcs_2$h), 2)
  expect_true(all(dcs_2$h > 0))
})
    
context("given bandwidths")

test_that("given bandwidths are used", {
  set.seed(123)
  Y = y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
  
  dcs_KR = dcs(Y, set.options(type = "KR"), h = c(0.1, 0.15))
  dcs_LP = dcs(Y, set.options(type = "LP"), h = c(0.1, 0.15))
  
  expect_equal(dcs_KR$h, c(0.1, 0.15))
  expect_equal(dcs_LP$h, c(0.1, 0.15))
  
  expect_equal(dim(dcs_KR$Y), c(101, 101))
  expect_true(is.numeric(dcs_KR$Y))
  expect_equal(dim(dcs_KR$M), c(101, 101))
  expect_true(is.numeric(dcs_KR$M))
  expect_equal(dim(dcs_KR$R), c(101, 101))
  expect_true(is.numeric(dcs_KR$R))
  
  expect_equal(dim(dcs_LP$Y), c(101, 101))
  expect_true(is.numeric(dcs_LP$Y))
  expect_equal(dim(dcs_LP$M), c(101, 101))
  expect_true(is.numeric(dcs_LP$M))
  expect_equal(dim(dcs_LP$R), c(101, 101))
  expect_true(is.numeric(dcs_LP$R))
})

test_that("(too) small bandwidths are handled correctly", {
  set.seed(123)
  Y = y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
  dcs_KR_small = dcs(Y, h = c(0.001, 0.001), 
                     dcs_options = set.options(type = "KR"))
  expect_equal(dcs_KR_small$h, c(0.001, 0.001))
  expect_equal(is.numeric(dcs_KR_small$M), TRUE)
  expect_equal(dim(dcs_KR_small$M), c(101, 101))
  expect_warning(dcs(Y, h = c(0.001, 0.001), 
                     dcs_options = set.options(type = "LP")),
                 "Bandwidth h too small for \"LP\", changed to smallest working value.")
})

test_that("(too) large bandwidths are handled correctly", {
  set.seed(123)
  Y = y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
  dcs_LP_large = dcs(Y, h = c(0.6, 0.6), 
                     dcs_options = set.options(type = "LP"))
  expect_equal(dcs_LP_large$h, c(0.6, 0.6))
  expect_equal(is.numeric(dcs_LP_large$M), TRUE)
  expect_equal(dim(dcs_LP_large$M), c(101, 101))
  expect_error(dcs(Y, h = c(0.6, 0.6), 
                   dcs_options = set.options(type = "KR")),
               "Bandwidth h must be < 0.45 for kernel regression")
})