################################################################################
#                                                                              #
#           Test if results of dcs() with parallelization are correct          #
#                                                                              #
################################################################################

### Tests for kernel regression
  # Not done on CRAN
  context("Parallelization")
  
  skip_on_cran()
  skip_on_bioc()
  test_that("kernel Regression for iid errors works", {
    library(DCSmooth)
    set.seed(123)
    Y = y.norm1 + rnorm(101^2)
    dcs_par = dcs(Y, set.options(type = "KR"), parallel = TRUE)
    dcs_0 = dcs(Y, set.options(type = "KR"), parallel = FALSE)
    
    expect_equal(dim(dcs_par$Y), c(101, 101))
    expect_true(is.numeric(dcs_par$Y))
    expect_equal(dim(dcs_par$M), c(101, 101))
    expect_true(is.numeric(dcs_par$M))
    expect_equal(dim(dcs_par$R), c(101, 101))
    expect_true(is.numeric(dcs_par$R))
    
    expect_true(dcs_par$var_est$stnry)
    
    expect_equal(length(dcs_par$h), 2)
    expect_true(is.numeric(dcs_par$h), 2)
    expect_true(all(dcs_par$h > 0))
    
    expect_equal(dcs_par$h, dcs_0$h)
  })

  skip_on_cran()
  skip_on_bioc()
  test_that("kernel Regression for sarma_sep errors works", {
    library(DCSmooth)
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    
    dcs_par = dcs(Y, set.options(type = "KR", var_model = "sarma_sep"),
                  parallel = TRUE)
    dcs_0 = dcs(Y, set.options(type = "KR", var_model = "sarma_sep"),
                parallel = FALSE)
    
    expect_equal(dim(dcs_par$Y), c(101, 101))
    expect_true(is.numeric(dcs_par$Y))
    expect_equal(dim(dcs_par$M), c(101, 101))
    expect_true(is.numeric(dcs_par$M))
    expect_equal(dim(dcs_par$R), c(101, 101))
    expect_true(is.numeric(dcs_par$R))
    
    expect_true(dcs_par$var_est$stnry)
    
    expect_equal(length(dcs_par$h), 2)
    expect_true(is.numeric(dcs_par$h), 2)
    expect_true(all(dcs_par$h > 0))
    
    expect_equal(dcs_par$h, dcs_0$h)
  })

### Tests for local polynomial regression
  
  skip_on_cran()
  skip_on_bioc()
  test_that("local polynomial regression for iid errors work", {
    library(DCSmooth)
    set.seed(123)
    Y = y.norm1 + rnorm(101^2)
    dcs_par = dcs(Y, set.options(type = "LP"), parallel = TRUE)
    dcs_0 = dcs(Y, set.options(type = "LP"), parallel = FALSE)
    
    expect_equal(dim(dcs_par$Y), c(101, 101))
    expect_true(is.numeric(dcs_par$Y))
    expect_equal(dim(dcs_par$M), c(101, 101))
    expect_true(is.numeric(dcs_par$M))
    expect_equal(dim(dcs_par$R), c(101, 101))
    expect_true(is.numeric(dcs_par$R))
    
    expect_true(dcs_par$var_est$stnry)
    
    expect_equal(length(dcs_par$h), 2)
    expect_true(is.numeric(dcs_par$h), 2)
    expect_true(all(dcs_par$h > 0))
    
    expect_equal(dcs_par$h, dcs_0$h)
  })
  
  skip_on_cran()
  skip_on_bioc()
  test_that("local polynomial regression for sarma_sep errors work", {
    library(DCSmooth)
    set.seed(123)
    ar = c(1, -0.3) %*% t(c(1, 0.2))
    ma = c(1, 0.5) %*% t(c(1, -0.1))
    model = list(ar = ar, ma = ma, sigma = 1)
    Y = y.norm1 + sarma.sim(101, 101, model = model)$Y
    
    dcs_par = dcs(Y, set.options(type = "LP", var_model = "sarma_sep"),
                  parallel = TRUE)
    dcs_0 = dcs(Y, set.options(type = "LP", var_model = "sarma_sep"),
                parallel = FALSE)
    
    expect_equal(dim(dcs_par$Y), c(101, 101))
    expect_true(is.numeric(dcs_par$Y))
    expect_equal(dim(dcs_par$M), c(101, 101))
    expect_true(is.numeric(dcs_par$M))
    expect_equal(dim(dcs_par$R), c(101, 101))
    expect_true(is.numeric(dcs_par$R))
    
    expect_true(dcs_par$var_est$stnry)
    
    expect_equal(length(dcs_par$h), 2)
    expect_true(is.numeric(dcs_par$h), 2)
    expect_true(all(dcs_par$h > 0))
    
    expect_equal(dcs_par$h, dcs_0$h)
  })

### Derivatives
  
  skip_on_cran()
  skip_on_bioc()
  test_that("derivatives are correctly estimated by KR", {
    set.seed(123)
    Y = y.norm1 + rnorm(101^2)
    opt_1 = set.options(type = "KR", drv = c(1, 0),
                          kerns = c("MW_321", "MW_220"))
    opt_2 = set.options(type = "KR", drv = c(1, 0),
                        kerns = c("MW_321", "MW_220"))
    dcs_1 = dcs(Y, opt_1, parallel = TRUE)
    dcs_2 = dcs(Y, opt_2, parallel = FALSE)

    
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
    
    expect_equal(dcs_1$h, dcs_2$h)
  })
  
  skip_on_cran()
  skip_on_bioc()
  test_that("derivatives are correctly estimated by LP", {
    set.seed(123)
    Y = y.norm1 + rnorm(101^2)
    opt_1 = set.options(type = "LP", drv = c(1, 0),
                        kerns = c("MW_321", "MW_220"))
    opt_2 = set.options(type = "LP", drv = c(1, 0),
                        kerns = c("MW_321", "MW_220"))
    dcs_1 = dcs(Y, opt_1, parallel = TRUE)
    dcs_2 = dcs(Y, opt_2, parallel = FALSE)
    
    
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
    
    expect_equal(dcs_1$h, dcs_2$h)
  })