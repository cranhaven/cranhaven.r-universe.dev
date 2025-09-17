################################################################################
#                                                                              #
#                           Test for set.options()                             #
#                                                                              #
################################################################################

### Test exception handling (positives)
context("set.options() exception handling (positives)")

test_that("set.options() gets \"type\" correctly.", {
  expect_equal(set.options()$type, "LP")
  expect_equal(set.options(type = "LP")$type, "LP")
  expect_equal(set.options(type = "KR")$type, "KR")
  expect_error(set.options(type = "test"), "Unsupported values in argument")
})

test_that("set.options() gets \"kerns\" correctly.",{
  for (test in seq_along(length(dcs_list_kernels)))
  {
    expect_equal(set.options(kerns = c(dcs_list_kernels[test], 
                                       dcs_list_kernels[test]))$kerns,
                 c(dcs_list_kernels[test], dcs_list_kernels[test]))
    expect_equal(set.options(kerns = dcs_list_kernels[test])$kerns,
                 c(dcs_list_kernels[test], dcs_list_kernels[test]))
  }
})

test_that("set.options() gets \"drv\" correctly.",{
  expect_equal(set.options(drv = c(0, 0))$drv, c(0, 0))
  expect_equal(set.options(drv = c(2, 1), 
                           kerns = c("MW_422", "MW_321"))$drv, c(2, 1))
})

test_that("set.options() gets \"var_model\" correctly.",{
  expect_equal(set.options()$var_model, "iid")
  expect_equal(set.options(var_model = "sarma_HR")$var_model, "sarma_HR")
  expect_equal(set.options(var_model = "sarma_sep")$var_model, "sarma_sep")
  expect_equal(set.options(var_model = "sarma_RSS")$var_model, "sarma_RSS")
  expect_equal(set.options(var_model = "sfarima_RSS")$var_model, "sfarima_RSS")
})

test_that("set.options() converts \"var_est\" correctly.",{
  expect_equal(set.options(var_est = "iid")$var_model, "iid")
  expect_equal(set.options(var_est = "qarma")$var_model, "sarma_HR")
  expect_equal(set.options(var_est = "sarma")$var_model, "sarma_sep")
  expect_equal(set.options(var_est = "lm")$var_model, "sfarima_RSS")
  expect_warning(set.options(var_est = "qarma_gpac")$var_model, "For automatic")
})

test_that("set.options() gets \"IPI_options\" correctly.",{
  IPI_LP = list(infl_exp = c("auto", " "), infl_par = c(2, 1), 
                trim = c(0.05, 0.05), const_window = FALSE)
  IPI_KR = list(infl_exp = c(0.5, 0.5), infl_par = c(2, 1), 
                trim = c(0.05, 0.05), const_window = FALSE)
  expect_equal(set.options(type = "LP")$IPI_options, IPI_LP)
  expect_equal(set.options(type = "KR")$IPI_options, IPI_KR)
})

test_that("set.options() gets \"IPI_options\" correctly without list", {
  expect_equal(set.options(trim = c(0.1, 0.1))$IPI_options$trim, c(0.1, 0.1))
  expect_equal(set.options(infl_par = c(3, 2))$IPI_options$infl_par, c(3, 2))
  expect_equal(set.options(infl_exp = c(0.8, 0.8))$IPI_options$infl_exp,
               c(0.8, 0.8))
  expect_true(set.options(const_window = TRUE)$IPI_options$const_window)
})

test_that("set.options() gets additional options correctly.",{
  order_list = list(ar = c(1, 1), ma = c(1, 1))
  
  expect_message(set.options(model_order = order_list), "unused for iid.")
  expect_message(set.options(model_order = "bic"), "unused for iid.")
  expect_equal(set.options(var_model = "sarma_sep", 
                           model_order = order_list)$add_options$model_order,
               order_list)
  expect_equal(set.options(var_model = "sarma_sep", 
                           model_order = "bic")$add_options$model_order, "bic")
  expect_equal(set.options(var_model = "sarma_sep", 
                           model_order = "bic")$add_options$order_max,
               order_list)
  expect_equal(set.options(var_model = "sarma_sep", 
                           model_order = "bic")$add_options$order_max,
               order_list)
})


### Test exception handling (negatives)
context("set.options() exception handling (negatives)")

test_that("set.options() gives correct error messages for problems", {
  expect_error(set.options(type = "test"), "Unsupported values in argument")
  expect_error(set.options(type = c("KR", "LP")),
               "Unsupported values in argument")
})

test_that("set.options() gets additional options correctly.",{
  order_list = list(ar = c(1, 1), ma = c(1, 1))
  
  expect_message(set.options(model_order = order_list), "unused for iid.")
  expect_message(set.options(model_order = "bic"), "unused for iid.")
  
  expect_error(set.options(var_model = "sarma_sep", model_order = 5),
               "unknown model selection criterion")
  order_list_2 = order_list
  order_list_2$ar = "test"
  expect_error(set.options(var_model = "sarma_sep", model_order = order_list_2),
               "AR order of")
  order_list_3 = order_list
  order_list_3$ma = "test"
  expect_error(set.options(var_model = "sarma_sep", model_order = order_list_3),
               "MA order of")
})

test_that("errors for derivative estimation work.", {
  expect_error(set.options(drv = c(1, "a")), "Unsupported values in argument")
  expect_error(set.options(drv = c(-1, 1)), "Derivative order must be")
  expect_error(set.options(drv = c(1, 0)), "Kernel orders not matching")
})