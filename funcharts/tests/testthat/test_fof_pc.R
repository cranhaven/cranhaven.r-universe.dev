set.seed(0)
data("air")
air <- lapply(air, function(x) x[1:10, , drop = FALSE])
fun_covariates <- names(air)[names(air) != "NO2"]
mfdobj <- get_mfd_list(air,
                       grid = 1:24,
                       n_basis = 13,
                       lambda = 1e-2)
mfdobj_y <- mfdobj[, "NO2"]
mfdobj_x <- mfdobj[, fun_covariates]

test_that("fof_pc works with studentized residuals", {
  mod <- fof_pc(mfdobj_y, mfdobj_x, type_residuals = "studentized")
  expect_is(mod, "list")
  pred <- predict_fof_pc(mod, mfdobj_y_new = mfdobj_y, mfdobj_x_new = mfdobj_x)
  expect_is(pred, "list")
})

test_that("fof_pc with one obs", {
  expect_error(fof_pc(mfdobj_y = mfdobj_y[1], mfdobj_x = mfdobj_x[1]),
               "There is only one observation in the data set")
  expect_error(fof_pc(mfdobj_y = mfdobj_y[1:10], mfdobj_x = mfdobj_x[1:2]),
               paste0("mfdobj_y and mfdobj_x must have ",
                      "the same number of observations."))
})

test_that("fof_pc works with multiple responses", {
  expect_is(fof_pc(mfdobj_y = mfdobj_x[1:5], mfdobj_x = mfdobj_x[6:10]),
            "list")
})

test_that("fof_pc", {
  expect_error(
    predict_fof_pc("not_a_list"),
    "object must be a list produced by fof_pc."
  )
  expect_error(
    predict_fof_pc(list("not from sof_pc" = 1)),
    "object must be a list produced by fof_pc."
  )

  air1 <- lapply(air, function(x) x[1:8, , drop = FALSE])
  air2 <- lapply(air, function(x) x[9:10, , drop = FALSE])
  mfdobj_x1_list <- get_mfd_list_real_time(air1[c("CO", "temperature")],
                                           n_basis = 15,
                                           lambda = 1e-2,
                                           k_seq = c(0.5, 1))
  mfdobj_x2_list <- get_mfd_list_real_time(air2[c("CO", "temperature")],
                                           n_basis = 15,
                                           lambda = 1e-2,
                                           k_seq = c(0.5, 1))
  mfdobj_y1_list <- get_mfd_list_real_time(air1["NO2"],
                                           n_basis = 15,
                                           lambda = 1e-2,
                                           k_seq = c(0.5, 1))
  mfdobj_y2_list <- get_mfd_list_real_time(air2["NO2"],
                                           n_basis = 15,
                                           lambda = 1e-2,
                                           k_seq = c(0.5, 1))
  mod_list <- fof_pc_real_time(mfdobj_y1_list, mfdobj_x1_list)
  cclist <- regr_cc_fof_real_time(
    mod_list = mod_list,
    mfdobj_y_new_list = mfdobj_y2_list,
    mfdobj_x_new_list = mfdobj_x2_list)
  expect_is(cclist, "data.frame")
})



