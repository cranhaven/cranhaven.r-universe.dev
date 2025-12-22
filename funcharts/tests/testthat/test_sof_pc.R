set.seed(0)
data("air")
air <- lapply(air, function(x) x[1:10, , drop = FALSE])
fun_covariates <- names(air)[names(air) != "NO2"]
mfdobj_x <- get_mfd_list(air[fun_covariates], lambda = 1e-2)
y <- rowMeans(air$NO2)

test_that("sof_pc", {
  expect_error(sof_pc(y = y[1], mfdobj_x = mfdobj_x[1]),
               "There is only one observation in the data set")
  expect_error(sof_pc(y = y[1:10], mfdobj_x = mfdobj_x[1:2]),
               paste0("y and mfdobj_x must have ",
                      "the same number of observations."))
  expect_error(sof_pc(y, mfdobj_x, selection = 1),
               "selection must be one of 'variance', 'PRESS', 'gcv'.")
  expect_error(sof_pc(y, mfdobj_x, components = "aaa"),
               "components must be a vector of positive integers.")
  expect_error(sof_pc(y, mfdobj_x, components = - 10),
               "components must be a vector of positive integers.")


  mod <- sof_pc(y[1:5], mfdobj_x[1:5])

  p <- plot_bootstrap_sof_pc(mod, nboot = 1)
  expect_is(p, "ggplot")
  expect_equal(names(mod), c("mod",
                             "pca",
                             "beta_fd",
                             "residuals",
                             "components",
                             "selection",
                             "single_min_variance_explained",
                             "tot_variance_explained",
                             "gcv",
                             "PRESS"))
  expect_error(
    predict_sof_pc(mod, y_new = y[1], mfdobj_x_new = mfdobj_x[, 1]),
    "mfdobj_x_new must have the same number of variables as training data.")
  expect_error(
    predict_sof_pc(mod, alpha = 0),
    "alpha must be strictly between 0 and 1.")
  expect_error(
    predict_sof_pc(mod, alpha = 1),
    "alpha must be strictly between 0 and 1.")
  expect_is(
    predict_sof_pc(mod, alpha = 0.5),
    "data.frame")
  expect_error(
    predict_sof_pc(mod, alpha = - 1),
    "alpha must be strictly between 0 and 1.")
  expect_error(
    predict_sof_pc(mod, alpha = 2),
    "alpha must be strictly between 0 and 1.")
  expect_error(
    predict_sof_pc("not_a_list"),
    "object must be a list produced by sof_pc."
  )
  expect_error(
    predict_sof_pc(list("not from sof_pc" = 1)),
    "object must be a list produced by sof_pc."
  )


})



