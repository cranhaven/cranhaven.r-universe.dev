data("air")
air <- lapply(air, function(x) x[1:220, , drop = FALSE])
fun_covariates <- c("CO", "temperature")
mfdobj_x <- get_mfd_list(air[fun_covariates],
                         n_basis = 15,
                         lambda = 1e-2)
mfdobj_y <- get_mfd_list(air["NO2"],
                         n_basis = 15,
                         lambda = 1e-2)
y_scalar <- rowMeans(air$NO2)
y1_scalar <- y_scalar[1:200]
y2_scalar <- y_scalar[201:220]
mfdobj_y1 <- mfdobj_y[1:200]
mfdobj_y2 <- mfdobj_y[201:220]
mfdobj_x1 <- mfdobj_x[1:200]
mfdobj_x2 <- mfdobj_x[201:220]
mod_sof <- sof_pc(y1_scalar, mfdobj_x1)
mod_fof <- fof_pc(mfdobj_y1, mfdobj_x1)

test_that("regr_sof_pc works", {
  expect_error(regr_cc_sof(object = 0,
                           y_new = y2_scalar,
                           mfdobj_x_new = mfdobj_x2),
               "object must be a list produced by sof_pc.")
  expect_error(regr_cc_sof(object = list(123),
                           y_new = y2_scalar,
                           mfdobj_x_new = mfdobj_x2),
               "object must be a list produced by sof_pc.")
  expect_is(regr_cc_sof(object = mod_sof,
                           y_new = y2_scalar,
                           mfdobj_x_new = mfdobj_x2),
               "data.frame")
})

test_that("control_charts functions works", {
  pca <- pca_mfd(mfdobj_x)
  cc <- control_charts_pca(pca, newdata = mfdobj_x)
  expect_is(cc, "data.frame")
  expect_is(get_ooc(cc), "data.frame")
  expect_is(which_ooc(cc), "list")
  p <- cont_plot(cc, 1)
  expect_is(p, "ggplot")
  p <- plot_mon(cc, mfdobj_x, mfdobj_x[1])
  expect_is(p, "ggplot")
  cc_cv <- control_charts_pca(pca, newdata = mfdobj_x,
                              limits = "cv",
                              nfold = 2)
  expect_is(cc_cv, "data.frame")

  cc <- regr_cc_sof(mod_sof,
                    y_new = y2_scalar,
                    mfdobj_x_new = mfdobj_x2)
  expect_is(cc, "data.frame")


  cc <- regr_cc_fof(mod_fof,
                    mfdobj_y_new = mfdobj_y2,
                    mfdobj_x_new = mfdobj_x2)
  # expect_is(cc, "data.frame")
  p <- plot_control_charts(cc)
  expect_is(p, "ggplot")




})

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
y1 <- rowMeans(air1$NO2)
y2 <- rowMeans(air2$NO2)
mod_list <- sof_pc_real_time(y1, mfdobj_x1_list)

test_that("control_charts_sof gives warning", {

  expect_warning(cc <- control_charts_sof_pc(mod_sof,
                                             y_test = y2_scalar,
                                             mfdobj_x_test = mfdobj_x2))


  expect_warning(cclist <- control_charts_sof_pc_real_time(
    mod_list = mod_list,
    y_test = y2,
    mfdobj_x_test = mfdobj_x2_list))
})



test_that("control chart real time works", {


  expect_no_error(cclist <- regr_cc_sof_real_time(mod_list = mod_list,
                                        y_new = y2,
                                        mfdobj_x_new_list = mfdobj_x2_list,
                                        include_covariates = TRUE))
  p <- plot_control_charts_real_time(cclist, 1)
  expect_is(p, "ggplot")
})
