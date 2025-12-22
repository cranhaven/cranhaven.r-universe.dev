set.seed(0)
x <- seq(1, 10, length = 25)
y11 <- cos(x)
y21 <- cos(2 * x)
y12 <- sin(x)
y22 <- sin(2 * x)
df <- data.frame(id = factor(rep(1:2, each = length(x))),
                 x = rep(x, times = 2),
                 y1 = c(y11, y21),
                 y2 = c(y12, y22))

data_list <- list(y1 = rbind(y11, y21),
                  y2 = rbind(y12, y22))

data_array <- aperm(simplify2array(data_list), c(2, 1, 3))


test_that("domain must be a vector of 2 numbers", {
  expect_error(get_mfd_df(dt = df,
                          domain = c(1, 10, 20),
                          arg = "x",
                          id = "id",
                          variables = c("y1", "y2")),
               "domain must be a vector with two numbers.")
})

test_that("check parallel", {
  expect_no_error(get_mfd_df(dt = df,
                          domain = c(1, 10),
                          arg = "x",
                          id = "id",
                          variables = c("y1", "y2"),
                          ncores = 2))
})

test_that("get_mfd functions work", {
  expect_is(get_mfd_df(dt = df,
                       domain = c(0, 1),
                       arg = "x",
                       id = "id",
                       variables = c("y1", "y2"),
                       lambda = 1e-2), "mfd")
  expect_is(get_mfd_df_real_time(dt = df,
                       domain = c(1, 10),
                       arg = "x",
                       id = "id",
                       variables = c("y1", "y2"),
                       lambda = 1e-2,
                       k_seq = seq(0.5, 1)), "list")

  expect_is(get_mfd_list(data_list = data_list), "mfd")
  expect_is(get_mfd_list_real_time(data_list = data_list,
                                   lambda = 1e-2,
                                   k_seq = seq(0.5, 1)), "list")

  expect_is(get_mfd_array(data_array  = data_array), "mfd")
  expect_is(get_mfd_array_real_time(data_array = data_array,
                                   lambda = 1e-2,
                                   k_seq = seq(0.5, 1)), "list")


})

test_that("plot mfd functions work", {
  mfdobj <- data_sim_mfd()
  p <- plot_mfd(mfdobj)
  expect_is(p, "ggplot")

  mfdobj_y <- data_sim_mfd(nbasis = 15)
  mfdobj_x <- data_sim_mfd(nbasis = 15)
  mod <- fof_pc(mfdobj_y, mfdobj_x)
  p <- plot_bifd(mod$beta_fd)
  expect_is(p, "ggplot")
  p <- plot_bifd(mod$beta_fd, type_plot = "contour")
  expect_no_error(plot_bifd(mod$beta_fd, type_plot = "perspective"))

  library(ggplot2)
  data("air")
  xlist <- list(NO2 = air$NO2[1:2, ])
  mfdobj <- get_mfd_list(xlist)
  p <- plot_mfd(mfdobj = mfdobj, type_mfd = "raw")
  expect_is(p, "ggplot")
  p <- lines_mfd(p, mfdobj = mfdobj)
  expect_is(p, "ggplot")
})

test_that("cbind rbind mfd work", {
  mfdobj1 <- data_sim_mfd()
  mfdobj2 <- data_sim_mfd()
  expect_is(cbind_mfd(mfdobj1, mfdobj2), "mfd")
  expect_is(rbind_mfd(mfdobj1, mfdobj2), "mfd")
})

test_that("get_mfd_fd correctly converts fd objects", {
  bs <- fda::create.bspline.basis(nbasis = 10)
  fdobj <- fda::fd(basisobj = bs)
  expect_equal(get_mfd_fd(fdobj),
               mfd(coef = array(0, dim = c(10, 1, 1)),
                   basisobj = fdobj$basis,
                   fdnames = fdobj$fdnames))
  expect_equal({
    mfdobj <- data_sim_mfd()
    fdobj <- fda::fd(mfdobj$coefs, mfdobj$basis, mfdobj$fdnames)
    get_mfd_fd(fdobj[1, 1:2])
  },
  mfdobj[1, 1:2])
  expect_equal({
    mfdobj <- data_sim_mfd()
    fdobj <- fda::fd(mfdobj$coefs, mfdobj$basis, mfdobj$fdnames)
    get_mfd_fd(fdobj[1:2, 1])
  },
  mfdobj[1:2, 1])
})


test_that("tensor_product_mfd works with multivariate objects", {
  mfdobj1 <- data_sim_mfd(nobs = 1, nvar = 3)
  mfdobj2 <- data_sim_mfd(nobs = 1, nvar = 2)
  expect_is(tensor_product_mfd(mfdobj1), "bifd")
  expect_is(tensor_product_mfd(mfdobj1, mfdobj2), "bifd")
  expect_equal({
    tp <- tensor_product_mfd(mfdobj1, mfdobj2)
    dim(tp$coef)
  }, c(5, 5, 1, 3 * 2))
})

test_that("scale_mfd returns error with one single obs", {
  mfdobj1 <- data_sim_mfd()
  mfdobj2 <- data_sim_mfd(nobs = 1)
  expect_error({
    scale_mfd(mfdobj2)
  },
  "There is only one observation in the data set")
  expect_error({
    pca_mfd(mfdobj2)
  },
  "There is only one observation in the data set")
  expect_s3_class({
    mfdobj1_scaled <- scale_mfd(mfdobj1)
    mfdobj2_scaled <- scale_mfd(mfdobj2,
                                center = attr(mfdobj1_scaled, "scaled:center"),
                                scale = attr(mfdobj1_scaled, "scaled:scale"))
    mfdobj2_scaled
  },
  "mfd")
})

test_that("scale_mfd requires center to be fd object", {
  mfdobj1 <- data_sim_mfd()
  mfdobj2 <- data_sim_mfd()

  # Normal scaling
  mfdobj1_scaled <- scale_mfd(mfdobj1)
  expect_s3_class(mfdobj1_scaled, "mfd")
  expect_s3_class(attr(mfdobj1_scaled, "scaled:center"), "fd")
  expect_is(attr(mfdobj1_scaled, "scaled:scale"), "fd")

  # Only scale
  mfdobj1_scaled <- scale_mfd(mfdobj1, center = FALSE)
  expect_s3_class(mfdobj1_scaled, "mfd")
  expect_null(attr(mfdobj1_scaled, "scaled:center"))
  expect_is(attr(mfdobj1_scaled, "scaled:scale"), "fd")

  # Only center
  mfdobj1_scaled <- scale_mfd(mfdobj1, scale = FALSE)
  expect_s3_class(mfdobj1_scaled, "mfd")
  expect_s3_class(attr(mfdobj1_scaled, "scaled:center"), "fd")
  expect_null(attr(mfdobj1_scaled, "scaled:scale"))

  # Provide center, scale TRUE
  mfdobj2_scaled <- scale_mfd(mfdobj2,
                              center = attr(mfdobj1_scaled, "scaled:center"))
  expect_s3_class(mfdobj2_scaled, "mfd")
  expect_s3_class(attr(mfdobj2_scaled, "scaled:center"), "fd")
  expect_is(attr(mfdobj2_scaled, "scaled:scale"), "fd")

  # Provide center, scale FALSE
  mfdobj1_scaled <- scale_mfd(mfdobj1)
  mfdobj2_scaled <- scale_mfd(mfdobj2,
                              center = attr(mfdobj1_scaled, "scaled:center"),
                              scale = FALSE)
  expect_s3_class(mfdobj2_scaled, "mfd")
  expect_s3_class(attr(mfdobj2_scaled, "scaled:center"), "fd")
  expect_null(attr(mfdobj2_scaled, "scaled:scale"))

  # Provide scale, center TRUE
  mfdobj2_scaled <- scale_mfd(mfdobj2,
                              scale = attr(mfdobj1_scaled, "scaled:scale"))
  expect_s3_class(mfdobj2_scaled, "mfd")
  expect_s3_class(attr(mfdobj2_scaled, "scaled:center"), "fd")
  expect_is(attr(mfdobj2_scaled, "scaled:scale"), "fd")

  # Provide scale, center FALSE
  mfdobj2_scaled <- scale_mfd(mfdobj2,
                              scale = attr(mfdobj1_scaled, "scaled:scale"),
                              center = FALSE)
  expect_s3_class(mfdobj2_scaled, "mfd")
  expect_null(attr(mfdobj2_scaled, "scaled:center"))
  expect_is(attr(mfdobj2_scaled, "scaled:scale"), "fd")

})

test_that("inprod functions work", {
  mfdobj1 <- data_sim_mfd()
  mfdobj2 <- data_sim_mfd()
  expect_is(inprod_mfd_diag(mfdobj1, mfdobj2), "matrix")
  mfdobj3 <- data_sim_mfd(nbasis = 10)
  expect_is(inprod_mfd_diag(mfdobj1, mfdobj3), "matrix")
  expect_is(inprod_mfd(mfdobj1, mfdobj3), "array")
})


test_that("norm_mfd works", {
  mfdobj1 <- data_sim_mfd()
  expect_no_error(norm.mfd(mfdobj1))
})


test_that("cov_mfd and cor_mfd functions work", {
  mfdobj1 <- data_sim_mfd()
  mfdobj2 <- data_sim_mfd()
  expect_is(cov_mfd(mfdobj1, mfdobj2), "bifd")
  expect_is(cor_mfd(mfdobj1, mfdobj2), "bifd")
})

test_that("mean_mfd works", {
  mfdobj <- data_sim_mfd()
  expect_is(mean(mfdobj), "mfd")
})
