set.seed(0)
mfdobj <- data_sim_mfd()
pca <- pca_mfd(mfdobj)
test_that("pca_mfd works well", {
  mfdobj <- data_sim_mfd()
  expect_is(pca, "pca_mfd")
  # Error with one observation
  expect_error(pca_mfd(mfdobj[1]),
               "There is only one observation in the data set")
  # Works with only one variable.
  expect_is(pca_mfd(mfdobj[, 1]), "pca_mfd")
})

test_that("plot pca works", {
  p <- plot_pca_mfd(pca)
  expect_is(p, "ggplot")
})


x <- seq(1, 10, length = 25)
y11 <- cos(x)
y21 <- cos(2 * x)
y12 <- sin(x)
y22 <- sin(2 * x)
data_list <- list(y1 = rbind(y11, y21),
                  y2 = rbind(y12, y22))
test_that("control_charts_pca_real_time works", {
  mfdobj <- get_mfd_list_real_time(data_list,
                                   k_seq = c(0.5, 1),
                                   lambda = 1e-2)
  pca_rt <- pca_mfd_real_time(mfdobj)
  ccl <- control_charts_pca_mfd_real_time(pca_rt,
                                          mfdobj_x_test = mfdobj)
  expect_is(ccl, "data.frame")

  p <- plot_control_charts_real_time(ccl, 1)
  expect_is(p, "ggplot")
})

