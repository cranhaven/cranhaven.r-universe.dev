test_that("cpp ns functions gives the same as the R version", {
  skip_if_not_installed("splines")
  skip_if_not_installed("Matrix")
  require(splines)
  require(Matrix)

  b_ks <- seq(1, 10, length.out = 4)
  b_func_R <- function(x)
    ns(x, knots = b_ks[-c(1L, length(b_ks))],
       Boundary.knots = b_ks[ c(1L, length(b_ks))],
       intercept = TRUE)

  b_func <- get_ns_spline(b_ks, do_log = FALSE)

  xs <- seq(0, 20, length.out = 1000)
  check_spline <- function(v1, v2)
    expect_equal(rankMatrix(cbind(v1, v2)), NCOL(v1),
                 check.attributes = FALSE)
  check_spline(b_func_R(xs), b_func(xs))

  b_ks <- log(b_ks)
  b_func_R <- function(x)
    ns(log(x), knots = b_ks[-c(1L, length(b_ks))],
       Boundary.knots = b_ks[ c(1L, length(b_ks))],
       intercept = TRUE)

  b_func <- get_ns_spline(b_ks, do_log = TRUE)

  xs <- seq(1e-4, 20, length.out = 1000)
  check_spline(b_func_R(xs), b_func(xs))
})
