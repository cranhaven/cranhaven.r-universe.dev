# ==============================================================================
# lap_animate() and the optional htmlwidgets dependency
# ==============================================================================
# htmlwidgets is in Suggests: it is needed only to wrap a trace into a widget.
# Trace generation and every solver run without it.
# ==============================================================================

test_that("lap_animate() returns an htmlwidget", {
  skip_if_not_installed("htmlwidgets")

  cost <- matrix(c(4, 2, 5,
                   3, 3, 6,
                   7, 5, 4), nrow = 3, byrow = TRUE)

  w <- lap_animate(cost, method = "hungarian")

  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$meta$cost_matrix, unname(as.matrix(cost)))
})

test_that("trace generation needs no htmlwidgets", {
  cost <- matrix(c(4, 2, 5,
                   3, 3, 6,
                   7, 5, 4), nrow = 3, byrow = TRUE)

  trace_fn <- couplr:::get_trace_fn("hungarian")
  trace <- trace_fn(cost, maximize = FALSE)

  expect_true(is.list(trace))
  expect_true(length(trace$frames) > 0)
})

test_that("lap_animate() validates its cost matrix", {
  skip_if_not_installed("htmlwidgets")

  expect_error(lap_animate(matrix(numeric(0), nrow = 0, ncol = 0)),
               "at least one row and one column")
  expect_error(lap_animate(matrix(NaN, 2, 2)), "NaN")
})
