context("test-plot_discrete_vars")

test_that("dataframe is received", {
  expect_error(plot_discrete_vars(list(a = 1:3, b = TRUE)))
})

test_that("warning if no variable is selected", {
  expect_warning(plot_discrete_vars(cars))
})

test_that("matrix is received",  {
  expect_warning(plot_discrete_vars(state.x77))
})

test_that("successful grid", {
  expect_is(plot_discrete_vars(mtcars), "gtable")
})

test_that("successful grid sorted by frequency", {
  expect_is(plot_discrete_vars(mtcars, sort_by_frequency = TRUE),
            "gtable")
})

# plot_discrete_vars(mtcars)
# plot_discrete_vars(mtcars, sort_by_frequency = TRUE)
# plot_discrete_vars(iris)
#
# plot_discrete_vars(cars)
# plot_discrete_vars(state.x77)
