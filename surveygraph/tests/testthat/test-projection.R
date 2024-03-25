test_that("correct edge list", {
  S <- data.frame(group = c("0", "0", "1", "1"), item_1 = c(1, 2, 5, 4), item_2 = c(1, 3, 2, 2))

  fn <- function(m){
    make_projection(
      S,
      "agent",
      threshold_method = "raw_similarity",
      method_value = m
    )
  }

  expect_equal(
    fn(-1.00),
    data.frame(
      u = c(1, 1, 1, 2, 2, 3),
      v = c(2, 3, 4, 3, 4, 4),
      weight = c(-0.25, -0.5, -0.25, -0.25, 0, 0.75)
    )
  )

  expect_equal(
    fn(-0.50),
    data.frame(
      u = c(1, 1, 2, 2, 3),
      v = c(2, 4, 3, 4, 4),
      weight = c(-0.25, -0.25, -0.25, 0, 0.75)
    )
  )

  expect_equal(
    fn(-0.25),
    data.frame(
      u = c(2, 3),
      v = c(4, 4),
      weight = c(0, 0.75)
    )
  )

  expect_equal(
    fn(0.00),
    data.frame(
      u = c(3),
      v = c(4),
      weight = c(0.75)
    )
  )

  expect_equal(
    fn(0.75),
    data.frame(
      u = as.integer(c()),
      v = as.integer(c()),
      weight = as.numeric(c())
    )
  )
})
