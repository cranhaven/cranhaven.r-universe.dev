context("difference_inner_join")

test_that("difference_inner_join works on a df with one match", {
  sepal_lengths1 <- tibble(Sepal.Length = c(5, 6, 7),
                           Type = 1:3)
  j1 <- iris %>%
    difference_inner_join(sepal_lengths1, max_dist = .25, distance_col = "difference")

  dists <- apply(abs(outer(iris$Sepal.Length, 5:7, "-")), 1, min)
  expect_equal(nrow(j1), sum(dists <= .25))
  expect_equal(abs(j1$Sepal.Length.x - j1$Sepal.Length.y), j1$difference)
})

test_that("difference_inner_join works on a df with two matches", {
  sepal_lengths2 <- tibble(Sepal.Length = c(5, 6, 7),
                           Sepal.Width = 1:3,
                           Type = 1:3)

  j2 <- iris %>%
    difference_inner_join(sepal_lengths2, max_dist = .5, distance_col = "difference")

  expect_is(j2$Sepal.Length.difference, "numeric")
  expect_is(j2$Sepal.Width.difference, "numeric")
  expect_gt(min(j2$Sepal.Length.x - j2$Sepal.Length.y), -.55)
  expect_lt(max(j2$Sepal.Length.x - j2$Sepal.Length.y), .55)

  expect_true(all(j2$Type[j2$Sepal.Length.x < 5.5] == 1))

  expect_equal(j2$Sepal.Length.difference,
               abs(j2$Sepal.Length.x - j2$Sepal.Length.y))
  expect_equal(j2$Sepal.Width.difference,
               abs(j2$Sepal.Width.x - j2$Sepal.Width.y))
})

test_that("difference_ joins where there are no overlapping rows still get a distance column", {
  a <- tibble(x = 1:10)
  b <- tibble(y = 21:30)

  result <- difference_left_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")

  expect_equal(colnames(result), c("x", "y", "distance"))
  expect_equal(nrow(result), 10)
  expect_true(all(is.na(result$y)))
  expect_true(all(is.na(result$distance)))

  result <- difference_inner_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")

  expect_equal(colnames(result), c("x", "y", "distance"))
  expect_equal(nrow(result), 0)

  # Don't add it for semi or anti join
  result <- difference_semi_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")
  expect_equal(colnames(result), "x")
  expect_equal(nrow(result), 0)

  result <- difference_anti_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")
  expect_equal(colnames(result), "x")
  expect_equal(a, result)
})

test_that("semi and anti joins support one-column data.frames", {
  a <- data.frame(x = 1:10)
  b <- data.frame(y = 3:12)

  result <- difference_semi_join(a, b, by = c(x = "y"), max_dist = 1.5)
  expect_is(result, "data.frame")
  expect_equal(colnames(result), "x")
  expect_equal(nrow(result), 9)

  result <- difference_anti_join(a, b, by = c(x = "y"), max_dist = 1.5)
  expect_is(result, "data.frame")
  expect_equal(colnames(result), "x")
  expect_equal(nrow(result), 1)
})
