context("distance_join")

sepal_lengths <- tibble(Sepal.Length = c(5, 6, 7), Sepal.Width = 1:3, Type = 1:3)

test_that("distance_inner_join works for Euclidean distance", {
  ret <- iris %>%
    distance_inner_join(sepal_lengths, max_dist = .25,
                        by = c("Sepal.Length", "Sepal.Width"),
                        distance_col = "distance") %>%
    mutate(calculated_distance = sqrt((Sepal.Length.x - Sepal.Length.y) ^ 2 +
                                        (Sepal.Width.x - Sepal.Width.y) ^ 2))

  expect_true(nrow(ret) > 0)
  expect_true(all(ret$calculated_distance < .25))
  expect_true(all(ret$calculated_distance == ret$distance))

  # without distance column
  ret2 <- iris %>%
    distance_inner_join(sepal_lengths, max_dist = .25,
                        by = c("Sepal.Length", "Sepal.Width"))
  expect_false("distance" %in% colnames(ret2))
  expect_equal(ret$Sepal.Length.x, ret2$Sepal.Length.x)
  expect_equal(ret$Type, ret2$Type)
})

test_that("distance_inner_join works for Manhattan distance", {
  ret2 <- iris %>%
    distance_inner_join(sepal_lengths, max_dist = .25,
                        by = c("Sepal.Length", "Sepal.Width"),
                        method = "manhattan",
                        distance_col = "distance") %>%
    mutate(calculated_distance = abs(Sepal.Length.x - Sepal.Length.y) +
             abs(Sepal.Width.x - Sepal.Width.y))

  expect_true(nrow(ret2) > 0)
  expect_true(all(ret2$calculated_distance < .25))
  expect_true(all(ret2$calculated_distance == ret2$distance))
})

test_that("distance_ functions besides inner work", {
  ret2 <- iris %>%
    distance_left_join(sepal_lengths, max_dist = .25,
                        by = c("Sepal.Length", "Sepal.Width"),
                       distance_col = "distance") %>%
    mutate(calculated_distance = sqrt((Sepal.Length.x - Sepal.Length.y) ^ 2 +
                                        (Sepal.Width.x - Sepal.Width.y) ^ 2))

  expect_equal(nrow(iris), nrow(ret2))
  expect_true(all(ret2$calculated_distance < .25, na.rm = TRUE))
  expect_true(all(ret2$calculated_distance == ret2$distance, na.rm = TRUE))

  ret3 <- iris %>%
    distance_right_join(sepal_lengths, max_dist = .25,
                       by = c("Sepal.Length", "Sepal.Width"),
                       distance_col = "distance") %>%
    mutate(calculated_distance = sqrt((Sepal.Length.x - Sepal.Length.y) ^ 2 +
                                        (Sepal.Width.x - Sepal.Width.y) ^ 2))

  expect_equal(sum(!is.na(ret2$calculated_distance)) + 1, nrow(ret3))
  expect_true(all(ret3$calculated_distance < .25, na.rm = TRUE))
  expect_equal(ret3$Type[1], 1)

  ret4 <- iris %>%
    distance_full_join(sepal_lengths, max_dist = .25,
                        by = c("Sepal.Length", "Sepal.Width"),
                        distance_col = "distance") %>%
    mutate(calculated_distance = sqrt((Sepal.Length.x - Sepal.Length.y) ^ 2 +
                                        (Sepal.Width.x - Sepal.Width.y) ^ 2))

  expect_equal(nrow(iris) + 1, nrow(ret4))
  expect_true(all(ret4$calculated_distance < .25, na.rm = TRUE))

  ret5 <- iris %>%
    distance_semi_join(sepal_lengths, max_dist = .25,
                       by = c("Sepal.Length", "Sepal.Width"),
                       distance_col = "distance")

  expect_equal(nrow(ret5), sum(!is.na(ret2$calculated_distance)))
  expect_equal(colnames(ret5), colnames(iris))

  ret6 <- iris %>%
    distance_anti_join(sepal_lengths, max_dist = .25,
                       by = c("Sepal.Length", "Sepal.Width"),
                       distance_col = "distance")

  expect_equal(nrow(ret6), sum(is.na(ret2$calculated_distance)))
  expect_equal(colnames(ret6), colnames(iris))
})

test_that("distance_inner_join works when there's only one distance column", {
  a <- tibble(x = 1:10)
  b <- tibble(y = 3:12)

  result <- distance_inner_join(a, b, by = c("x" = "y"), max_dist = 1.5, distance_col = "distance")
  expect_equal(nrow(result), 24)
  expect_true(all(result$distance < 1.5))
})

test_that("distance joins where there are no overlapping rows still get a distance column", {
  a <- tibble(x = 1:10, y = 1:10)
  b <- tibble(x = 21:30, y = 21:30)

  result <- distance_left_join(a, b, by = c("x", "y"), max_dist = 1, distance_col = "distance")

  expect_equal(colnames(result), c("x.x", "y.x", "x.y", "y.y", "distance"))
  expect_equal(nrow(result), 10)
  expect_true(all(is.na(result$y.y)))
  expect_true(all(is.na(result$distance)))

  result <- distance_inner_join(a, b, by = c("x", "y"), max_dist = 1, distance_col = "distance")

  expect_equal(colnames(result), c("x.x", "y.x", "x.y", "y.y", "distance"))
  expect_equal(nrow(result), 0)

  # Don't add it for semi or anti join
  result <- distance_semi_join(a, b, by = c("x", "y"), max_dist = 1, distance_col = "distance")
  expect_equal(colnames(result), c("x", "y"))
  expect_equal(nrow(result), 0)

  result <- distance_anti_join(a, b, by = c("x", "y"), max_dist = 1, distance_col = "distance")
  expect_equal(colnames(result), c("x", "y"))
  expect_equal(a, result)
})


