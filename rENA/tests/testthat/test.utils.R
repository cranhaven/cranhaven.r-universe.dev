suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test util methods");

library(magrittr)

data(RS.data)
units <- c("UserName", "Condition")
conversation <- c("ActivityNumber", "GroupName")
codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
            "Client.and.Consultant.Requests", "Design.Reasoning",
            "Collaboration")

set_end <- RS.data %>%
  ena(
    units = units,
    conversation = conversation,
    codes = codes,
    window.size.back = 4
  )

test_that("Find different col types", {
  meta_cols <- find_meta_cols(set_end$line.weights)
  meta_col_names <- names(meta_cols)[meta_cols]
  testthat::expect_equal(meta_col_names,  c("ENA_UNIT", "UserName", "Condition"))

  code_cols <- find_code_cols(set_end$line.weights)
  code_col_names <- names(code_cols)[code_cols]

  expected_names <- svector_to_ut(codes)
  testthat::expect_true(all(code_col_names %in% expected_names))

  dim_cols <- find_dimension_cols(set_end$rotation.matrix)
  testthat::expect_equal(length(which(dim_cols)), 15)
})

test_that("Custom subsetting.", {
  weight_groups <- unique(`$.line.weights`(set_end$line.weights, "Condition"));
  testthat::expect_equal(weight_groups, c("FirstGame", "SecondGame"))

  first_group_points = set_end$points$Condition$FirstGame
  testthat::expect_true(is(first_group_points, "ena.points"))
  testthat::expect_equal(nrow(first_group_points), 26)

  meta_groups <- `.DollarNames.ena.metadata`(set_end$line.weights$Condition)
  testthat::expect_equal(meta_groups, c("FirstGame", "SecondGame"))
})

test_that("Test summary output", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  sink(tmp)
  summary(set_end)

  testthat::expect_true(
    grepl(x = readChar(tmp, nchars = 1024), pattern = "pearson")
  )
  unlink(tmp)
})

test_that("Test print output", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  sink(tmp)
  print(set_end)

  testthat::expect_true(
    grepl(x = readLines(tmp)[1], pattern = "\\$connection\\.counts")
  )
  unlink(tmp)
})

# test_that("Test print plot output", {
#   tmp <- tempfile()
#   on.exit(unlink(tmp), add = TRUE)
#
#   sink(tmp)
#   suppressWarnings(print(plot(set_end)))
#
#   testthat::expect_true(
#     grepl(x = readLines(tmp)[1], pattern = "[[1]]")
#   )
#   unlink(tmp)
# })

test_that("Test means rotation", {
  set_rotated <- means_rotate(set_end)

  testthat::expect_equal(
    round(mean(as.matrix(set_rotated$points$Condition$FirstGame)[,2]), 10),
    0
  )
  testthat::expect_equal(
    round(mean(as.matrix(set_rotated$points$Condition$SecondGame)[,2]), 10),
    0
  )
  testthat::expect_gt(
    abs(mean(as.matrix(set_rotated$points$Condition$FirstGame)[,1])), 0)
  testthat::expect_gt(
    abs(mean(as.matrix(set_rotated$points$Condition$SecondGame)[,1])), 0)


  set_rotated_on <- means_rotate(set_end, on = list(Condition = c("FirstGame", "SecondGame")))
  testthat::expect_equal(set_rotated_on$points, set_rotated$points)

  testthat::expect_error(means_rotate(set_end, on = c("FirstGame", "SecondGame")))
})

test_that("Test projections", {
  set_groups <- RS.data %>%
    ena(
      units = c("Condition", "GroupName"),
      conversation = conversation,
      codes = codes,
      window.size.back = 4
    )

  set_projected <- project_in(set_groups, set_end)

  testthat::expect_equal(set_projected$rotation$nodes, set_end$rotation$nodes)
  testthat::expect_false(all(set_projected$rotation$nodes == set_groups$rotation$nodes))
  testthat::expect_false(all(set_projected$points == set_groups$points))
  testthat::expect_error(all(set_projected$points == set_end$points))

  testthat::expect_error(project_in(set_groups))

  set_projected_matrix <- project_in(set_groups, set_end$rotation)
  testthat::expect_equal(set_projected, set_projected_matrix)
})

test_that("Test trajectory", {
  set_traj <- as_trajectory(set_end)

  testthat::expect_false(
    nrow(set_end$points) == nrow(set_traj$points)
  )
  testthat::expect_equal(set_traj$model$model.type, "AccumulatedTrajectory")

  set_traj_2 <- as_trajectory(set_end, codes = codes[1:(length(codes) - 1)])
  testthat::expect_error(project_in(set_traj_2, set_end))

  set_traj_projected <- project_in(set_traj, set_end$rotation)

  testthat::expect_equal(nrow(set_traj_projected$points), nrow(set_traj$points))
  testthat::expect_equal(set_traj_projected$rotation$nodes, set_end$rotation$nodes)
  testthat::expect_false(all(set_traj$rotation$nodes == set_traj_projected$rotation$nodes))
  testthat::expect_true(all(set_end$rotation$nodes == set_traj_projected$rotation$nodes))
})

test_that("Test as using factors", {
  code_vec <- as.ena.code(factor(sample(LETTERS, 100, replace = T)))
  testthat::expect_is(code_vec, "ena.code")

  code_vec <- as.ena.codes(factor(sample(LETTERS, 100, replace = T)))
  testthat::expect_is(code_vec, "ena.codes")

  code_vec <- as.ena.dimension(factor(sample(LETTERS, 100, replace = T)))
  testthat::expect_is(code_vec, "ena.dimension")

  code_vec <- as.ena.co.occurrence(factor(sample(LETTERS, 100, replace = T)))
  testthat::expect_is(code_vec, "ena.co.occurrence")
})

test_that("Verify tri indices", {
  two <- rENA:::triIndices(3)
  testthat::expect_equal(two[1,], c(0, 0, 1))
  testthat::expect_equal(two[2,], c(1, 2, 2))

  one <- rENA:::triIndices(3, row = 0)
  testthat::expect_equal(one[1,], c(0, 0, 1))

  one <- rENA:::triIndices(3, row = 1)
  testthat::expect_equal(one[1,], c(1, 2, 2))
})
