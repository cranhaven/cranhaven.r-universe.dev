testthat::test_that("SpearmanHeuristic: heuristic function works", {

  heuristic <- SpearmanHeuristic$new()

  col1 <- c(1, 0, 2)
  col2 <- c(3, 2, 3)
  column.names <- c("ex", "Class")
  testthat::expect_type(heuristic$heuristic(col1 = col1,
                                            col2 = col2,
                                            column.names = column.names),
                        "double")
})

testthat::test_that("SpearmanHeuristic: heuristic function checks parameter type", {

  heuristic <- SpearmanHeuristic$new()

  col1 <- c("1.2", "2.2")
  col2 <- c("1.1", "2.2", "3.4")
  column.names <- c("ex", "Class")

  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[SpearmanHeuristic][WARNING] Columns must be 'numeric' type. Returning NA",
                           fixed = TRUE)

  testthat::expect_equal(heuristic$heuristic(col1 = col1,
                                             col2 = col2,
                                             column.names = column.names),
                         NA)

  col1 <- c(1.2, 2.2)
  col2 <- c(1.1, 2.2, 3.4)
  column.names <- c("ex", "Class")
  testthat::expect_equal(heuristic$heuristic(col1 = col1,
                                             col2 = col2,
                                             column.names = column.names),
                         NA)

  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[SpearmanHeuristic][ERROR] Error occurred calculating spearman heuristic: 'Error in cor.test.default(col1, col2, method = \"spearman\", exact = FALSE): 'x' and 'y' must have the same length
' . Returning NA",
                           fixed = TRUE)
})
