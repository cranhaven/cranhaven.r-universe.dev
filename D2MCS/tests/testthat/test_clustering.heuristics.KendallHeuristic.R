testthat::test_that("KendallHeuristic: heuristic function works", {

  heuristic <- KendallHeuristic$new()

  col1 <- c(2, 0, 1)
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_type(suppressWarnings(heuristic$heuristic(col1 = col1,
                                                             col2 = col2,
                                                             column.names = column.names)),
                        "double")
})

testthat::test_that("KendallHeuristic: heuristic function checks parameter type", {

  heuristic <- KendallHeuristic$new()

  col1 <- c(1, 0, 1)
  col2 <- c(1, 2, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[KendallHeuristic][WARNING] Columns must be real. Returning NA",
                           fixed = TRUE)

  col1 <- c(1, 2, 1)
  col2 <- c(2, 0, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[KendallHeuristic][WARNING] Columns must be real. Returning NA",
                           fixed = TRUE)

  col1 <- c("2", "0", "1")
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_equal(heuristic$heuristic(col1 = col1,
                                             col2 = col2,
                                             column.names = column.names),
                         NA)

  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[KendallHeuristic][ERROR] Error occurred calculating kendall heuristic: 'Error in cor.test.default(col1, col2, method = \"kendall\"): 'x' must be a numeric vector
' . Returning NA",
                           fixed = TRUE)

})
