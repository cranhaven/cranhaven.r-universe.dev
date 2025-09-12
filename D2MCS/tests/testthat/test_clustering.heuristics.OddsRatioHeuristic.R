testthat::test_that("OddsRatioHeuristic: heuristic function works", {

  heuristic <- OddsRatioHeuristic$new()

  col1 <- c(1, 0, 1)
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_type(heuristic$heuristic(col1 = col1,
                                            col2 = col2,
                                            column.names = column.names),
                        "double")
})

testthat::test_that("OddsRatioHeuristic: heuristic function checks parameter type", {

  heuristic <- OddsRatioHeuristic$new()

  col1 <- c(2, 0, 1)
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[OddsRatioHeuristic][WARNING] Columns must be binary. Returning NA",
                           fixed = TRUE)

  col1 <- c(1, 0, 1)
  col2 <- c(2, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[OddsRatioHeuristic][WARNING] Columns must be binary. Returning NA",
                           fixed = TRUE)

  col1 <- c("1", "0", "1")
  col2 <- c("1", "0", "1", "1")
  column.names <- c("ex", "Class")
  testthat::expect_equal(heuristic$heuristic(col1 = col1,
                                             col2 = col2,
                                             column.names = column.names),
                         NA)
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[OddsRatioHeuristic][ERROR] Error occurred calculating odds.ratio heuristic: 'Error in table(col1, col2): all arguments must have the same length
' . Returning NA",
                           fixed = TRUE)
})
