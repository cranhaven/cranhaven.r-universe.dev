testthat::test_that("FisherTestHeuristic: heuristic function works", {

  heuristic <- FisherTestHeuristic$new()

  col1 <- c(1, 0, 1)
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_type(heuristic$heuristic(col1 = col1,
                                            col2 = col2,
                                            column.names = column.names),
                        "double")
})

testthat::test_that("FisherTestHeuristic: heuristic function checks parameter type", {

  heuristic <- FisherTestHeuristic$new()

  col1 <- c(2, 0, 1)
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[FisherTestHeuristic][WARNING] Column 'ex' is not binary. Returning NA",
                           fixed = TRUE)

  col1 <- c(1, 0, 1)
  col2 <- c(2, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[FisherTestHeuristic][WARNING] Column 'Class' is not binary. Returning NA",
                           fixed = TRUE)

  col1 <- c("1", "2")
  col2 <- c("1", "2", "2")
  column.names <- c("ex", "Class")
  testthat::expect_equal(heuristic$heuristic(col1 = col1,
                                             col2 = col2,
                                             column.names = column.names),
                         NA)

  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[FisherTestHeuristic][ERROR] Error occurred calculating fisher.test heuristic: 'Error in stats::fisher.test(col1.factor, col2.factor): 'x' and 'y' must have the same length
' . Returning NA",
                           fixed = TRUE)
})
