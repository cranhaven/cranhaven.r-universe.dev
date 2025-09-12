testthat::test_that("PearsonHeuristic: heuristic function works", {

  heuristic <- PearsonHeuristic$new()

  col1 <- c(1, 0, 2)
  col2 <- c(3, 2, 3)
  column.names <- c("ex", "Class")
  testthat::expect_type(heuristic$heuristic(col1 = col1,
                                            col2 = col2,
                                            column.names = column.names),
                        "double")
})

testthat::test_that("PearsonHeuristic: heuristic function checks parameter type", {

  heuristic <- PearsonHeuristic$new()

  col1 <- "1"
  col2 <- "2"
  column.names <- c("ex", "Class")
  testthat::expect_equal(heuristic$heuristic(col1 = col1,
                                             col2 = col2,
                                             column.names = column.names),
                         NA)
})
