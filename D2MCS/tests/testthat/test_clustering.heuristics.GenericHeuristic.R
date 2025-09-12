testthat::test_that("GenericHeuristic: heuristic function works", {

  heuristic <- GenericHeuristic$new()

  col1 <- c(1, 0)
  col2 <- c(3, 2)
  column.names <- c("ex", "Class")
  testthat::expect_error(heuristic$heuristic(col1 = col1,
                                             col2 = col2,
                                             column.names = column.names),
                         "[GenericHeuristic][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})
