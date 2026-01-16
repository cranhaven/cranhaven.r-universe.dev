test_that("Input has the correct format", {

  eM <- "# The input data format has not been respected. Make sure your input file contains 3 columns named: ID, Time and Reads."
  Column_1 <- c("testing")
  Column_2 <- c(1.675)
  mock_df <- data.frame(Column_1, Column_2)
  expect_error(reshapeData(mock_df), eM, fixed = TRUE)

  eM <- "# The values in 'Reads' column must be numeric."
  ID <- c(1,2,3,4,5)
  Time <- c("d1","d1","d1","d1", "d1")
  Reads <- c("20","20","20","20","20")
  mock_df <- data.frame(ID, Time, Reads)
  expect_error(reshapeData(mock_df), eM, fixed = TRUE)

  eM <- "# The values in 'Time' column must be numeric."
  ID <- c(1,2,3,4,5)
  Time <- c("d1","d1","d1","d1", "d1")
  Reads <- c(20,20,0,20,20)
  mock_df <- data.frame(ID, Time, Reads)
  expect_error(reshapeData(mock_df), eM, fixed = TRUE)

})
