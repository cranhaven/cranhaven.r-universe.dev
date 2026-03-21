test_that("no error has occurred", {
  nt <- c("A", "G", "C", "T")
  seq_len <- 15
  seq_n <- c(15, 19)
  # Create data
  c1 <- replicate(seq_n[1],
                  paste(sample(nt, seq_len, replace = TRUE), collapse = ''))
  c2 <- replicate(seq_n[2],
                  paste(sample(nt, seq_len, replace = TRUE), collapse = ''))
  # Execute function
  expect_identical(cr_source(c1, c2), NULL)
})
