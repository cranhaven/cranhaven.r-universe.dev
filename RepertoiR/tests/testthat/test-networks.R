test_that("no error has occurred", {
  # Create data
  data <- matrix(rexp(1/2, n=1000), ncol = 4)
  aa <- c("G", "A", "V", "L", "I", "P", "F", "Y", "W", "S",
          "T", "N", "Q", "C", "M", "D", "E", "H", "K", "R")
  cons <- sample(aa, 8)
  # Generate similar sequences
  aavec <- c()
  while(length(aavec) < nrow(data)) {
    aaseq <- cons
    index <- sample(length(aaseq), sample(length(aaseq)/2, 1))
    aaseq[index] <- sample(aa, length(index), replace = TRUE)
    aaseq <- paste0(aaseq, collapse = "")
    aavec <- unique(append(aavec, aaseq))
  }
  rownames(data) <- aavec
  colnames(data) <- LETTERS[seq(ncol(data))]
  # Execute function with default arguments
  expect_identical(network(data), NULL)
})
