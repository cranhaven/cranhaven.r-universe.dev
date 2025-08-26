context("partition matrix to vector")

test_that(
  "part_matrix_to_vector returns the index of the nonzero column for each row", {
    
    test_matrix <- matrix(
      c(
        1, 0, 0,
        1, 0, 0,
        0, 0, 1,
        0, 1, 0,
        0, 0, 1,
        0, 1, 0
      ),
      ncol = 3, byrow = TRUE
    )
    
    expect_equal(part_matrix_to_vector(test_matrix), c(1, 1, 3, 2, 3, 2))
    
  })