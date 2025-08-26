context("partition gen by P")

test_that(
  "test that partition_gen_by_p() correctly generates a matrix", {
    P <- matrix(
      c(
        1, 0, 0, 0,
        1, 0, 0, 0,
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 1, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 1, 0,
        0, 0, 1, 0,
        0, 0, 0, 1,
        0, 0, 0, 1,
        0, 0, 0, 1
      ),
      byrow = TRUE, ncol = 4
    )
    
    expect_equal(partition_gen_by_p(12, 4, rep(1:4, each = 3)), P)
    
  }
)

test_that(
  "test that a matrix generateed by partition_gen_by_p() is a proper partition matrix", {
    
    P <- partition_gen_by_p(12, 4, rep(1:4, each = 3))
    
    expect_equal(colSums(P), c(3, 3, 3, 3))
    expect_equal(sum(colSums(P)), 12)
    expect_equal(all(rowSums(P) == 1), TRUE)
    expect_equal(ncol(P), 4)
    expect_equal(nrow(P), 12)
    
  }
)