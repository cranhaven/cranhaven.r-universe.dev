context("properties of generated partition matrix")

test_that(
  "partition_gen() generates a proper partition matrix", {
    
    P <- partition_gen(20, 4)
    expect_equal(sum(colSums(P)), 20)
    expect_equal(sum(rowSums(P)), 20)
    expect_equal(all(rowSums(P) == 1), TRUE)
    
  }
)

test_that(
  "partition_gen() generates a partition matrix of the correct dimensions", {
    
    P <- partition_gen(20, 4)
    expect_equal(nrow(P), 20)
    expect_equal(ncol(P), 4)
    
  }
)

