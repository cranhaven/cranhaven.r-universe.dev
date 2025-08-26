context("sse calculation for partition matrices")

test_that(
  "the correct sse is calculated for a given biclustering", {
    
    sse <- function(x) {
      
      mu <- mean(x, na.rm = TRUE)
      sum((x - mu) ^ 2, na.rm = TRUE)
      
    }
    
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
    Q <- matrix(
      c(
        1, 0,
        1, 0,
        1, 0,
        0, 1,
        0, 1,
        0, 1
      ),
      byrow = TRUE, ncol = 2
    )
    
    hand_sse <- sse(synthetic[1:3, 1:3]) + 
      sse(synthetic[1:3, 4:6]) + 
      sse(synthetic[1:3, 7:9]) + 
      sse(synthetic[1:3, 10:12]) + 
      sse(synthetic[4:6, 1:3]) +  
      sse(synthetic[4:6, 4:6]) + 
      sse(synthetic[4:6, 7:9]) + 
      sse(synthetic[4:6, 10:12])
    
    expect_equal(cluster_iteration_sum_sse(synthetic, P, Q), hand_sse)
    
  }
)