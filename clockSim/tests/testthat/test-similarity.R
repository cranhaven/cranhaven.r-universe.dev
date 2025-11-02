# compute_normalize
test_that("compute_normalize works with range method", {
  mat <- cbind(time = 1:3, state1 = c(1, 2, 3), state2 = c(4, 6, 8))
  result <- compute_normalize(mat, "range")
  
  # Check normalized values
  expect_equal(result[,"state1"], c(0, 0.5, 1))
  expect_equal(result[,"state2"], c(0, 0.5, 1))
  
  # Check attributes
  expect_equal(attr(result, "ref_stats")$mins, c(state1=1, state2=4))
})

test_that("compute_normalize works with sd method", {
  mat <- cbind(time = 1:3, state1 = c(1, 2, 3))
  result <- compute_normalize(mat, "sd")
  
  # Check normalized values (mean=2, sd=1)
  expect_equal(result[,"state1"], c(-1, 0, 1))
})

test_that("compute_normalize handles constant columns", {
  mat <- cbind(time = 1:3, state1 = c(1, 1, 1))
  result <- compute_normalize(mat, "range")
  expect_equal(result[,"state1"], c(0, 0, 0)) # Shouldn't be NaN
})

#.compute_validatePair
test_that(".compute_validatePair catches mismatched times", {
  mat1 <- cbind(time = 1:3, state1 = 1:3)
  mat2 <- cbind(time = 2:4, state1 = 1:3)
  expect_error(.compute_validatePair(mat1, mat2), "Time columns must be identical")
})

#compute_rmse
test_that("compute_rmse calculates correct errors", {
  mat1 <- cbind(time = 1:3, state1 = c(1, 2, 3))
  mat2 <- cbind(time = 1:3, state1 = c(1, 2, 4)) # Only last point differs
  
  # RMSE = sqrt(mean(c(0, 0, 1))) = sqrt(1/3)
  expect_equal(compute_rmse(mat1, mat2), c(state1=sqrt(1/3)))
})

test_that("compute_rmse with normalization", {
  mat1 <- cbind(time = 1:3, state1 = c(0, 10, 20)) # range=20
  mat2 <- cbind(time = 1:3, state1 = c(1, 11, 21)) # diff=1
  
  # NRMSE = 1/20 = 0.05
  expect_equal(compute_rmse(mat1, mat2, "range"), c(state1=0.05))
})

test_that("compute_rmse handles multiple states without normalization", {
  mat1 <- cbind(time = 1:3, 
                state1 = c(1, 2, 3), 
                state2 = c(4, 5, 6))
  mat2 <- cbind(time = 1:3, 
                state1 = c(1, 2, 4),  # diff = (0,0,1)
                state2 = c(4, 7, 6))   # diff = (0,2,0)
  
  # RMSE for state1 = sqrt(mean(c(0,0,1)^2)) = sqrt(1/3)
  # RMSE for state2 = sqrt(mean(c(0,2,0)^2)) = sqrt(4/3)
  expect_equal(compute_rmse(mat1, mat2), 
               c(state1=sqrt(1/3), state2=sqrt(4/3)))
})

test_that("compute_rmse handles multiple states with range normalization", {
  mat1 <- cbind(time = 1:3, 
                state1 = c(0, 10, 20),  # range=20
                state2 = c(0, 5, 10))   # range=10
  mat2 <- cbind(time = 1:3, 
                state1 = c(1, 11, 21),  # diff=1
                state2 = c(1, 6, 11))   # diff=1
  
  # NRMSE for state1 = 1/20 = 0.05
  # NRMSE for state2 = 1/10 = 0.1
  expect_equal(compute_rmse(mat1, mat2, "range"), 
               c(state1=0.05, state2=0.1))
})

#compute_cosine
test_that("compute_cosine handles perfect alignment", {
  mat1 <- cbind(time = 1:3, state1 = 1:3)
  mat2 <- cbind(time = 1:3, state1 = 2:4) # Same direction
  
  expect_equal(compute_cosine(mat1, mat2), c(1, 1, 1))
})

test_that("compute_cosine handles orthogonal vectors", {
  mat1 <- cbind(time = 1:2, state1 = c(1, 0), state2 = c(0, 1))
  mat2 <- cbind(time = 1:2, state1 = c(0, 1), state2 = c(1, 0))
  
  expect_equal(compute_cosine(mat1, mat2), c(0, 0))
})
