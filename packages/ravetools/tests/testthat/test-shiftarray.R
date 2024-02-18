test_that("Shift array with different types", {
  x0 <- as.integer(1:10)
  x <- matrix(x0, nrow = 2, byrow = TRUE)
  z <- shift_array(x, 2, 1, c(1,2))
  y <- NA * x
  y[1,1:4] <- x[1,2:5]
  y[2,1:3] <- x[2,3:5]
  expect_identical(y, z)

  x0 <- as.double(1:10)
  x <- matrix(x0, nrow = 2, byrow = TRUE)
  z <- shift_array(x, 2, 1, c(-2,1))
  y <- NA * x
  y[1,3:5] <- x[1,1:3]
  y[2,1:4] <- x[2,2:5]
  expect_identical(y, z)

  x0 <- as.complex(1:10)
  x <- matrix(x0, nrow = 2, byrow = TRUE)
  z <- shift_array(x, 2, 1, c(-2,1))
  y <- NA * x
  y[1,3:5] <- x[1,1:3]
  y[2,1:4] <- x[2,2:5]
  expect_identical(y, z)

  x0 <- sample(c(TRUE, FALSE), 10, replace = TRUE)
  x <- matrix(x0, nrow = 2, byrow = TRUE)
  z <- shift_array(x, 2, 1, c(1,2))
  y <- x
  y[] <- NA
  y[1,1:4] <- x[1,2:5]
  y[2,1:3] <- x[2,3:5]
  expect_identical(y, z)

})


test_that("Shift array with different arrays", {
  dim <- c(4,15,10)
  x <- array(rnorm(prod(dim)), dim)
  shift_amount <- c(2,10,-3,NA)
  z <- shift_array(x, along_margin = 3, unit_margin = 1, shift_amount = shift_amount)

  y <- x * NA
  for(ii in 1:3){
    idx <- shift_amount[ii] + 1:10
    sel <- idx >= 1 & idx <= 10
    y[ii,, sel] <- x[ii, , idx[sel]]
  }
  y[4,,] <- NA
  expect_identical(y, z)


  shift_amount <- sample(c(-5:5, NA, NA), size = 15, replace = TRUE)
  z <- shift_array(x, along_margin = 1, unit_margin = 2, shift_amount = shift_amount)

  y <- x * NA
  for(ii in 1:15){
    idx <- shift_amount[ii] + 1:4
    if(!is.na(shift_amount[ii])){
      sel <- idx >= 1 & idx <= 4
      y[sel,ii,] <- x[idx[sel], ii,]
    }
  }
  expect_identical(y, z)
})

