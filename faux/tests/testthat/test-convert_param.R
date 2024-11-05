context("test-convert_param")

# errors ----
test_that("errors", {
  cells_w <- c("W1_X1", "W2_X1", "W1_X2", "W2_X2")
  cells_b <- c("B1_C1", "B2_C1", "B1_C2", "B2_C2")
  
  df <- data.frame(
    A = 1:4, B = 5:8, 
    C = 9:12, D = 13:16, 
    row.names = cells_b
  )
  
  expect_error(convert_param(df, cells_w, cells_b), 
               "The this parameter data table is misspecified.")
  expect_error(convert_param(df, cells_w, cells_b, "err"), 
               "The err data table is misspecified.")
  
  # wrong names
  param <- list(
    A = 1:4, B = 5:8, 
    C = 9:12, D = 13:16
  )
  expect_error(convert_param(param, cells_w, cells_b), 
               "The names in the list this parameter are not correct.")
  
  # wrong vector length
  param <- list("B1_C1" = 1:2, 
                "B2_C1" = 3:4, 
                "B1_C2" = 5:6, 
                "B2_C2" = 7:8)
  expect_error(convert_param(param, cells_w, cells_b, "mu"), 
               "The number of mu for cell B1_C1 is not correct. Please specify either 1 or a vector of 4 per cell")
  
  param <- list("W1_X1" = 1:4, "W2_X1" = 1:4, "W1_X2" = 1:4)
  expect_error(convert_param(param, cells_w, c("y"), "mu"), 
               "The number of mu for cell W1_X1 is not correct. Please specify either 1 or a vector of 1 per cell")
  
  param <- c(10, 11)
  expect_error(convert_param(param, cells_w, cells_b, "mu"), 
                           "The number of mu is not correct. Please specify 1, a vector of 16, or use the list format")
})

# matrix specifications ----
test_that("matrix specifications", {
  cells_b <- c("A1", "A2", "A3")
  cells_w <- c("B1", "B2")
  
  # matrix
  param <- matrix(10:15, nrow = 2)
  rownames(param) <- cells_w
  colnames(param) <- cells_b
  target <- list(A1 = list(B1 = 10, B2 = 11),
                 A2 = list(B1 = 12, B2 = 13),
                 A3 = list(B1 = 14, B2 = 15))
  conv <- convert_param(param, cells_w, cells_b)
  
  expect_equal(conv, target)
})

# vector specifications 3b*2w ----
test_that("vector specifications 3b*2w", {
  cells_b <- c("A1", "A2", "A3")
  cells_w <- c("B1", "B2")
  
  # . single number ----
  param <- 10
  target <- list(A1 = list(B1 = 10, B2 = 10),
                 A2 = list(B1 = 10, B2 = 10),
                 A3 = list(B1 = 10, B2 = 10))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # . unnamed vector ----
  param <- c(10, 15, 20, 25, 30, 35)
  target <- list(A1 = list(B1 = 10, B2 = 15),
                 A2 = list(B1 = 20, B2 = 25),
                 A3 = list(B1 = 30, B2 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # . named vector ----
  # within
  target <- list(A1 = list(B1 = 10, B2 = 15),
                 A2 = list(B1 = 10, B2 = 15),
                 A3 = list(B1 = 10, B2 = 15))
  
  param <- c(B1 = 10, B2 = 15)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- c(B2 = 15, B1 = 10)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # between 
  target <- list(A1 = list(B1 = 10, B2 = 10),
                 A2 = list(B1 = 20, B2 = 20),
                 A3 = list(B1 = 30, B2 = 30))
  
  param <- c(A1 = 10, A2 = 20, A3 = 30)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- c(A2 = 20, A1 = 10, A3 = 30)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # . named list of unnamed vectors ----
  
  target <- list(A1 = list(B1 = 10, B2 = 15),
                 A2 = list(B1 = 20, B2 = 25),
                 A3 = list(B1 = 30, B2 = 35))
  
  # between
  param <- list(A1 = c(10, 15), 
                A2 = c(20, 25), 
                A3 = c(30, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(A2 = c(20, 25),
                A1 = c(10, 15), 
                A3 = c(30, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # within
  param <- list(B1 = c(10, 20, 30), 
                B2 = c(15, 25, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(B2 = c(15, 25, 35),
                B1 = c(10, 20, 30))
  expect_equal(convert_param(param, cells_w, cells_b), target)

  # . named list of named vectors ----
  
  # between
  param <- list(A1 = c(B1 = 10, B2 = 15), 
                A2 = c(B1 = 20, B2 = 25), 
                A3 = c(B1 = 30, B2 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(A2 = c(B2 = 25, B1 = 20),
                A1 = c(B2 = 15, B1 = 10), 
                A3 = c(B1 = 30, B2 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # within
  param <- list(B1 = c(A1 = 10, A2 = 20, A3 = 30), 
                B2 = c(A1 = 15, A2 = 25, A3 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(B2 = c(A2 = 25, A1 = 15, A3 = 35),
                B1 = c(A3 = 30, A1 = 10, A2 = 20))
  expect_equal(convert_param(param, cells_w, cells_b), target)
})

# vector specifications 2b ----
test_that("vector specifications 2b", {
  cells_b <- c("A1", "A2")
  cells_w <- c("y")
  
  # . single number ----
  param <- 10
  target <- list(A1 = list(y = 10),
                 A2 = list(y = 10))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # . unnamed vector ----
  param <- c(10, 15)
  target <- list(A1 = list(y = 10),
                 A2 = list(y = 15))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # . named vector ----
  param <- c(A1 = 10, A2 = 15)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- c(A2 = 15, A1 = 10)
  expect_equal(faux:::convert_param(param, cells_w, cells_b), target)
  
  # . named list of named vectors ----
  
  param <- list(A1 = c(y = 10), A2 = c(y = 15))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(A2 = c(y = 15), A1 = c(y = 10))
  expect_equal(convert_param(param, cells_w, cells_b), target)
})


# list specifications ----
test_that("list specifications", {
  cells_b <- c("A1", "A2", "A3")
  cells_w <- c("B1", "B2")
  
  # unnamed list
  param <- list(10, 15, 20, 25, 30, 35)
  target <- list(A1 = list(B1 = 10, B2 = 15),
                 A2 = list(B1 = 20, B2 = 25),
                 A3 = list(B1 = 30, B2 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # named between list of unnamed lists
  param <- list(A1 = list(10, 15), 
                A2 = list(20, 25), 
                A3 = list(30, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(A2 = list(20, 25),
                A1 = list(10, 15), 
                A3 = list(30, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # named within list of unnamed lists
  param <- list(B1 = list(10, 20, 30), 
                B2 = list(15, 25, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(B2 = list(15, 25, 35),
                B1 = list(10, 20, 30))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # named between list of named lists
  param <- list(A1 = list(B1 = 10, B2 = 15), 
                A2 = list(B1 = 20, B2 = 25), 
                A3 = list(B1 = 30, B2 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(A2 = list(B2 = 25, B1 = 20),
                A1 = list(B2 = 15, B1 = 10), 
                A3 = list(B1 = 30, B2 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # named within list of named lists
  param <- list(B1 = list(A1 = 10, A2 = 20, A3 = 30), 
                B2 = list(A1 = 15, A2 = 25, A3 = 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # change order
  param <- list(B2 = list(A2 = 25, A1 = 15, A3 = 35),
                B1 = list(A3 = 30, A1 = 10, A2 = 20))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # same for all within levels
  target <- list(A1 = list(B1 = 10, B2 = 15),
                 A2 = list(B1 = 10, B2 = 15),
                 A3 = list(B1 = 10, B2 = 15))
  
  param <- list(B1 = 10, B2 = 15)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- list(B2 = 15, B1 = 10)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # same for all between levels
  target <- list(A1 = list(B1 = 10, B2 = 10),
                 A2 = list(B1 = 20, B2 = 20),
                 A3 = list(B1 = 30, B2 = 30))
  
  param <- list(A1 = 10, A2 = 20, A3 = 30)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- list(A2 = 20, A1 = 10, A3 = 30)
  expect_equal(convert_param(param, cells_w, cells_b), target)
})


# df specifications ----
test_that("df specifications", {
  cells_b <- c("A1", "A2", "A3")
  cells_w <- c("B1", "B2")
  
  target <- list(A1 = list(B1 = 10, B2 = 15),
                 A2 = list(B1 = 20, B2 = 25),
                 A3 = list(B1 = 30, B2 = 35))
  
  # between, no row names
  param <- data.frame(A1 = c(10, 15), 
                      A2 = c(20, 25), 
                      A3 = c(30, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # between, no row names, change order 
  param <- data.frame(A1 = c(10, 15), 
                      A3 = c(30, 35),
                      A2 = c(20, 25))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # between, row names
  param <- data.frame(A1 = c(10, 15), 
                      A2 = c(20, 25), 
                      A3 = c(30, 35),
                      row.names = c("B1", "B2"))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # between, row names, change order 
  param <- data.frame(A3 = c(35, 30),
                      A1 = c(15, 10), 
                      A2 = c(25, 20), 
                      row.names = c("B2", "B1"))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # within, no row names
  param <- data.frame(B1 = c(10, 20, 30), 
                      B2 = c(15, 25, 35))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # within, no row names, change order
  param <- data.frame(B2 = c(15, 25, 35),
                      B1 = c(10, 20, 30))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # within, row names
  param <- data.frame(B1 = c(10, 20, 30), 
                      B2 = c(15, 25, 35),
                      row.names = c("A1", "A2", "A3"))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # within, row names, change order
  param <- data.frame(B2 = c(15, 35, 25),
                      B1 = c(10, 30, 20), 
                      row.names = c("A1", "A3", "A2"))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  
  # same for all within levels
  target <- list(A1 = list(B1 = 10, B2 = 15),
                 A2 = list(B1 = 10, B2 = 15),
                 A3 = list(B1 = 10, B2 = 15))
  
  param <- data.frame(B1 = 10, B2 = 15)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- data.frame(B1 = 10, B2 = 15)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- data.frame(c(10, 15), row.names = c("B1", "B2"))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- data.frame(c(15, 10), row.names = c("B2", "B1"))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  # same for all between levels
  target <- list(A1 = list(B1 = 10, B2 = 10),
                 A2 = list(B1 = 20, B2 = 20),
                 A3 = list(B1 = 30, B2 = 30))
  
  param <- data.frame(A1 = 10, A2 = 20, A3 = 30)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- data.frame(A2 = 20, A1 = 10, A3 = 30)
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- data.frame(c(10, 20, 30), row.names = c("A1", "A2", "A3"))
  expect_equal(convert_param(param, cells_w, cells_b), target)
  
  param <- data.frame(c(20, 10, 30), row.names = c("A2", "A1", "A3"))
  expect_equal(convert_param(param, cells_w, cells_b), target)

})

# guess row/col ----
test_that("matrix specifications", {
  cells_b <- c("A1", "A2", "A3")
  cells_w <- c("B1", "B2")
  
  # unnamed list
  param <- list(10:11, 12:13, 14:15)
  target <- list(A1 = list(B1 = 10, B2 = 11),
                 A2 = list(B1 = 12, B2 = 13),
                 A3 = list(B1 = 14, B2 = 15))
  conv <- convert_param(param, cells_w, cells_b)
  
  expect_equal(conv, target)
  
  param <- list(c(10, 12, 14), c(11, 13, 15))
  conv <- convert_param(param, cells_w, cells_b)
  
  expect_equal(conv, target)
})
