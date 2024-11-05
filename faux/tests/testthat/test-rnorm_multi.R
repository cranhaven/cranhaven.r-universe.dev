test_that("error messages", {
  expect_warning(rnorm_multi(n = 1, vars = 3, empirical = TRUE),
    "When n = 1 and empirical = TRUE, returned data are equal to mu")
  
  expect_error(rnorm_multi(-1), "n must be an integer > 0")
  expect_error(rnorm_multi(10.3), "n must be an integer > 0")
  expect_error(rnorm_multi("A"), "n must be an integer > 0")
  
  expect_error(
    rnorm_multi(10, 3, 1:2),
    "the length of mu must be 1 or vars"
  )
  
  expect_error(
    rnorm_multi(10, 3, 0, 1:2),
    "the length of sd must be 1 or vars"
  )
  
  expect_error(
    rnorm_multi(10, empirical = NA),
    "empirical must be TRUE or FALSE"
  )
  
  expect_error(
    rnorm_multi(10, 3, 0, 1, matrix("A", 3, 3)),
    "The correlation you specified is impossible: cors matrix not numeric" 
  )
  expect_error(
    rnorm_multi(10, 3, 0, 1, matrix(0.5, 4, 2)),
    "The correlation you specified is impossible: cors matrix wrong dimensions" 
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .75, 1), 3)
  expect_error( 
    rnorm_multi(10, 3, 0, 1, m), 
    "The correlation you specified is impossible: cors matrix not symmetric"
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 0), 3)
  expect_error(
    rnorm_multi(10, 3, 0, 1, m),
    "The correlation you specified is impossible: correlation matrix not positive definite"
  )
  
  cors <- c(-0.06826927, -0.89756943, -0.45636273)
  expect_error(
    rnorm_multi(10, 3, 0, 1, cors),
    "The correlation you specified is impossible: correlation matrix not positive definite"
  )
})

test_that("correct default parameters", {
  n <- 1e5
  dat <- rnorm_multi(n, vars = 3)
  r <- cor(dat)
  means <- dplyr::summarise_all(dat, mean)
  sds <- dplyr::summarise_all(dat, stats::sd)
  
  expect_equal(nrow(dat), n)
  expect_equal(ncol(dat), 3)
  expect_equal(means, data.frame(X1 = 0, X2 = 0, X3 = 0), tolerance = 0.05)
  expect_equal(sds, data.frame(X1 = 1, X2 = 1, X3 = 1), tolerance = 0.05)
})

test_that("correct default parameters with empirical = TRUE", {
  n <- 50
  dat <- rnorm_multi(n, vars = 3, empirical = TRUE)
  r <- cor(dat)
  means <- dplyr::summarise_all(dat, mean)
  sds <- dplyr::summarise_all(dat, stats::sd)
  
  expect_equal(nrow(dat), n)
  expect_equal(ncol(dat), 3)
  expect_equal(means, data.frame(X1 = 0, X2 = 0, X3 = 0))
  expect_equal(sds, data.frame(X1 = 1, X2 = 1, X3 = 1))
})

# names ----
test_that("names", {
  cmat <- cor(iris[,1:4])
  dat <- rnorm_multi(10, vars = 4, r = cmat)
  
  expect_equal(colnames(dat), colnames(cmat))
})

# matrix ----
test_that("matrix", {
  dat <- mat <- rnorm_multi(10, vars = 4, r = cor(iris[,1:4]))
  mat <- rnorm_multi(10, vars = 4, r = cor(iris[,1:4]), as.matrix = TRUE)
  
  expect_true(is.data.frame(dat))
  expect_true(is.matrix(mat))
})

# small n ----
test_that("small n", {
  n1 <- rnorm_multi(n = 1, vars = 1, r = 0.5)
  expect_equal(names(n1), "X1")
  expect_equal(nrow(n1), 1)
  
  n1 <- rnorm_multi(n = 1, vars = 2, r = 0.5)
  expect_equal(names(n1), c("X1", "X2"))
  expect_equal(nrow(n1), 1)
  
  n1 <- rnorm_multi(n = 1, vars = 10, r = 0.5)
  expect_equal(names(n1), c("X01", "X02", "X03", "X04", "X05", 
                            "X06", "X07", "X08", "X09", "X10"))
  expect_equal(nrow(n1), 1)
  
  v1 <- rnorm_multi(n = 10, vars = 1, r = 0.5)
  expect_equal(names(v1), "X1")
  expect_equal(nrow(v1), 10)
  
  expect_warning(
    source <- rnorm_multi(n = 1, 5, 1:5, empirical = TRUE),
    "When n = 1 and empirical = TRUE, returned data are equal to mu")
  target <- data.frame(X1 = 1, X2 = 2, X3 = 3, X4 = 4, X5 = 5)
  expect_equal(source,  target)
  
  expect_error(
    rnorm_multi(n = 2, vars = 3, r = 0.5, empirical = TRUE),
    "The correlated variables could not be generated."
  )
})

# guessing vars ----
test_that("guessing vars", {
  err <- "The number of variables (vars) was not explicitly set and can't be guessed from the input."
  
  expect_error(rnorm_multi(n = 10), err, fixed = TRUE)
  expect_error(rnorm_multi(n = 10, r = c(.5, .5, .25)), err, fixed = TRUE)
  
  s <- rnorm_multi(n = 10, mu = 1:2)
  expect_equal(ncol(s), 2)
  
  s <- rnorm_multi(n = 10, sd = 1:3)
  expect_equal(ncol(s), 3)
  
  s <- rnorm_multi(n = 10, varnames = LETTERS[1:4])
  expect_equal(ncol(s), 4)
})

# names from parameters ----
test_that("names", {
  s <- rnorm_multi(n = 10, varnames = c("A", "B"))
  expect_equal(names(s), c("A", "B"))
  
  s <- rnorm_multi(n = 10, mu = c(A = 1, B = 2))
  expect_equal(names(s), c("A", "B"))
  
  s <- rnorm_multi(n = 10, sd = c(A = 1, B = 2))
  expect_equal(names(s), c("A", "B"))
  
  r <- cor(s)
  s <- rnorm_multi(n = 10, r = r)
  expect_equal(names(s), c("A", "B"))
})

# seed ----
# test_that("seed", {
#   set.seed(10)
#   x1 <- rnorm(1)
#   x2 <- rnorm(1)
#   
#   set.seed(10)
#   x1b <- rnorm(1)
#   r1 <- rnorm_multi(n = 1, vars = 1, seed = 10)
#   x2b <- rnorm(1)
#   
#   expect_equal(x1, x1b)
#   expect_equal(x2, x2b) # returns to global state
#   expect_equal(x1, r1[[1]])
# })