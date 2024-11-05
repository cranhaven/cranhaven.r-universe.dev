context("test-cormat")

test_that("errors", {
  expect_error(
    cormat(matrix("A", 3, 3)),
    "cors matrix not numeric" 
  )
  expect_error(
    cormat(matrix(0.5, 4, 2)),
    "cors matrix wrong dimensions" 
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .75, 1), 3)
  expect_error( 
    cormat(m), 
    "cors matrix not symmetric"
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 0), 3)
  expect_error(
    cormat(m),
    "correlation matrix not positive definite"
  )
  
  cors <- c(-0.06826927, -0.89756943, -0.45636273)
  expect_error(
    cormat(cors),
    "correlation matrix not positive definite"
  )
})

test_that("correct matrix", {
  # specify by single value
  mat1 <- cormat(.5, 3)
  compmat1 <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), 3)
  expect_equal(mat1, compmat1)
  
  # specify by vars*vars vector
  mat2 <- cormat(c( 1, .2, .3, .4,
                   .2,  1, .5, .6, 
                   .3, .5,  1, .7,
                   .4, .6, .7,  1), 4)
  # specify by vars*(vars-1)/2 vector
  mat3 <- cormat(c(.2, .3, .4, .5, .6, .7), 4)
  expect_equal(mat2, mat3)
  
  compmat2 <- matrix(c(  1, .2, .3, .4,
                        .2,  1, .5, .6, 
                        .3, .5,  1, .7,
                        .4, .6, .7,  1), 4)
  expect_equal(mat2, compmat2)
  
  # larger vector
  mat4 <- cormat(c(.2, .3, .4, -.45, .5, .6, -.65, .7, -.75, -.85), 5)
  compmat4 <- matrix(c(  1, .2, .3, .4, -.45,
                         .2,  1, .5, .6, -.65,
                         .3, .5,  1, .7, -.75,
                         .4, .6, .7,  1, -.85,
                         -.45,-.65,-.75,-.85, 1), 5)
  expect_equal(mat4, compmat4)
  
  # specify by matrix
  mat5 <- cormat(compmat4, 5)
  expect_equal(mat5, compmat4)
})

