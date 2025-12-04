context("variance functions")

# simulate matrices
library(Matrix)
get_spm <- function(nrow=30, ncol=3, seed=100, lambda=.01){
  set.seed(seed)
 mat <- matrix(rpois(nrow*ncol, lambda), ncol=ncol)
 mat <- as(mat, "dgCMatrix")
 return(mat)
}

test_that("colVars_spm works", {
  expect_equal( colVars_spm(get_spm(3, 3)), apply(get_spm(3,3), 2, var) )
  expect_equal( colVars_spm(get_spm(1, 3)), apply(get_spm(1,3), 2, var) )
  expect_equal( colVars_spm(get_spm(100, 3)), apply(get_spm(100,3), 2, var) )
  expect_equal( colVars_spm(get_spm(100, 3000)), apply(get_spm(100,3000), 2, var) )
  expect_equal( colVars_spm(get_spm(1000, 3)), apply(get_spm(1000,3), 2, var) )
})

test_that("rowVars_spm works", {
  expect_equal( rowVars_spm(get_spm(3, 3)), apply(get_spm(3,3), 1, var) )
  expect_equal( rowVars_spm(get_spm(1, 3)), apply(get_spm(1,3), 1, var) )
  expect_equal( rowVars_spm(get_spm(100, 3)), apply(get_spm(100,3), 1, var) )
  expect_equal( rowVars_spm(get_spm(100, 3000)), apply(get_spm(100,3000), 1, var) )
  expect_equal( rowVars_spm(get_spm(1000, 3)), apply(get_spm(1000,3), 1, var) )
})

