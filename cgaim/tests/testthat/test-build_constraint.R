# Create AIM model
Alpha <- c(.7, .2, .1, 0)
n <- 1000

# Generate data
p <- length(Alpha)
X <- matrix(rnorm(n * p), nrow = n)
Y <- 5 + scale(X %*% Alpha)

# QP matrices
Dmat <- crossprod(X)
dvec <- drop(crossprod(X, Y))

#----- Positive decreasing
# Create constraints matrix
full <- rbind(diag(p), -diff(diag(p)))
reduced <- build_constraints(p, sign = 1, monotone = -1)

# Fit QP
fullres <- quadprog::solve.QP(Dmat, dvec, t(full))
redres <- quadprog::solve.QP(Dmat, dvec, t(reduced))

#----- Increasing convex
# Create constraints matrix
full2 <- rbind(diff(diag(p)), diff(diag(p), diff = 2))
reduced2 <- build_constraints(p, first = 0, monotone = 1, convex = 1)

# Fit QP
fullres2 <- quadprog::solve.QP(Dmat, dvec, t(full2))
redres2 <- quadprog::solve.QP(Dmat, dvec, t(reduced2))

#----- Test
test_that("Solution is feasible", {
  expect_true(all((full %*% redres$solution) >= -.Machine$double.eps))
  expect_true(all((full2 %*% redres2$solution) >= -.Machine$double.eps))
})

test_that("Result is identicial with full and reduced matrices", {
  expect_equal(redres$solution, fullres$solution)
  expect_equal(redres2$solution, fullres2$solution)
})