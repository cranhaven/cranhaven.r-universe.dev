
library(qpmadr)
library(tinytest)

n = 6
k = 3

set.seed(42)

Q = crossprod(matrix(rnorm(n*n), n))
invQchol = solve(chol(Q))
Q0 = Q
Q0[1,1] = 0
A = matrix(5, k, n)

Alb = c(1, numeric(k-1))
l = rep_len(1, n)


expect_error(solveqp(Q))
expect_error(solveqp(Q, A=A))
expect_true(solveqp(Q, lb = l)$status == 0)
expect_true(solveqp(Q0, lb = l)$status == -1)
expect_true(all(is.finite(solveqp(Q, A=A, Alb=Alb)$solution)))



resLlt = solveqp(Q, A=A, Alb=Alb)
resQpm = solveqp(t(chol(Q)), A=A, Alb=Alb, pars = list(factorizationType="CHOLESKY"))
resChol = solveqp(invQchol, A=A, Alb=Alb, pars = list(factorizationType="INV_CHOLESKY"))

expect_equal(resLlt, resQpm)
expect_equal(resLlt, resChol)


resFull = solveqp(Q, A=A, Alb=Alb, pars=qpmadr::qpmadParameters(withLagrMult = TRUE, returnInvCholFac = TRUE))

expect_inherits(resFull$lagrangeMult, "data.frame")
expect_equal(resFull$invHessian[upper.tri(Q)], invQchol[upper.tri(Q)])

