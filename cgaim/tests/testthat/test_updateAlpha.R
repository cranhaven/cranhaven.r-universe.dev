########################################################
#
# Test alpha update
#
########################################################

#----- Simple model
set.seed(2020)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
mu <- 4 * exp(8 * x1) / (1 + exp(8 * x1)) + exp(x3)
y <- mu + rnorm(n)
df2 <- data.frame(y, x1, x2, x3, x4)

# Initialize alpha and smooths
alpha <- coef(lm(y ~ ., df2))[-1]
alpha[1:2] <- cgaim:::normalize(alpha[1:2], "1")
alpha[3:4] <- cgaim:::normalize(alpha[3:4], "1")
dfgam <- data.frame(y, i1 = cbind(x1, x2) %*% alpha[1:2], 
  i2 = cbind(x3, x4) %*% alpha[3:4])
gamres <- mgcv::gam(y ~ s(i1) + s(i2), data = dfgam)
derivs <- gratia::derivatives(gamres, data = dfgam)
dgz <- matrix(derivs[[".derivative"]], ncol = 2)
gz <- stats::predict(gamres, type = "terms")

# Test with constraints
Cmat <- as.matrix(Matrix::bdiag(list(diff(diag(2)), diff(diag(2)))))

# Test all solvers
resosqp <- alpha_update(y = y, x = as.matrix(df2[,-1]), w = rep(1, n), 
  index = rep(1:2, each = 2), dgz = dgz, 
  alpha = alpha, Cmat = Cmat, 
  bvec = rep(0, 2), solver = "osqp", ctol = 0.001, 
  qp_pars = list(verbose = F))$alpha

resquadprog <- alpha_update(y = y, x = as.matrix(df2[,-1]), w = rep(1, n), 
  index = rep(1:2, each = 2), dgz = dgz, 
  alpha = alpha, Cmat = Cmat, 
  bvec = rep(0, 2), solver = "quadprog", ctol = 0.001, 
  qp_pars = list())$alpha

resconeproj <- alpha_update(y = y, x = as.matrix(df2[,-1]), w = rep(1, n), 
  index = rep(1:2, each = 2), dgz = dgz, 
  alpha = alpha, Cmat = Cmat, 
  bvec = rep(0, 2), solver = "coneproj", ctol = 0.001, 
  qp_pars = list())$alpha

test_that("Update respects constraints", {
  expect_true(all(Cmat %*% resosqp >= 0))
  expect_true(all(Cmat %*% resquadprog >= 0))
  expect_true(all(Cmat %*% resconeproj >= 0))
})

# system.time(replicate(100, alpha_update(y = y, x = as.matrix(df2[,-1]), w = rep(1, n), 
#   index = rep(1:2, each = 2), dgz = sapply(derivs$derivatives, "[[", "deriv"), 
#   alpha = alpha, Cmat = Cmat, 
#   bvec = rep(0, 2), solver = "osqp", ctol = 0.001, 
#   qp_pars = list(verbose = F))))
