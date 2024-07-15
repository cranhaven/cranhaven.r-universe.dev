context("Quantile regression")

source("optimCheck-testfunctions.R")

# quantile regression objective function
qr.obj <- function(y, X, beta, tau) {
  u <- y - c(X %*% beta)
  sum(u * (tau - (u < 0)))
}

# automated tests
ntest <- 20
test_that("quantreg::rq converges to local mode", {
  skip_if_not(requireNamespace("quantreg", quietly = TRUE),
              "quantreg package required to run this test.")
  require(quantreg)
  replicate(ntest, expr = {
    n <- sample(100:1000, 1)
    p <- sample(1:20, 1)
    X <- matrix(rnorm(n*p), n, p)
    colnames(X) <- paste0("x", 1:p)
    beta <- rnorm(p)
    y <- c(X %*% beta) + rnorm(n)
    ds <- data.frame(y = y, X)
    tau <- runif(1)
    M <- rq(y ~ . - 1, tau = tau, data = ds)
    beta.hat <- coef(M)
    ocheck <- optim_proj(fun = function(beta) {
      qr.obj(y = y, X = X, beta = beta, tau = tau)
    }, xsol = beta.hat, maximize = FALSE, plot = FALSE)
    # largest of min(abs,rel) difference between xsol and xopt
    expect_lt(max.xdiff(ocheck), .01)
  })
})
