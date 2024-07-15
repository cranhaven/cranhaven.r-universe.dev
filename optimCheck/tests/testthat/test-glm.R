#--- test glm ------------------------------------------------------------------

context("Logistic regression: projection plots")

source("optimCheck-testfunctions.R")

# likelihood function
loglik <- function(beta, y, X) {
  sum(dbinom(y, size = 1,
             prob = binomial()$linkinv(X %*% beta), log = TRUE))
}

ntest <- 50

test_that("glm/logistic converges according to optim_proj.", {
  replicate(ntest, {
    # generate data
    n <- sample(100:200,1)
    p <- sample(2:10,1)
    X <- rMnorm(n,p)
    beta0 <- rnorm(p, sd = .1)
    # response
    y <- rbinom(n, size = 1, prob = binomial()$linkinv(X %*% beta0))
    # fit glm
    beta.hat <- coef(glm(y ~ X - 1, family = binomial))
    # check with optim_proj
    ocheck <- optim_proj(fun = function(beta) loglik(beta, y, X),
                         xsol = beta.hat, plot = FALSE)
    # largest of min(abs,rel) difference between xsol and xopt
    expect_lt(max.xdiff(ocheck), .01)
  })
})

context("Logistic regression: \"refit\" with optim")

test_that("glm/logistic converges according to optim_refit.", {
  replicate(ntest, {
    # generate data
    n <- sample(100:200,1)
    p <- sample(2:10,1)
    X <- rMnorm(n,p)
    beta0 <- rnorm(p, sd = .1)
    # response
    y <- rbinom(n, size = 1, prob = binomial()$linkinv(X %*% beta0))
    # fit glm
    beta.hat <- coef(glm(y ~ X - 1, family = binomial))
    # check with optim_proj
    ocheck <- optim_refit(fun = function(beta) loglik(beta, y, X),
                          xsol = beta.hat)
    # largest of min(abs,rel) difference between xsol and xopt
    expect_lt(max.xdiff(ocheck), .01)
  })
})


## beta.names <- parse(text = paste0("beta[", 1:p, "]"))
## plot(ocheck, xnames = beta.names, xlab = "Coefficient", ylab = "Loglikelihood"

## system.time({
##   ocheck2 <- optim_refit(xsol = beta.hat, fun = loglik)
## })
