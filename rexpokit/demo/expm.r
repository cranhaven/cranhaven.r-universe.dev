library(rexpokit, quietly=TRUE)

set.seed(1234)

n <- 5
x <- matrix(rnorm(n*n), nrow=n, ncol=n)

expm(x)
