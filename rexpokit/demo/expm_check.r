library(rexpokit, quietly=TRUE)
require(Matrix)

set.seed(1234)

n <- 5
x <- matrix(rnorm(n*n), nrow=n, ncol=n)

A <- rexpokit::expm(x, t=1)
B <- Matrix::expm(x)@x
dim(B) <- c(n, n)

all.equal(A, B)
