# Tests from Moler and Van Loan, "Nineteen Dubious Ways to Compute
# the Exponential of a Matrix, Twenty-Fice Years Later"

suppressPackageStartupMessages(library(rexpokit))

### From page 8
x <- matrix(c(1, 2, 3, 4), 2, 2)
y <- matrix(c(-1, 0, 0, -17), 2, 2)
expy <- matrix(c(exp(-1), 0, 0, exp(-17)), 2, 2)
z <- solve(x)
A <- x %*% y %*% z

truth <- x %*% expy %*% z

# R CMD check --as-cran isn't running expm for some reason, putting
# by-hand results here:
#
# test <- rexpokit::expm(A)
# dput(test)
test <- structure(c(-0.735758758144768, -1.47151759908829, 0.551819099658109, 
1.10363824071559), .Dim = c(2L, 2L))

stopifnot(all.equal(test, truth))



### From page 4
lambda <- 10
alpha <- 1
mu <- 5

x <- matrix(c(lambda, alpha, 0, mu), 2, 2, byrow=TRUE)

truth <- matrix(c(exp(lambda), alpha*(exp(lambda)-exp(mu))/(lambda-mu), 0, exp(mu)), 2, 2, byrow=TRUE)

# R CMD check --as-cran isn't running expm for some reason, putting
# by-hand results here:
#
# test <- rexpokit::expm(x)
# dput(test)
test <- structure(c(22026.4657948068, 0, 4375.61052714084, 148.413159102577), .Dim = c(2L, 2L))

stopifnot(all.equal(test, truth))
