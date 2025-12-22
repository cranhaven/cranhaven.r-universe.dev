\name{print.regsdml}
\alias{print.regsdml}
\title{Printing regsdml fits}
\usage{
\method{print}{regsdml}(x, ...)
}
\arguments{
\item{x}{An object of class \code{regsdml}. This object usually results
from a function call to \code{\link{regsdml}}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
By default, \code{summary(x)} is called. Please see
    \code{\link{summary.regsdml}} for further details.
}
\description{
This is a method for the class \code{regsdml}.
    It prints
    objects of class \code{regsdml}, which typically result from a function
    call to \code{\link{regsdml}}.
}
\examples{
## Generate some data:
set.seed(19)
# true linear parameter
beta0 <- 1
n <- 40
# observed confounder
w <- pi * runif(n, -1, 1)
# instrument
a <- 3 * tanh(2 * w) + rnorm(n, 0, 1)
# unobserved confounder
h <- 2 * sin(w) + rnorm(n, 0, 1)
# linear covariate
x <- -1 * abs(a) - h - 2 * tanh(w) + rnorm(n, 0, 1)
# response
y <- beta0 * x - 3 * cos(pi * 0.25 * h) + 0.5 * w ^ 2 + rnorm(n, 0, 1)

## Estimate the linear coefficient from x to y
## (The parameters are chosen small enough to make estimation fast):
## Caveat: A spline estimator is extrapolated, which raises a warning message.
## Extrapolation lies in the nature of our method. To omit the warning message
## resulting from the spline estimator, another estimator may be used.
fit <- regsdml(a, w, x, y,
               gamma = exp(seq(-4, 1, length.out = 4)),
               S = 3,
               do_regDML_all_gamma = TRUE,
               cond_method = c("forest",  # for E[A|W]
                               "spline",  # for E[X|W]
                               "spline"), # for E[Y|W]
               params = list(list(ntree = 1), NULL, NULL))
print(fit)
}
\seealso{
\code{\link{regsdml}},
    \code{\link{summary.regsdml}},
    \code{\link{confint.regsdml}},
    \code{\link{coef.regsdml}},
    \code{\link{vcov.regsdml}}
}
