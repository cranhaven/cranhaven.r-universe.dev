\name{estimateDeriv}
\alias{estimateDeriv}
\title{
Derivative Estimation
}
\description{
Estimate derivative of a function at a point d_0  based on a 
local quadratic regression procedure of Fan and Gijbels (1996) that 
utilizes an automatic bandwidth selection formula.
}
\usage{
estimateDeriv(explanatory, response, d_0, sigmaSq)
}
\arguments{
  \item{explanatory}{
Explanatory sample points
}
  \item{response}{
Observed responses at the explanatory sample points
}
  \item{d_0}{
d_0 is the point of interest where the derivative is estimated
}
  \item{sigmaSq}{
estimate of variance at d_0
}
}
\details{
This is an internal function not meant to be called directly.
}
\value{
Returns a single number representing the derivative estimate at d_0. 
If a negative derivative has been estimated, then a warning is given, as this 
violates the isotonic (non-decreasing) assumption.
}
\references{
Fan J, Gijbels I (1996). Local polynomial modelling and its applications, 
volume 66 of Monographs on Statistics and Applied Probability. 
Chapman & Hall, London. ISBN 0-412-98321-4.
}
\author{
Shawn Mankad
}
\examples{
explanatory = runif(50)
response = explanatory^2 + rnorm(50, sd=0.1)
estimateDeriv(explanatory, response, d_0=0.5,
    sigmaSq=estimateSigmaSq(explanatory, response)$sigmaSq) 

## The function is currently defined as
function (explanatory, response, d_0, sigmaSq) 
{
    deriv_estimateHelper <- function(explanatory, response, d_0, 
        sigmaSq) {
        n = length(response)
        p = 5
        X = matrix(0, n, p)
        for (i in 1:p) {
            X[, i] = (explanatory - d_0)^i
        }
        beta_hat = lm(response ~ 0 + X)$coef
        h = 0
        for (i in (p - 1):(p + 1)) {
            j = i - p + 2
            h = h + beta_hat[i - 1] * factorial(j) * d_0^(j - 
                1)
        }
        return(2.275 * (sigmaSq/h^2)^(1/7) * n^(-1/7))
    }
    n = length(response)
    p = 2
    X = matrix(0, n, p)
    X[, 1] = (explanatory - d_0)
    X[, 2] = (explanatory - d_0)^2
    bw_opt = deriv_estimateHelper(explanatory, response, d_0, 
        sigmaSq)
    W = 0.75/bw_opt * sapply(1 - ((explanatory - d_0)/bw_opt)^2, 
        max, 0)
    while (sum(W > 1) <= 1 & bw_opt <= max(explanatory) - min(explanatory)) {
        bw_opt = bw_opt * 2
        W = 0.75/bw_opt * sapply(1 - ((explanatory - d_0)/bw_opt)^2, 
            max, 0)
    }
    beta_hat = lm(response ~ 0 + X, weight = W)$coef
    while (beta_hat[1] <= 0 & bw_opt <= max(explanatory) - min(explanatory)) {
        bw_opt = bw_opt * 2
        W = 0.75/bw_opt * sapply(1 - ((explanatory - d_0)/bw_opt)^2, 
            max, 0)
        beta_hat = lm(response ~ 0 + X, weight = W)$coef
    }
    if (beta_hat[1] <= 0) {
        warning("deriv_estimate:WARNING: NEGATIVE DERIVATIVE HAS BEEN ESTIMATED", 
            .call = FALSE)
        return(1/log(n))
    }
    return(beta_hat[1])
  }
}


