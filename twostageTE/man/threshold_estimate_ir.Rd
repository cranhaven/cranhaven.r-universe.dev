\name{threshold_estimate_ir}
\alias{threshold_estimate_ir}
\title{
Threshold estimate based on IR
}
\description{
Uses isotonic regression and PAVA to form a point estimate.
}
\usage{
threshold_estimate_ir(explanatory, response, Y_0)
}
\arguments{
  \item{explanatory}{
Explanatory sample points
}
  \item{response}{
Observed responses at the explanatory sample points
}
  \item{Y_0}{
Threshold of interest
}
}
\details{
This is an internal function not meant to be called directly.
It function relies on the PAVA algorithm to form a point estimate. 
}
\value{
list(threshold_estimate_explanatory = estim_x, threshold_estimate_response = fit$y[ind], 
    threshold = Y_0, Y_hat = fit$y, index = ind)
\item{threshold_estimate_explanatory}{Point estimate of d_{0}}
\item{threshold_estimate_response}{Estimate of f(d_{0}), which may not be 
    exactly equal to the desired threshold}
\item{threshold}{Threshold of interest (equal to Y_0 input)}
\item{Y_hat}{Fitted values from PAVA}
\item{index}{index that corresponds to the point estimate, so 
    that Y_hat[index]=threshold_estimate_response}
}
\author{
Shawn Mankad
}
\examples{
X=runif(25, 0,1)
Y=X^2+rnorm(n=length(X), sd=0.1)
stageOneAnalysis(X, Y, 0.25, type="IR-wald", 0.99)

## The function is currently defined as
function (explanatory, response, Y_0) 
{
    n = length(response)
    if (sum(response < Y_0) == n) {
        warning("Y_0 is outside observed region")
        list(threshold_estimate_explanatory = max(explanatory), 
            threshold_estimate_response = max(response), threshold = Y_0, 
            Y_hat = max(response), index = n)
    }
    else if (sum(response >= Y_0) == n) {
        warning("Y_0 is outside observed region")
        list(threshold_estimate_explanatory = min(explanatory), 
            threshold_estimate_response = min(response), threshold = Y_0, 
            Y_hat = min(response), index = 1)
    }
    else {
        fit = pava(explanatory, response)
        if (sum(fit$y >= Y_0) == 0) {
            warning("estimate is on the boundary")
            ind = n
            estim_x = fit$x[ind]
        }
        else if (sum(fit$y <= Y_0) == 0) {
            warning("estimate is on the boundary")
            ind = min(which(fit$y >= Y_0))
            estim_x = fit$x[ind]
        }
        else {
            ind = min(which(fit$y >= Y_0))
            estim_x = fit$x[ind]
        }
        list(threshold_estimate_explanatory = estim_x, 
            threshold_estimate_response = fit$y[ind], 
            threshold = Y_0, Y_hat = fit$y, index = ind)
    }
  }
}


