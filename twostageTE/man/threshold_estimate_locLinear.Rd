\name{threshold_estimate_locLinear}
\alias{threshold_estimate_locLinear}
\title{
Threshold estimate based on local linear approximation
}
\description{
The main idea for the procedure in Tang et al. (2011) is to utilize 
a local linear approximation in the vicinity of the first stage estimate, and 
to bootstrap this local approximation to obtain confidence intervals.
}
\usage{
threshold_estimate_locLinear(explanatory, response, Y_0)
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
It function uses a local linear approximation to form a point estimate. 
}
\value{
\item{threshold_estimate_explanatory}{Point estimate of d_0}
\item{threshold}{Threshold of interest (equal to Y_0 input)}
}
\references{
Tang R, Banerjee M, Michailidis G (2011). 'A two-stage hybrid procedure 
for estimating an inverse regression function.' The Annals of Statistics,
39, 956-989.
}
\author{
Shawn Mankad
}
\examples{
X=runif(25, 0,1)
Y=X^2+rnorm(n=length(X), sd=0.1)
oneStage_IR=stageOneAnalysis(X, Y, 0.25, type="IR-wald", 0.99)
X2 = c(rep(oneStage_IR$L1,37),rep(oneStage_IR$U1,38))
Y2=X2^2+rnorm(n=length(X2), sd=0.1)
stageTwoAnalysis(oneStage_IR, explanatory = X2, response = Y2,
type = "locLinear", level = 0.95)


## The function is currently defined as
function (explanatory, response, Y_0) 
{
    n = length(response)
    if (sum(response < Y_0) == n) {
        list(threshold_estimate_explanatory = max(explanatory), 
            threshold_estimate_response = max(response), threshold = Y_0, 
            Y_hat = max(response), index = n)
    }
    else if (sum(response >= Y_0) == n) {
        list(threshold_estimate_explanatory = min(explanatory), 
            threshold_estimate_response = min(response), threshold = Y_0, 
            Y_hat = min(response), index = 1)
    }
    else {
        beta = lm(response ~ explanatory)$coef
        estim_x = (Y_0 - beta[1])/beta[2]
        list(threshold_estimate_explanatory = estim_x, threshold = Y_0)
    }
  }
}

