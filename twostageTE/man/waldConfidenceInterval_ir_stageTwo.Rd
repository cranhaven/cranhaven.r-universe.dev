\name{waldConfidenceInterval_ir_stageTwo}
\alias{waldConfidenceInterval_ir_stageTwo}
\title{
Two-stage IR-Wald confidence interval
}
\description{
This is an internal function not meant to be called directly.
IR-Wald confidence interval that can be called at the second 
stage of a multistage procedure
}
\usage{
waldConfidenceInterval_ir_stageTwo(explanatory, response, 
    Y_0, gamma1, C_1, n1, level = NA)
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
  \item{gamma1}{
Constant that is used in the first stage confidence interval
}
  \item{C_1}{
Constant that is used in the first stage confidence interval
}
  \item{n1}{
Sample size of the first stage
}
  \item{level}{
Desired confidence level
}
}
\value{
\item{estimate}{Point estimate for d_0}
\item{lower}{Lower bound of the confidence interval}
\item{upper}{upper bound of the confidence interval}
\item{sigmaSq}{Estimate of variance}
\item{deriv_d0}{Estimate of the derivative at d_0}
}
\references{
Tang R, Banerjee M, Michailidis G, Mankad S (2013). 'Two-Stage Plans for Estimating a
Threshold Value of a Regression Function.' http://arxiv.org/abs/1304.4637
}
\author{
Shawn Mankad
}
\examples{
X=runif(25, 0,1)
Y=X^2+rnorm(n=length(X), sd=0.1)
oneStage_IR=stageOneAnalysis(X, Y, 0.25, type="IR-wald", 0.99)
X2=runif(75,oneStage_IR$L1 ,oneStage_IR$U1)
Y2=X2^2+rnorm(n=length(X2), sd=0.1)
twoStage_IR_IR = stageTwoAnalysis(oneStage_IR, X2, Y2, type="IR-wald", 0.95)

## The function is currently defined as
function (explanatory, response, Y_0, gamma1, C_1, n1, level = NA) 
{
    if (is.na(level)) {
        level = 0.95
    }
    alpha = 1 - level
    chernoff_realizations <- NULL; rm(chernoff_realizations);
    data("chernoff_realizations", envir =environment())

    ind = min(which(chernoff_realizations$DF - (1-alpha/2) >= 0))
    q = chernoff_realizations$xcoor[ind]
    n = length(response)
    fit = threshold_estimate_ir(explanatory, response, Y_0)
    phi_0 = C_1 * n1 * (n^(-1))
    sigmaSq = estimateSigmaSq(explanatory, response)$sigmaSq
    deriv_d0 = estimateDeriv(explanatory, response, fit$threshold_estimate_explanatory, 
        sigmaSq)
    C_di = (4 * sigmaSq/(deriv_d0^2))^(1/3)
    n = length(explanatory)
    p = gamma1/(1 + gamma1)
    C_di2 = C_di * (C_1/((1 - p) * p^(gamma1) * phi_0))
    band = n^(-1 * (1 + gamma1)/3) * C_di2 * q
    return(list(estimate = fit$threshold_estimate_explanatory, 
        lower = max(min(explanatory), fit$threshold_estimate_explanatory - 
            band), upper = min(max(explanatory), fit$threshold_estimate_explanatory + 
            band), sigmaSq = sigmaSq, deriv_d0 = deriv_d0))
  }
}


