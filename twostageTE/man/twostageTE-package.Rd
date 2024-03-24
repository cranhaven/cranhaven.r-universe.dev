\name{twostageTE}
\alias{twostageTE-package}
\alias{twostageTE}
\docType{package}
\title{
Threshold value estimation using two-stage plans
}
\description{
This package implements a variety of nonparametric methods for 
computing one-stage and two-stage confidence intervals and point 
estimates of threshold values.
}
\details{
\tabular{ll}{
Package: \tab twostageTE\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-23\cr
License: \tab GPL-2\cr
}
The user interacts with the package by utilizing two functions:
stageOneAnalysis and stageTwoAnalysis. These functions take the
sampled explanatory variable and corresponding responses at the 
first and second stage, respectively, and outputs point estimate 
and confidence intervals based on different user specific procedures.
}
\author{
Shawn Mankad
Maintainer: Shawn Mankad <smankad@umich.edu>
}
\references{
Shawn Mankad, George Michailidis, Moulinath Banerjee (2015).
Threshold Value Estimation Using Adaptive Two-Stage Plans in R.
Journal of Statistical Software, 67(3), 1-19.
doi:10.18637/jss.v067.i03
}
\keyword{ package }
\examples{
## Simulating the (wiggly) isotonic Sine function ##
sampleData=function(n, lower, upper) {
x=runif(n, lower, upper)
y=(1/40)*sin(6*pi*x) + 1/4 + x/2 + (1/4)*x^2
+ rnorm(n=length(x), sd=0.1)
return(list(X=x, Y=y))
}
Budget=100
d0=0.5
threshold = (1/40)*sin(6*pi*d0) + 1/4 + d0/2 + (1/4)*d0^2

n1=floor(Budget*0.25)
n2=Budget - n1
samp = sampleData(n1, lower=0, upper=1)
X = samp$X
Y = samp$Y
## Two Stage IR+IR ##
stageOne_IR=stageOneAnalysis(X, Y, threshold, type="IR-wald", 0.99)
samp2 = sampleData(n2, lower=stageOne_IR$L1, upper=stageOne_IR$U1)
X2 = samp2$X
Y2 = samp2$Y
twoStageIR = stageTwoAnalysis(stageOne_IR, X2, Y2, type="IR-wald", 0.95)
## Two Stage LR+LR ##
stageOne_LR=stageOneAnalysis(X, Y, threshold, type="IR-likelihood", 0.99)
samp2 = sampleData(n2, lower=stageOne_LR$L1, upper=stageOne_LR$U1)
X2 = samp2$X
Y2 = samp2$Y
twoStageLR = stageTwoAnalysis(stageOne_LR, X2, Y2, 
    type="IR-likelihood", 0.95)
## Two Stage IR+Local Linear ##
X2 = c(rep(stageOne_IR$L1,37),rep(stageOne_IR$U1,38))
Y2=X2^2+rnorm(n=length(X2), sd=0.1)
twoStageLinear=stageTwoAnalysis(stageOne_IR, explanatory = X2, response = Y2,
    type = "locLinear", level = 0.95)
}

