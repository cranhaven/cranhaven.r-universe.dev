\name{freq_binom_one_landemets}
\alias{freq_binom_one_landemets}
\docType{methods}
\title{Single arm, two stage, Binomial sample size calculator}
\description{
Sample size calculation for single arm, multistage trials using the alpha spending approach to reduce type I and type II error rates. This implimentation uses the O'Brien-Fleming alpha spending function for this purpose.
}
\usage{
freq_binom_one_landemets(reviews, p0, p1, r=c(p0,p1), 
	alpha=0.1, beta=0.1, prior.a=0, prior.b=0)
}
\arguments{
\item{reviews}{A vector of the number of patients to perform interim analysis at}
\item{p0}{Probability of success under the H0}
\item{p1}{Probability of success under the H1}
\item{r}{A vector of probabilities used to perform simulations from}
\item{alpha}{The largest allowed value for the frequentist type one error}
\item{beta}{The smallest allowed value for the frequentist type two error}
\item{prior.a, prior.b}{Prior parameters for the beta prior}
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_one}}
}
\examples{
reviews=c(11,22,33,44)
p0=0.2
p1=0.35
r=c(0.2,0.35)
alpha=0.1
beta=0.2
freq_binom_one_landemets(reviews,p0,p1,r,alpha,beta)
}
\references{
DeMets, D. L. and G. Lan (1995). The alpha spending function approach to interim data analyses. Cancer Treat Res 75: 1-27.

O'Brien, P. C. and T. R. Fleming (1979). A Multiple Testing Procedure for Clinical Trials. Biometrics 35(3): 549-556.
}