\name{bayes_binom_one_postlike_nstage}
\alias{bayes_binom_one_postlike_nstage}
\docType{methods}
\title{Single arm, any stage, cut-point calculator us posterior predictive distribution of a successful trial occurring to make the cuts.}
\description{
Generate cut-points given interim analyses at set numbers of patients for Bayesian posterior likelihood approach to stopping early for futility or efficacy 
}
\usage{
bayes_binom_one_postlike_nstage(reviews, prob.success, prob.failure,
 eta, zeta, p0, p1, prior.a=1e-6, prior.b=1e-6, round=TRUE, warn=TRUE)
}
\arguments{
\item{reviews}{Vector of sample sizes to perform analysis at}
\item{p0}{Probability of success under the null hypothesis}
\item{p1}{Probability of success under the alternative hypothesis}
\item{eta}{The smallest probability that p is less than p1 which is allowed to stop for futility}
\item{zeta}{The smallest probability that p is greater than p0 which is allowed to stop for efficacy}
\item{prob.success,prob.failure}{The probability of success and failure required to stop early at interim analysis}
\item{prior.a, prior.b}{The prior parameters for the beta prior distribution}
\item{round}{Optionally round the probability outputs to 3 significant figures}
\item{warn}{Turn off warnings for designs which are not optimal}
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_one}}
}
\seealso{
\code{\link{bayes_binom_one_postprob_onestage}}
}
\examples{
reviews=c(7,18)
prob.success=c(0.9)
prob.failure=c(0.9)
eta=0.9
zeta=0.9
p0=0.1
p1=0.3
prior.a=1e-6
prior.b=1e-6
bayes_binom_one_postlike_nstage(reviews,prob.success,prob.failure,
eta,zeta,p0,p1,prior.a,prior.b)
}