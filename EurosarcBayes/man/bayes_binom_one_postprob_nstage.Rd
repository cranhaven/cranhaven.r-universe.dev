\name{bayes_binom_one_postprob_nstage}
\alias{bayes_binom_one_postprob_nstage}
\docType{methods}
\title{Single arm, any stage, cut-point calculator using posterior probabilities to make the cuts.}
\description{
Generate cut-points given interim analyses at set numbers of patients for Bayesian posterior probability approach to stopping early for futility or efficacy 
}
\usage{
bayes_binom_one_postprob_nstage(reviews, eta, zeta, p0, p1, 
  prior.a=0, prior.b=0, h0=p0, h1=p1, round=TRUE, warn=TRUE)
}
\arguments{
\item{reviews}{Vector of sample sizes to perform analysis at}
\item{p0}{Probability of success under the null hypothesis}
\item{p1}{Probability of success under the alternative hypothesis}
\item{eta}{The smallest probability that p is less than p1 which is allowed to stop for futility}
\item{zeta}{The smallest probability that p is greater than p0 which is allowed to stop for efficacy}
\item{h0,h1}{Optional values to be used if the properties of the design should be based on hypotheses which do not use the last values of p0 and p1.}
\item{prior.a,prior.b}{The prior parameters for the beta prior distribution}
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
eta=c(0.9,0.9)
zeta=c(0.9,0.9)
p0=0.1
p1=0.3
prior.a=0
prior.b=0
bayes_binom_one_postprob_nstage(reviews,eta,zeta,p0,p1,prior.a,
prior.b)
}