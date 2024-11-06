\name{bayes_binom_one_postprob_onestage}
\alias{bayes_binom_one_postprob_onestage}
\alias{shiny_binom_single_onestage}
\docType{methods}
\title{Bayesian single-arm single-endpoint minimum sample size}
\description{
Generate  minimum sample size for the Bayesian single-endpoint single-arm trial. Also provided a shiny app to evaluate the same thing with both frequentist and Bayesian methods side by side.
}
\usage{
bayes_binom_one_postprob_onestage(p0, p1, eta, zeta, prior.a, 
prior.b, round=TRUE)

shiny_binom_single_onestage()
}
\arguments{
\item{p0}{Probability of success under the null hypothesis}
\item{p1}{Probability of success under the alternative hypothesis}
\item{eta}{The smallest probability that p is less than p1 which is allowed to stop for futility}
\item{zeta}{The smallest probability that p is greater than p0 which is allowed to stop for efficacy}
\item{prior.a,prior.b}{The prior parameters for the beta prior distribution}
\item{round}{Optionally round the probability outputs to 3 significant figures}
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_one}}
}
\seealso{
\code{\link{bayes_binom_one_postprob_nstage}}
}
\examples{
p0=0.1
p1=0.3
eta=c(0.9)
zeta=c(0.9)
prior.a=0
prior.b=0
bayes_binom_one_postprob_onestage(p0,p1,eta,zeta,prior.a,prior.b)
}