\name{freq_binom_one_onestage}
\alias{freq_binom_one_onestage}
\docType{methods}
\title{Bayesian single-arm single-endpoint minimum sample size}
\description{
Generate  minimum sample size for the frequentist single-endpoint single-arm trial. Also provided a shiny app to evaluate the same thing with both frequentist and Bayesian methods side by side.
}
\usage{
freq_binom_one_onestage(p0, p1, alpha, power, prior.a=0, prior.b=0, 
 round=TRUE)

shiny_binom_single_onestage()
}
\arguments{
\item{p0}{Probability of success under H0}
\item{p1}{Probability of success under H1}
\item{alpha}{The largest allowed value for the frequentist type one error}
\item{power}{The smallest allowed frequentist power}
\item{prior.a,prior.b}{The prior parameters for the beta prior distribution}
\item{round}{Optionally round the probability outputs to 3 significant figures}
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_one}}
}
\examples{
p0=0.1
p1=0.3
alpha=0.1
power=0.8
prior.a=0
prior.b=0
freq_binom_one_onestage(p0,p1,alpha,power,prior.a,prior.b)
}