\name{freq_binom_one_simons_twostage}
\alias{freq_binom_one_simons_twostage}
\alias{shiny_binom_single_twostage}
\docType{methods}
\title{Single arm, two stage, Binomial sample size calculator}
\description{
Sample size calculation for single arm, two stage designs (Simon's optimal and minmax designs) where stoping early for futility is permitted. Returns frequentist and Bayesian properties for the designs.

A shiny app is also provided. This is interactive for Simon's two stage design and also describes a number of multistage designs for the same problem.
}
\usage{
freq_binom_one_simons_twostage(p0, p1, alpha, power, prior.a=0,
	prior.b=0, nmax=100, round=TRUE, method="optimal")
 
shiny_binom_single_twostage()
}
\arguments{
\item{p0}{Probability of success under H0}
\item{p1}{Probability of success under H1}
\item{alpha}{The largest allowed value for the frequentist type one error}
\item{power}{The smallest allowed frequentist power}
\item{prior.a,prior.b}{The prior parameters for the beta prior distribution}
\item{nmax}{The maximum sample size to search up to}
\item{round}{Optionally round the probability outputs to 3 significant figures}
\item{method}{Defining the method of optimisation. Either "optimal" or "minmax"}
}
\details{
freq_binom_one_simons_twostage is a wrapper function. It uses \code{\link{ph2simon}} from the clinfun package to generate optimal sample sizes for the the frequentist single arm, two stage designs. Frequentist and Bayesian properties are then calculated using properties_binom_one and then optimal and minimax designs are returned.
}
\value{
Returns an object of class \code{\linkS4class{trialDesign_binom_one}}
}
\seealso{
	\code{\link{ph2simon}}
}
\examples{
p0=0.2
p1=0.35
alpha=0.1
power=0.8
freq_binom_one_simons_twostage(p0,p1,alpha,power)
}
\references{
Simon R. (1989). Optimal Two-Stage Designs for Phase II Clinical Trials. Controlled Clinical Trials 10, 1-10.
}