\name{exp_par}
\alias{exp_par}
\title{Fitting causal log-linear models without the multiplicative interaction}
\description{ exp_par calculates the causal parameters for a causal log-linear model under dummy code parametrization
}
\usage{exp_par(table)}
\arguments{
  \item{table}{it is a table containing the frequency distribution of the variables}}

\value{ exp_par returns the estimated causal parameters of the multiplicative form
}
\references{
Gheno Gloria (2015), The causal analysis in the loglinear model}
\author{
Gloria Gheno}
\examples{
tableXZY<-data.frame(expand.grid( 
 X=factor(c("0","1"),levels=c("0","1")), 
 Z=factor(c("0","1"),levels=c("0","1")), 
  Y=factor(c("0","1"),levels=c("0","1"))), 
 count=c(57,47,21,39,31,40,20,95))
exp_par(tableXZY)
}

