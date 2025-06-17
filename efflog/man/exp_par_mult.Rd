\name{exp_par_mult}
\alias{exp_par_mult}
\title{Fitting causal log-linear model}
\description{ exp_par_mult calculates the causal parameters for a causal log-linear model with the multiplicative interaction
}
\usage{exp_par_mult(table)}
\arguments{
  \item{table}{it is a table containing the frequency distribution of the variables}}
\value{ exp_par_mult returns the estimated causal parameters of the multiplicative form
}
\references{
Gheno Gloria (2015), The causal analysis in the loglinear model}
\author{
Gloria Gheno
}
\examples{
tableXZY<-data.frame(expand.grid( 
 X=factor(c("0","1"),levels=c("0","1")), 
  Z=factor(c("0","1"),levels=c("0","1")), 
 Y=factor(c("0","1"),levels=c("0","1"))), 
 count=c(57,47,21,39,31,40,20,95))
exp_par_mult(tableXZY)
}
