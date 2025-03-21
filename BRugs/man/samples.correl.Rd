\name{samplesCorrel}
\alias{samplesCorrel}
\title{Correlation}
\description{
This function calculates the correlation matrix between two vectors of variables.
}
\usage{
samplesCorrel(node0, node1, beg = samplesGetBeg(), 
    end = samplesGetEnd(), firstChain = samplesGetFirstChain(), 
    lastChain = samplesGetLastChain(), thin = samplesGetThin())
}
\arguments{
  \item{node0, node1}{Character vectors of length 1, name of variables in the model.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{firstChain, lastChain}{Arguments to select a sub group of chains to calculate correlation(s) for.}
  \item{thin}{to only use every \code{thin}-th value of the stored sample for statistics.}
}
\details{
    If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    
    If the arguments are left at their defaults the whole sample for all chains will be used for calculation.}
\value{
Correlation matrix.
}
\note{If the MCMC simulation has an adaptive phase it will not be possible to make inference using values 
    sampled before the end of this phase.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{univar}

