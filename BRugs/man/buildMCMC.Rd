\name{buildMCMC}
\alias{buildMCMC}
\title{Generating mcmc.list objects for package coda}
\description{This functions reads samples from OpenBUGS and converts the results into an object of class mcmc.list
that can directly be used by package coda for further analysis.}
\usage{
buildMCMC(node, beg = samplesGetBeg(), end = samplesGetEnd(), 
    firstChain = samplesGetFirstChain(), 
    lastChain = samplesGetLastChain(), thin = samplesGetThin())
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{firstChain, lastChain}{Arguments to select a sub group of chains.}
  \item{thin}{To only use every \code{thin}-th value of the stored sample.}
}
\value{
An object of class \code{\link[coda]{mcmc.list}} which is a list containing \code{\link[coda]{mcmc}} objects.
}
\details{If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    A star \sQuote{\code{*}} can be entered as shorthand for all the stored samples.
    
    If the arguments are left at their defaults the whole sample for all chains will be used for calculation.}
\seealso{\code{\link[coda]{mcmc.list}}, \code{\link[coda]{mcmc}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{MCMC}
\concept{OpenBUGS}
\concept{coda}
\concept{sample}


