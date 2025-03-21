\name{samplesAutoC}
\alias{samplesAutoC}
\title{Plot autocorrelation function}
\description{This function calculates and plots the autocorrelation function of a variable.}
\usage{
samplesAutoC(node, chain, beg = samplesGetBeg(), 
    end = samplesGetEnd(), thin = samplesGetThin(), plot = TRUE,
    mfrow = c(3, 2), ask = NULL, ann = TRUE, ...)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
  \item{chain}{Selects a chain to plot autocorrelation function for.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{thin}{To only use every \code{thin}-th value of the stored sample for statistics.}
  \item{plot}{Logical, whether to plot the ACF or only return the values. If \code{TRUE}, values are returned invisibly.}
  \item{mfrow, ask, ann}{Graphical parameters, see \code{\link{par}} for details.
    \code{ask} defaults to \code{TRUE} unless it is plotting into an already opened non-interactive device.  
    The \code{ann} parameter is not available in S-PLUS, and will be ignored if it is set.}
  \item{...}{Further graphical parameters as in \code{\link{par}} may also be passed as arguments
     to \code{\link{plotAutoC}}.}
}
\value{
A list containing \code{\link{acf}} objects - one for each scalar variable contained in 
argument \code{node}. See \code{\link{acf}} for details on the list elements.
}
\details{If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    A star \sQuote{\code{*}} can be entered as shorthand for all the stored samples.
    
    If the arguments are left at their defaults the whole sample for all chains will be used for calculation.}
\note{If the MCMC simulation has an adaptive phase it will not be possible to make inference using values 
    sampled before the end of this phase.}
\seealso{\code{\link{plotAutoC}}, \code{\link{acf}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}

