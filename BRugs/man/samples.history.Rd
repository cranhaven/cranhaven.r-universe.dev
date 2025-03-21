\name{samplesHistory}
\alias{samplesHistory}
\title{Trace of a variable}
\description{This function returns and plots a complete trace for a variable.}
\usage{
samplesHistory(node, beg = samplesGetBeg(), end = samplesGetEnd(), 
    firstChain = samplesGetFirstChain(), 
    lastChain = samplesGetLastChain(), thin = samplesGetThin(),
    plot = TRUE, mfrow = c(3, 1), ask = NULL, ann = TRUE, ...)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{firstChain, lastChain}{Arguments to select a sub group of chains to plot the trace for.}
  \item{thin}{to only use every \code{thin}-th value of the stored sample for statistics.}
  \item{plot}{Logical, whether to plot the trace or only return the values. 
    If \code{TRUE}, values are returned invisibly.} 
  \item{mfrow, ask, ann}{Graphical parameters, see \code{\link{par}} for details.
    \code{ask} defaults to \code{TRUE} unless it is plotting into an already opened non-interactive device.
    The \code{ann} parameter is not available in S-PLUS, and will be ignored if it is set.}
  \item{...}{Further graphical parameters as in \code{\link{par}} may also be passed as arguments
     to \code{\link{plotHistory}}.}
}
\value{
A list containing matrices - one for each scalar variable contained in 
argument \code{node}. Each row of a matrix corresponds to one chain.
}
\details{If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    A star \sQuote{\code{*}} can be entered as shorthand for all the stored samples.
    
    If the arguments are left at their defaults the whole sample for all chains will be used for calculation.}
\seealso{\code{\link{plotHistory}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}


