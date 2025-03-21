\name{samplesDensity}
\alias{samplesDensity}
\title{Plot density estimate or histogram}
\description{This function plots a smoothed kernel density estimate for a variable if it is continuous
    or a histogram if it is discrete.}
\usage{
samplesDensity(node, beg = samplesGetBeg(), end = samplesGetEnd(),
    firstChain = samplesGetFirstChain(),
    lastChain = samplesGetLastChain(), thin = samplesGetThin(),
    plot = TRUE, mfrow = c(3, 2), ask = NULL, ann = TRUE, ...)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{firstChain, lastChain}{Arguments to select a sub group of chains to plot density estimate or histogram for.}
  \item{thin}{to only use every \code{thin}-th value of the stored sample for statistics.}
  \item{plot}{Logical, whether to plot the trace or only return density estimates.
    If \code{TRUE}, density estimates are returned invisibly.}
  \item{mfrow, ask, ann}{Graphical parameters, see \code{\link{par}} for details.
    \code{ask} defaults to \code{TRUE} unless it is plotting into an already opened non-interactive device.
    The \code{ann} parameter is not available in S-PLUS, and will be ignored if it is set.}
  \item{...}{Further graphical parameters as in \code{\link{par}} may also be passed as arguments
     to \code{\link{plotDensity}}.}
}
\details{If the variable of interest is an array, slices of the array can be selected using the notation
    \code{variable[lower0:upper0, lower1:upper1, ...]}.
    A star \sQuote{\code{*}} can be entered as shorthand for all the stored samples.

    If the arguments are left at their defaults the whole sample for all chains will be used for calculation.}
\note{If the MCMC simulation has an adaptive phase it will not be possible to make inference using values
    sampled before the end of this phase.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}

