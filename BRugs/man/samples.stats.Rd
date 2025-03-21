\name{samplesStats}
\alias{samplesStats}
\title{Calculate summary statistics}
\description{This function produces summary statistics for a variable, 
    pooling over the chains selected.}
\usage{
samplesStats(node, beg = samplesGetBeg(), end = samplesGetEnd(), 
    firstChain = samplesGetFirstChain(), 
    lastChain = samplesGetLastChain(), thin = samplesGetThin())
}
\arguments{
  \item{node}{Character vector containing names of variables in the model.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{firstChain, lastChain}{Arguments to select a sub group of chains to calculate summary statistics for.}
  \item{thin}{to only use every \code{thin}-th value of the stored sample for statistics.}
}
\details{
    If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    A star \sQuote{\code{*}} can be entered as shorthand for all the stored samples.
    
    If the arguments are left at their defaults the whole sample for all chains will be used for calculation.}
\value{
\code{samples.stats} returns a data frame with columns:
\item{mean}{means}
\item{sd}{standard deviations}
\item{MC_error}{Estimate of \eqn{s/\sqrt(N)}{s/sqrt(N)},
    the Monte Carlo standard error of the mean. 
    The batch means method outlined by Roberts (1996; p.50) is used to estimate \eqn{s}.}
\item{val2.5pc}{0.025 quantiles}
\item{median}{medians}
\item{val97.5pc}{0.975 quantiles}
\item{start}{\code{beg} + 1}
\item{sample}{sample sizes}
}
\note{If the MCMC simulation has an adaptive phase it will not be possible to make inference using values 
    sampled before the end of this phase.}
\references{Roberts, G.O. (1996): Markov Chain Concepts Related to Sampling Algorithms. 
    In: W.R. Gilks, S. Richardson and D.J. Spiegelhalter (Eds.):
    \emph{Markov Chain Monte Carlo in Practice}. Chapman and Hall, London, UK.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{univar}
\concept{WinBUGS}
\concept{OpenBUGS}
\concept{chain}
\concept{update}
\concept{iteration}
