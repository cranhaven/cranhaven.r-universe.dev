\name{samplesBgr}
\alias{samplesBgr}
\title{Plot the Gelman-Rubin convergence statistic}
\description{This function calculates and plots the Gelman-Rubin convergence statistic, 
    as modified by Brooks and Gelman (1998).}
\usage{
samplesBgr(node, beg = samplesGetBeg(), end = samplesGetEnd(), 
    firstChain = samplesGetFirstChain(), 
    lastChain = samplesGetLastChain(), thin = samplesGetThin(),
    bins = 50, plot = TRUE, mfrow = c(3, 2), ask = NULL, 
    ann = TRUE, ...)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{firstChain, lastChain}{Arguments to select a sub group of chains to calculate 
    the Gelman-Rubin convergence statistics for. Number of chains must be larger than one.}
  \item{thin}{Only use every \code{thin}-th value of the stored sample for statistics.}
  \item{bins}{Number of blocks}
  \item{plot}{Logical, whether to plot the BGR statistics or only return the values. 
    If \code{TRUE}, values are returned invisibly.}
  \item{mfrow, ask, ann}{Graphical parameters, see \code{\link{par}} for details.
    \code{ask} defaults to \code{TRUE} unless it is plotting into an already opened non-interactive device.
    The \code{ann} parameter is not available in S-PLUS, and will be ignored if it is set.}
  \item{...}{Further graphical parameters as in \code{\link{par}} may also be passed as arguments 
    to \code{\link{plotBgr}}.}
}
\value{
A list containing data frames - one for each scalar variable contained in 
argument \code{node}. Each data frames contains elements
    \item{Iteration}{end iteration of corresponding bin}
    \item{pooledChain80pct)}{80pct interval (normalized) of pooled chains}
    \item{withinChain80pct}{80pct interval (normalized) of mean within chain}
    \item{bgrRatio}{BGR ratio}
}
\details{
    The width of the central 80\% interval of the pooled runs is green, 
    the average width of the 80\% intervals within the individual runs is blue, 
    and their ratio \eqn{R (= pooled / within)} is red.
    For plotting purposes the pooled and within interval widths are normalised to have an overall maximum of one. 
    The statistics are calculated in bins of length 50: 
    \eqn{R} would generally be expected to be greater than 1 if the starting values are suitably over-dispersed. 
    Brooks and Gelman (1998) emphasise that one should be concerned both with convergence of \eqn{R} to 1, 
    and with convergence of both the pooled and within interval widths to stability.

    If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    A star \sQuote{\code{*}} can be entered as shorthand for all the stored samples.
    
    If the arguments are left at their defaults the whole sample for all chains will be used for calculation.}
\note{If the MCMC simulation has an adaptive phase it will not be possible to make inference using values 
    sampled before the end of this phase.}
\references{Brooks, S.P. and Gelman A. (1998):
    Alternative Methods for Monitoring Convergence of Iterative Simulations. 
    \emph{Journal of Computational and Graphical Statistics}, 7, 434-455.
}
\seealso{\code{\link{plotBgr}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}

