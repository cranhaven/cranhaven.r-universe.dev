\name{summary}
\alias{summarySet}
\alias{summaryClear}
\alias{summaryStats}
\title{Summary of MCMC simulation}
\description{These functions are used to calculate running means, standard deviations and quantiles.}
\usage{
summarySet(node)
summaryStats(node)
summaryClear(node)
}
\arguments{
  \item{node}{Character vector containing names of a variables in the model.}
}
\details{
\code{summarySet} creates monitor(s) that starts recording the running totals for \code{node}.

\code{summaryStats} displays the running means, standard deviations, 
    and 2.5\%, 50\% (median) and 97.5\% quantiles for \code{node}. 
    Note that these running quantiles are calculated via an approximate algorithm
    and should therefore be used with caution.

\code{summaryClear} removes the monitor(s) calculating running totals for \code{node}.

These functions are less powerful and general than the samples functions (e.g., see \code{\link{samplesSet}}),
but they also require much less storage (an important consideration when many variables and/or long runs are of interest).}
\value{
\code{summaryStats} returns a data frame with columns:
\item{mean}{means}
\item{sd}{standard deviations}
\item{val2.5pc}{0.025 quantiles}
\item{median}{medians}
\item{val97.5pc}{0.975 quantiles}
\item{sample}{sample sizes}
}
\note{Users should ensure their simulation has converged before using these functions. 
Note that if the MCMC simulation has an adaptive phase it will not be possible to make 
inference using values sampled before the end of this phase.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
