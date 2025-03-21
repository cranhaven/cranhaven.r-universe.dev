\name{ranks}
\alias{ranksSet}
\alias{ranksStats}
\alias{ranksClear}
\title{Calculation of ranks}
\description{These functions are used to calculate ranks of vector valued quantities in the model.}
\usage{
ranksSet(node)
ranksStats(node)
ranksClear(node)
}
\arguments{
  \item{node}{Character, name of a vector (one dimensional array) variable in the model.}
}
\details{
\code{ranksSet} creates a monitor that starts building running histograms to represent 
the rank of each component of \code{node}. 
An amount of storage proportional to the square of the number of components of \code{node} is allocated. 
Even for large numbers of components this can require less storage than calculating the ranks 
explicitly in the model specification and storing their samples, and it is also much quicker.

\code{ranksStats} displays summarises of the distribution of the ranks of each component of \code{node}.

\code{ranksClear} removes the monitor calculating running histograms for \code{node}.
}
\value{
\code{ranksStats} returns a data frame with columns:
\item{val2.5pc}{0.025 quantiles}
\item{median}{medians}
\item{val97.5pc}{0.975 quantiles}
}
\note{Users should ensure their simulation has converged before using these functions. 
Note that if the MCMC simulation has an adaptive phase it will not be possible to make 
inference using values sampled before the end of this phase.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
