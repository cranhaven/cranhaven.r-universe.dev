\name{plotNaivePrev}
\alias{plotNaivePrev}
\alias{plotSampledPrev}
\title{
Plotting expected
}
\description{
Two functions for plotting the expected distribution of affecteds in a family, either assuming no genetic risk factors, or using sampled from a custom distribution of affecteds (such as produced by \code{\link[Mangrove]{MangroveTree}}).
}
\usage{
plotNaivePrev(ped,K,maxN = NULL,...)
plotSampledPrev(samples, obs_prev, exp_prev, maxN = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ped}{
	A \code{\link[Mangrove]{MangrovePed}} object.
}
  \item{K}{
	The prevalence of the disease.  
}
  \item{samples}{
	Samples from the distribution of number of affecteds.
}
  \item{obs_prev}{
	The number of affecteds actually observed.
}
  \item{exp_prev}{
	The expected number of affecteds under a naive model.
}
  \item{maxN}{
	The maximum number of affecteds to be shown on the graph. If \code{NULL}, an appropriate maximum is selected from the data.
}
  \item{\dots}{
	Additional arguments to \code{plot}.
}
}
\details{
These functions are used to assess how "unusual" a family is in terms of the number of affected individuals it contains. \code{plotNaivePrev} plots the distribution of affected individuals in the family assuming no genetic risk factors (i.e. under a binomial model). \code{plotSampledPrev} is more general, and takes in a set of samples from the expected distribution. 

Note that \code{plotSampledPrev} is called by \code{\link[Mangrove]{plot.MangroveSample}} to plot the results of a MangroveTree sampling. It will be easier to use the \code{print.MangroveSample} method rather than using \code{plotSampledPrev} under most circumstances.
}
\value{
Neither function returns anything.
}

\seealso{
\code{\link[Mangrove]{MangroveSample}}
}
\examples{
data(famped)

plotNaivePrev(famped,0.02) # is this unexpected for a 2% disease?
plotNaivePrev(famped,0.04) # how about for a 4% disease?

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
