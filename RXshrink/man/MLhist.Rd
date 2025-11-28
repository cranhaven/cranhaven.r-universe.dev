\name{MLhist}
\alias{MLhist}
\title{Plot method for MLboot objects}
\description{Frequency Histogram displays that use both a specified "Middle Percentage"
  of a MLboot() distribution that contains outliers as well as a proposed total number
  of bins.}
\usage{
  MLhist(x, comp="opt.beta", xvar=1, npct = 95, bins = 50 )}
\arguments{
  \item{x}{An Output-list object of class "MLboot".}
  \item{comp}{One of five possible choices of MLboot() resampled estimates: "ols.beta",
    "ols.rmse", "opt.dmse", "opt.beta" or "opt.rmse".} 
  \item{xvar}{Column number of estimates to be displayed: 1 <= xvar <= x$p.}  
  \item{npct}{An integer percentage of the simulated scalar estimates to be displayed.
    This percentage should be at least 66 and at most 100.}
  \item{bins}{This proposed number of histogram "breaks" is only a suggestion;
    actual breakpoints will be set to "pretty" values.}
}
\value{ An output list object of class MLhist:
  \item{x}{Character string showing user choices for "comp" and "xvar" arguments.}
  \item{form}{The regression formula specifying the linear model in MLboot().}	
  \item{reps}{Number of Bootstrap replications performed.}
  \item{npct}{An integer percentage of the simulated scalar estimates displayed.}
  \item{rbins}{Number of histogram Bins requested.}
  \item{dbins}{Number of histogram Bins actually displayed.}
  \item{ntot}{Total number of rows of scalar estimates.}
  \item{p}{Total number of columns of scalar estimates.}
  \item{nlo}{First (smallest) order statistic displayed in Histogram.}
  \item{nup}{Last (largest) order statistic displayed in Histogram.}
  \item{noin}{Total number of scalar estimates displayed in Histogram.}
  \item{xmn}{Observed Mean Estimate: location of vertical "blue" dashed line on Histogram.}
}
\author{Bob Obenchain <wizbob@att.net>}
\keyword{histogram}
