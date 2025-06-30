\name{exampletyrpr}
\docType{data}
\alias{exampletyrpr}
\title{Example numeric vector used internally.}
\description{
  This is an example of data that is passed 
  internally to \code{\link{seawaveQPlots}}.  This numeric
  vector contains decimal dates for continuous ancillary variables.  
  It is provided here for use with examples of the internal functions.
}
\usage{exampletyrpr}
\format{
  A numeric vector of 2,893 observations.
}
\examples{
data(swData)
head(exampletyrpr)
}
\source{Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}

