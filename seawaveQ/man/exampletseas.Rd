\name{exampletseas}
\docType{data}
\alias{exampletseas}
\title{Example numeric vector used internally.}
\description{
  This is an example of data that is passed 
  internally to \code{\link{seawaveQPlots}}.  This numeric
  vector contains decimal seasonal (0-1) values for the water-quality 
  samples. It is provided here for use with examples of the internal 
  functions.
}
\usage{exampletseas}
\format{
  A numeric vector of 115 observations.
}
\examples{
data(swData)
head(exampletseas)
}
\source{Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}

