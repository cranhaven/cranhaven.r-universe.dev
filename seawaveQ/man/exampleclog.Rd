\name{exampleclog}
\docType{data}
\alias{exampleclog}
\title{Example of logarithmically transformed concentration data.}
\description{
  This is an example of data that is used internally by \link{fitMod} and  
  passed to its subfunction \code{\link{seawaveQPlots}}.  This numeric
  vector represents the base-10 logarithm of the water-quality concentrations.  
  It is provided here for use with examples of the internal functions.
}
\usage{exampleclog}
\format{
  A numeric vector of 115 observations.
}
\examples{
data(swData)
exampleclog
}
\source{Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}


