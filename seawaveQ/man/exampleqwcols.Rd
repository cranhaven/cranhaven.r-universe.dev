\name{exampleqwcols}
\docType{data}
\alias{exampleqwcols}
\title{Example data indicators.}
\description{
  This is an example of the character vector used to indicate which 
  columns represent qualification codes and which represent water-quality 
  concentration data.  It is provided here for use with examples of the 
  internal functions.
}
\usage{exampleqwcols}
\format{
  A numeric vector of 115 observations.
}
\examples{
  data(swData)
  exampleqwcols
}
\seealso{
  \code{\link{prepData}}
  \code{\link{fitMod}}
}
\source{
Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}


