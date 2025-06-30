\name{examplecentmp}
\docType{data}
\alias{examplecentmp}
\title{Example logical vector.}
\description{
  This is an example of data that is passed 
  internally to subfunctions of \code{\link{fitswavecav}}. This logical
  vector indicates which water-quality values are censored. It is provided
  here for use with examples of the internal functions.
}
\usage{examplecentmp}
\format{
  A logical vector of 115 observations.
}
\examples{
data(swData)
examplecentmp
}
\source{Internal data captured from the following function call:
\preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}


