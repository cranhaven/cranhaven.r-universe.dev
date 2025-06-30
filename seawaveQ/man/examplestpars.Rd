\name{examplestpars}
\docType{data}
\alias{examplestpars}
\title{Example matrix for internal use.}
\description{
  This is an example of data that is passed 
  internally to subfunctions of \code{\link{fitswavecav}}.  It is provided
  here for use with examples of the internal functions.
}
\usage{examplestpars}
\format{
  A numeric matrix of two rows and 15 columns.\cr
   column  description\cr
   1  mclass, model class, 1 for linear models, 2 for those with restricted cubic splines\cr
   2  model chosen (a number 1-56), this number represents both the pulse input function and the half-life\cr
   3  is the scale factor from the \code{survreg.object}\cr
   4  is the likelihood for the model chosen\cr
   5  is the coefficient for the model intercept \cr
   6  is the coefficient for the seasonal wave component of the model \cr
   7  is the coefficient for the trend component of the model \cr
   8  is the coefficient for the 30-day flow anomaly \cr
   9  is the coefficient for the 1-day flow anomaly \cr
   10 is the standard error for the intercept term  \cr
   11 is the standard error for the seasonal wave term  \cr
   12 is the standard error for the trend term  \cr
   13 is the standard error for the 30-day flow anomaly term  \cr
   14 is the standard error for the 1-day flow anomaly term  \cr
   15 is cmaxt, the decimal season of maximum concentration \cr
}
\seealso{
  \code{\link{fitswavecav}}
}
\examples{
  data(swData)
  examplestpars
}
\source{
  Internal data captured from the following function call:
 \preformatted{fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexample", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))}
}


