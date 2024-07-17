\name{bsStat}
\alias{bsStat}
\title{Summary of Basic Statistics}
\description{Calcluate basic statistics of data.}
\usage{bsStat(y, two = NULL, digits = c(2, 2), use = 'complete.obs', 
  na.rm = TRUE, ...)}
\arguments{
  \item{y }{ input data for summary statistics.}
  \item{two }{ a logical value of whether to report the correlation and summary statistics separately; if \code{NULL } and the number of variables is less than 11, its value will be set to \code{TRUE}.}
  \item{digits }{ digits for the output data, one for correlation coefficients and the other for mean and others; if a single scalar is supplied, it will be used for both.}
  \item{use }{ an argument for correlation coefficient; see \code{cor} for detail.}
  \item{na.rm }{ an argument for \code{mean}, \code{sd}, \code{min}, and \code{max}.}  
  \item{\dots }{ additional arguments to be passed.}
}
\details{
Two set of summary statistics are generated. One is correlation coefficients and the other is mean, minimum, maximum, standard deviation, and number of observations. When \code{two} is unspecified and the number of variables is bigger than ten, the two sets are reported separately; otherwise, it is reported as a single data frame object.
}
\value{A dataframe or list of the summary statistics.}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\examples{
data(daIns)
(sum.daIns <- bsStat(y=daIns, digits=c(3,2)))
}
\keyword{manip}