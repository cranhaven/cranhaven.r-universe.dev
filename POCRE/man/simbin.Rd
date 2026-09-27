\name{simbin}
\alias{simbin}
\docType{data}
\title{
  A Set of Simulated Binomial Data.
}
\description{
  A simulated data set with 100 observations, each with one binary response variable and 1,000 covariates.
}
\usage{data("simbin")}
\format{
  A data frame with 100 observations on 1001 variables with the first column for the response variable, and the rest for the covariates.
}
\details{
  The true covariates are 1, 2, 103, 104, 205, and 206.
}
\references{
  Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Yu-ting Chen, Department of Statistics, Purdue University}
\seealso{
  \code{\link{gps}}
}
\examples{
  data(simbin)
}
\keyword{datasets}
