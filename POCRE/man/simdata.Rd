\name{simdata}
\alias{simdata}
\docType{data}
\title{
A Set of Simulated Data with Single Gaussian Response Variable
}
\description{
A simulated data set with 100 observations, each with one response variable and 1,000 covariates.
}
\usage{data("simdata")}
\format{
A data frame with 100 observations on 1001 variables with the first column for the response variable, and the rest for the covariates.
}
\details{
The 1,000 covariates are from 10 blocks of independent variables, with each block consisting 100 autoregressively correlated variables. There are a total of 20 covariates affecting the response variables: \eqn{x_1, \dots, x_{10}}, \eqn{x_{101}, \dots, x_{110}}.
}
\references{
Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{pocrescreen}}, \code{\link{pocrepath}}, \code{\link{pocre}}.
}
\examples{
data(simdata)
}
\keyword{datasets}
