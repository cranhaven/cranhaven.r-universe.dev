\name{sim5ydata}
\alias{sim5ydata}
\docType{data}
\title{
A Set of Simulated Data with Multiple Response Variables
}
\description{
A simulated data set with 100 observations, each with five response variable and 1,000 covariates.
}
\usage{data("sim5ydata")}
\format{
A data frame with 100 observations on 1005 variables with the first five columns for the response variables, and the rest for the covariates.
}
\details{
The 1,000 covariates are from 10 blocks of independent variables, with each block consisting 100 autoregressively correlated variables. There are a total of 12 covariates affecting the response variables: \eqn{x_{50}, x_{51}, x_{150}, x_{153}, x_{250}, x_{256}, x_{350}, x_{359}, x_{450}, x_{467}, x_{550}, x_{583}}.
}
\references{
Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{pocrescreen}}, \code{\link{pocrepath}}, \code{\link{pocre}}, \code{\link{cvpocre}}.
}
\examples{
data(sim5ydata)
}
\keyword{datasets}
