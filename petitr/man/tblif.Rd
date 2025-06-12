\name{tblif}
\alias{tblif}
\docType{data}
\title{
population life table
}
\description{
a population life table with three columns, x, lx, mx (See below)
}
\usage{data(tblif)}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{a numeric vector, age}
    \item{\code{lx}}{a numeric vector, Proportion of survivors at age x}
    \item{\code{mx}}{a numeric vector, mean number of offspring produced in the age class x}
  }
}
\details{
x must represent equal age classes in ascending order
}
\source{
Application of the function \code{\link{xlxmx}} on the data set \code{\link{life}}
}
\examples{
data(tblif)
r(tblif, eps=10e-08)
}
\keyword{datasets}
