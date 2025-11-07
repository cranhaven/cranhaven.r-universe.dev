\name{degreesoffreedom}
\alias{degreesoffreedom}
\title{Degrees of Freedom}
\description{This function calculates the degrees of freedom as part of
  the calculation of the Akaike Information Criterion (AIC).}
\usage{
degreesoffreedom(m, statusinfo)
}
\arguments{
  \item{m}{ a boosted multi state model.}
  \item{statusinfo}{ a logical indicating if information on the
    calculation process should be printed.}
}
\details{This function calculates the degress of freedom as part of
  the calculation of the Akaike Information Criterion.}
\value{
A vector of length equal to the number of boosting iterations in the
plugged in model object.
}
\author{Holger Reulen}
\examples{
\dontrun{degreesoffreedom(m, statusinfo)}
}