\name{caterpillar}
\alias{caterpillar}
\docType{data}
\title{
Pine processionary caterpillar dataset%%   ~~ data name/kind ... ~~
}
\description{
The \code{caterpillar} dataset is extracted from a
1973 study on pine processionary caterpillars.
The response variable is the
log transform of the number of nests per unit.
There are \eqn{p=8}{p=8} potential explanatory variables
and \eqn{n=33}{n=33} areas.
}
\usage{data(caterpillar)}
\format{
  A data frame with 33 observations on the following 9 variables.
  \describe{
    \item{\code{x1}}{altitude (in meters)}
    \item{\code{x2}}{slope (in degrees)}
    \item{\code{x3}}{number of pine trees in the area}
    \item{\code{x4}}{height (in meters) of the tree sampled at the center of the area}
    \item{\code{x5}}{orientation of the area (from 1 if southbound to 2 otherwise)}
    \item{\code{x6}}{height (in meters) of the dominant tree}
    \item{\code{x7}}{number of vegetation strata}
    \item{\code{x8}}{mix settlement index (from 1 if not mixed to 2 if mixed)}
    \item{\code{y}}{logarithmic transform of the average number of nests of caterpillars per tree}
  }
}
\details{
This dataset is used in Chapter 3 on linear regression. It assesses the
influence of some forest settlement characteristics on the development of
caterpillar colonies.  It was first published and studied in
Tomassone et al. (1993).  The response variable is the
logarithmic transform of the average number of nests of caterpillars per tree
in an area of 500 square meters (which corresponds to the last column in
\code{caterpillar}).  There are \eqn{p=8}{p=8} potential explanatory variables
defined on \eqn{n=33}{n=33} areas.
}
\source{
Tomassone, R., Dervin, C., and Masson, J.P. (1993)
\emph{Biometrie: modelisation de phenomenes biologiques}.
Dunod, Paris.%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\examples{
data(caterpillar)
summary(caterpillar)
}
\keyword{datasets}
\keyword{caterpillars}
