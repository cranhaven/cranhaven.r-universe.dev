\name{armenia}
\alias{armenia}
\docType{data}
\title{
Armenia dataset
}
\description{
Archaeological faunal remains (24 sheep second molars) from Late Bronze Age (1500–1100 BCE) sites in the Tsaghkahovit Plain, Armenia
}
\usage{data("armenia")}
\format{
  A data frame with 223 observations on the following 4 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{Subsample}}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E} \code{F} \code{G} \code{H} \code{I} \code{J}}
    \item{\code{distance}}{a numeric vector}
    \item{\code{oxygen}}{a numeric vector}
  }
}

\source{
H. Chazin, S. Deb, J Falk and A. SRINIVASAN
}
\references{
Chazin, Hannah, Soudeep Deb, Joshua Falk, and Arun Srinivasan. 2019. “New Statistical Approaches to Intra-Individual Isotopic Analysis and Modeling Birth Seasonality in Studies of Herd Animals.” Archaeometry 61 (2): 478–93
}
\examples{
data(armenia)
}
\keyword{datasets}
