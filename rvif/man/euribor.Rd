\encoding{utf8}
\name{euribor}
\alias{euribor}
\docType{data}
\title{
Euribor data
}
\description{
Data used in example 1 of Salmerón, García and García (2024) (subsection 4.1) on Euribor data.
}
\usage{data("euribor")}
\format{
  A data frame with 47 observations on the following 5 variables:
  \describe{
    \item{\code{E}}{Euribor (dependent variable, in percentage).}
    \item{\code{cte}}{Intercept.}
    \item{\code{HIPC}}{Harmonized index of consumer prices (in percentage).}
    \item{\code{BC}}{Balance of payments to net current account (millions of euros).}
    \item{\code{GD}}{Goverment deficit to net nonfinancial accounts (millions of euros).}
  }
}
\details{
This dataset is originally used by Salmerón, Rodríguez and García (2020).
}
\references{
Salmerón, R., Rodríguez, A. and García, C.B. (2020). Diagnosis and quantification of 
the non-essential collinearity. Computational Statistics, 35(2), 647-666, doi: https://doi.org/10.1007/s00180-019-00922-x.

Salmerón, R., García, C.B. and García, J. (2025). A redefined Variance Inflation Factor: overcoming the 
limitations of the Variance Inflation Factor. Computational Economics, 65, 337-363, doi: https://doi.org/10.1007/s10614-024-10575-8.
}
\examples{
  head(euribor, n=5)
  y = euribor[,1]
  x = euribor[,2:5]
  multicollinearity(y, x)
}
\keyword{datasets}
