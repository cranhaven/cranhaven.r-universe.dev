\encoding{utf8}
\name{employees}
\alias{employees}
\docType{data}
\title{
Spanish company employee data
}
\description{
Data used in example 3 of Salmerón, García and García (2024) (subsection 4.3) on the number of employees of Spanish companies.}
\usage{data("employees")}
\format{
  A data frame with 15 observations on the following 5 variables:
  \describe{
    \item{\code{NE}}{Number of employees (dependent variable).}
    \item{\code{cte}}{Intercept.}
    \item{\code{FA}}{Fixed assets (in euros).}
    \item{\code{OI}}{Operating income (in euros).}
    \item{\code{S}}{Sales (in euros).}
  }
}
\details{
This dataset is originally used by Salmerón, Rodríguez, García and García (2020).
}
\references{
Salmerón, R., Rodríguez, A., García, C.B. and García, J. (2020). The VIF and MSE in raise regression. Mathematics, 8(4), doi: https://doi.org/10.3390/math8040605.

Salmerón, R., García, C.B. and García, J. (2025). A redefined Variance Inflation Factor: overcoming the 
limitations of the Variance Inflation Factor. Computational Economics, 65, 337-363, doi: https://doi.org/10.1007/s10614-024-10575-8.
}
\examples{
  head(employees, n=5)
  y = employees[,1]
  x = employees[,3:5]
  multicollinearity(y, x)
}
\keyword{datasets}
