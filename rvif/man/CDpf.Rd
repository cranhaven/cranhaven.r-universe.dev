\encoding{utf8}
\name{CDpf}
\alias{CDpf}
\docType{data}
\title{Cobb-Douglas data}
\description{
Data used in Example 2 of Salmerón, García and García (2024) (subsection 4.2) on data for the Cobb-Douglas production function.
}
\usage{data("CDpf")}
\format{
  A data frame containing 28 observations on the following 4 variables:
  \describe{
    \item{\code{P}}{Production (dependent variable).}
    \item{\code{cte}}{Intercept.}
    \item{\code{logK}}{Capital (in logarithm).}
    \item{\code{logW}}{Work (in logarithm).}
  }
}
\details{
This dataset was originally used by Olva Maldonado (2009).
}
\references{
Olva Maldonado, H. (2009). Análisis de la función de producción Cobb-Douglas y su aplicación en el sector productivo mexicano.
Tesis, Universidad Autónoma de Chapingo.

Salmerón, R., García, C.B. and García, J. (2025). A redefined Variance Inflation Factor: overcoming the 
limitations of the Variance Inflation Factor. Computational Economics, 65, 337-363, doi: https://doi.org/10.1007/s10614-024-10575-8.
}
\examples{
  head(CDpf, n=5)
  y = CDpf[,1]
  x = CDpf[,2:4]  
  multicollinearity(y, x)
}
\keyword{datasets}
