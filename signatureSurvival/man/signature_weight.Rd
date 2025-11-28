\name{signature_weight}
\alias{signature_weight}
\docType{data}
\title{
Weights of genes in a signature
}
\description{
Weights of genes in a signature were estimated by using the following equation
\deqn{w_i=\frac{log_{10}(p_i)}{\sum_{i=1}^g{log_{10}(p_i)}}}
where \eqn{p_i} is p-value for Ward-test of gene i in survival analysis.
}
\usage{data("signature_weight")}
\format{
  A data frame with 84 observations on the following 2 variables.
  \describe{
    \item{\code{stage3_down}}{a character vector}
    \item{\code{weight.7}}{a numeric vector}
  }
}
\details{

signature_weight is a matrix with g\eqn{\times } 2 or 3 where g is number of
genes in a signature and there are two or three columns for gene id or gene name
and weights.

}

\examples{
data(signature_weight)
}
\keyword{datasets}
