\name{toy}
\Rdversion{1.1}
\alias{toy}
\docType{data}

\title{Toy dataset}

\description{
Synthetically produced data are provided to test the FuzzyHRT algorithm.
}

\usage{data(toy)}

\format{
 The \code{toy} data is a \code{data.frame} object consisting of 9265 synthetic records and 5 variables:
 \describe{
  \item{\code{strata}}{a numeric value of the strata}
  \item{\code{unit_id}}{a character string with the record ID}
  \item{\code{master_varname}}{a character string with the names of the variables}
  \item{\code{current_value_num}}{a numeric value with the current synthetic observation}
  \item{\code{pred_value}}{a numeric value with the previously synthetic observation}
 }
}

\examples{
# Load the package
library(HRTnomaly)
set.seed(2025L)
# Load the toy dataset
data(toy)
# Detect cellwise outliers
res <- fuzzyHRT(toy)
}

\keyword{dataset}
