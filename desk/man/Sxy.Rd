\name{Sxy}
\alias{Sxy}
\title{Variation and Covariation}
\usage{
Sxy(x, y = x, na.rm = FALSE)
}
\arguments{
\item{x}{vector of one variable.}

\item{y}{vector of another variable (optional). If specified then the covariation of \code{x} and \code{y} is calculated. If omitted then the variation of \code{x} is calculated.}

\item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
}
\value{
The variaion of \code{x} or the covariation of \code{x} and \code{y}.
}
\description{
Calculates the variation of one variable or the covariation of two different variables.
}
\examples{
x = c(1, 2)
y = c(4, 1)
Sxy(x) # variation
Sxy(x, y) # covariation

## Second example illustrating the na.rm option
x = c(1, 2, NA, 4)
Sxy(x)
Sxy(x, na.rm = TRUE)

}
\concept{covariance}
\concept{covariation}
\concept{variance}
\concept{variation}
