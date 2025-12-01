\name{summary.ISEdescription}
\alias{summary.ISEdescription}
\title{Summarise ISE parameters}
\usage{
\method{summary}{ISEdescription}(object, ...)
}
\arguments{
\item{object}{object of class ISEdescription}

\item{...}{Other objects passed through.}
}
\value{
table1: A matrix with parameter values for each ISE
}
\description{
summary.ISEdescription takes an object of class ISEddescription and prints a table of parameter values for
  y = a + b log(x + c) + error, with the erros following a Normal distribution with mean 0 and standard deviation sigma.
  Also calculates LOD using the conditional analytic method (alpha, beta, or S/N).
}
\seealso{
\code{\link{describeISE}}
}
\author{
Peter Dillingham, \email{peter.dillingham@otago.ac.nz}
}
