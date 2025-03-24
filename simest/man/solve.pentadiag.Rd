\name{solve.pentadiag}
\alias{solve.pentadiag}
\docType{data}
\title{
Pentadiagonal Linear Solver.
}
\description{
A function to solve pentadiagonal system of linear equations.
}
\usage{
\method{solve}{pentadiag}(a, b, ...)
}

\arguments{
	\item{a}{a numeric square matrix with pentadiagonal rows. The function does NOT check for pentadiagonal matrix.}
	\item{b}{a numeric vector of the same length as nrows(a). This argument cannot be a matrix.}
	\item{...}{any additional arguments}
}

\details{
This function is written mainly for use in this package. It may not be the most efficient code.
}
\value{
	A vector containing the solution.
}
\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu}
\examples{
A <- matrix(c(2,1,1,0,0,
			  1,2,1,1,0,
			  1,1,2,1,1,
			  0,1,1,2,1,
			  0,0,1,1,2),nrow = 5)
b <- rnorm(5)
tmp <- solve.pentadiag(A, b)
}
\keyword{Pentadiagonal}