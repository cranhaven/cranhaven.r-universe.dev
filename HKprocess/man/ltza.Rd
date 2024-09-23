\name{ltza}

\alias{ltza}

\title{
Value of quadratic forms for the inverse of a symmetric positive definite
autocorrelation matrix.
}

\description{
The function ltza is used to calculate the value of quadratic forms for the
inverse of a symmetric positive definite autocorrelation matrix, using the
Levinson algorithm (Golub and Van Loan 1996, Algorithm 4.7.2).
}

\usage{
ltza(r, x)
}

\arguments{
  \item{r}{autocorelation vector}
  \item{x}{time series data}
}

\value{
Vector with values t(x) * solve(R) * x, t(en) * solve(R) * x,
t(en) * solve(R) * en and the natural logarithm of the determinant of R. t(.)
denotes the transpose of a vector, en = (1,1,...,1) and R is the autocorrelation
matrix.
}

\author{Hristos Tyralis}

\references{
Golub GH, Van Loan CF (1996) \emph{Matrix Computations}. Baltimore: John
Hopkins University Press.
}

\examples{
# Estimate the parameters for the Nile time series.

r <- acfHKp(H = 0.8, maxlag = length(Nile)-1)

examp <- ltza(r, Nile)

# Comparison of the algorithm with typical approaches

examp[1] - as.numeric(t(Nile) \%*\% solve(toeplitz(r)) \%*\% Nile)

examp[2] - as.numeric(t(rep(1, length(r))) \%*\% solve(toeplitz(r)) \%*\% Nile)

examp[3] - as.numeric(t(rep(1, length(r))) \%*\% solve(toeplitz(r)) \%*\%
    rep(1,length(r)))

examp[4] - log(det(toeplitz(r)))
}

\keyword{array}