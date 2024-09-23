\name{ltzb}

\alias{ltzb}

\title{
Value of quadratic forms for the inverse of a symmetric positive definite
autocorrelation matrix.
}

\description{
The function ltzb is used to calculate the value of quadratic forms for the
inverse of a symmetric positive definite autocorrelation matrix, using the
Levinson algorithm (Golub and Van Loan 1996, Algorithm 4.7.2).
}

\usage{
ltzb(r, x)
}

\arguments{
  \item{r}{autocorelation vector}
  \item{x}{time series data}
}

\value{
Vector with values t(en) * solve(R) * x and t(en) * solve(R) * en. t(.) denotes
the transpose of a vector, en = (1,1,...,1) and R is the autocorrelation matrix.
}

\author{Hristos Tyralis}

\references{
Golub GH, Van Loan CF (1996) \emph{Matrix Computations}. Baltimore: John
Hopkins University Press.
}

\examples{
# Estimate the parameters for the Nile time series.

r <- acfHKp(H = 0.8,maxlag = length(Nile)-1)

examp <- ltzb(r,Nile)
# Comparison of the algorithm with typical approaches

examp[1] - as.numeric(t(rep(1, length(r))) \%*\% solve(toeplitz(r)) \%*\% Nile)

examp[2] - as.numeric(t(rep(1, length(r))) \%*\% solve(toeplitz(r)) \%*\%
    rep(1,length(r)))
}

\keyword{array}