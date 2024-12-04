\name{hypergeoquantile}
\alias{hypergeoquantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation Hypergeometric Quantiles Table using Chebyshev Polynomials

}
\description{
This is an algorithm for efficient and exact calculation of hypergeometric quantiles using Chebyshev polynomials. For a fixed population size N and fixed sample size n, such calculations simultaneously produce quantiles of q for all possible values of the population number of "successes" M.
}
\usage{
hypergeoquantile(q, N, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{
  probability, it must be between 0 and 1.
}
  \item{N}{population size N.
%%     ~~Describe \code{N} here~~
}
  \item{n}{sample size n.
%%     ~~Describe \code{n} here~~
}
}
\details{
The detailed algorthim can be found:
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
}
\value{
a matrix containing all possible required values of the hypergeometric quantiles for q in row M=0,1,...,N.

}
\references{
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
}

\note{
N can be very large say 2000 in our algorthim.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
# Calculate the hypergeometric quantile for q=0.05, N=10, n=5.
hypergeoquantile(0.05,10,5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hypergeometric distribution}% use one of  RShowDoc("KEYWORDS")
\keyword{quantile}% __ONLY ONE__ keyword per line
