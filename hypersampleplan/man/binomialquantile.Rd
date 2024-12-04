\name{binomialquantile}
\alias{binomialquantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation Binomial Quantiles Table using Chebyshev Polynomials
%%  ~~function to do ... ~~
}
\description{
This is an algorithm for exact calculation of Binomial quantiles using Chebyshev polynomials. For a fixed population size n and probability of "success" p, such calculations produce quantile of q.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binomialquantile(q, n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{
  probability, it must be between 0 and 1.
}
  \item{n}{number of observations.
%%     ~~Describe \code{n} here~~
}
  \item{p}{probability of "success"
%%     ~~Describe \code{p} here~~
}
}
\details{
The detailed algorthim can be found:
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
}
\value{the required values of the binomial quantiles for q
}
\references{
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
}

\note{
N can be very large in our algorthim.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# Calculate the hypergeometric quantile for q=0.3, N=20, p=0.4.
binomialquantile(0.3,20,0.4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
