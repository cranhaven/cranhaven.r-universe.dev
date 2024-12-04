\name{binomialtable}
\alias{binomialtable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation exact Binomial Probabilities Table using Chebyshev Polynomials
%%  ~~function to do ... ~~
}
\description{
This is an algorithm for efficient and exact calculation of Binomial probabilities using Chebyshev polynomials. For a fixed population size n and probability of "success" p, such calculations simultaneously produce distributions for all possible values of the number of "successes" x.
The algorthim calculate the exact probability even for large n, while other algorthims simply use normal approximation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binomialtable(n, p, output = "density")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of observations.
%%     ~~Describe \code{n} here~~
}
  \item{p}{probability of "success"
%%     ~~Describe \code{p} here~~
}
  \item{output}{The output can be 'density', 'distribution' or 'both'. Default output is 'density'
%%     ~~Describe \code{output} here~~
}
}
\details{
The detailed algorthim can be found:
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a matrix containing the required values of the hypergeometric probabilities indexed by the columns x=0,1,..,n.

}
\references{
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
}

\note{
n can be very large in our algorthim.
}


\examples{
# Calculate the binomialtable probabilities for n=10, p=0.4.
binomialtable(10,0.4)
# Calculate the binomialtable distribution for n=10, p=0.4.
binomialtable(10,0.4,output='distribution')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ Binomial Distribution }% use one of  RShowDoc("KEYWORDS")
% \keyword{ }% __ONLY ONE__ keyword per line
