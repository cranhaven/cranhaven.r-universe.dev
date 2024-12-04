\name{hypergeotable}
\alias{hypergeotable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation exact Hypergeometric Probabilities Table using Chebyshev Polynomials
%%  ~~function to do ... ~~
}
\description{
This is an algorithm for efficient and exact calculation of hypergeometric probabilities using Chebyshev polynomials. For a fixed population size N and fixed sample size n, such calculations simultaneously produce distributions for all possible values of the population number of "successes" M.

The well-known hypergeometric distribution arises in the combinatorial problem in which a finite population of N ob-jects contains M of one kind which may be labeled "success." A sample of n objects is picked without replacement from this set of N objects and it is wished to find the probability that the sample contains exactly x objects labeled "success", where x = 0,1, ... ,N. The probability of observing exactly x such objects is given by The hypergeometric distribution is used for sampling without replacement:

p(x)=choose(M,x)choose(N-M,n-x)/choose(N,n)

where max(0,n-N+M)<=x<=min(n,M)

This algorthim calculate the exact probability even for large N of n, while other algorthims simply use binomial approximation.



%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hypergeotable(N,n,output='density')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{population size N.
%%     ~~Describe \code{N} here~~
}
  \item{n}{sample size n.
%%     ~~Describe \code{n} here~~
}
  \item{output}{The output can be 'density', 'distribution' or 'both'. Default output is 'density'
%%     ~~Describe \code{n} here~~
}
}
\details{The detailed algorthim can be found:
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a matrix containing in row M=0,1,...,N, the required values of the hypergeometric probabilities(or distribution if output='distribution') indexed by the columns x=0,1,..,n.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.
%% ~put references to the literature/web site here ~
}

\note{
N can be very large say 2000 in our algorthim.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
# Calculate the hypergeometric probabilities for N=10, n=5.
hypergeotable(10,5)
# Calculate the hypergeometric distribution for N=10, n=5.
hypergeotable(10,5,output='distribution')
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
