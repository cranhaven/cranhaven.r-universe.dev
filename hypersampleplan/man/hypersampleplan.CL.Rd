\name{hypersampleplan.CL}
\alias{hypersampleplan.CL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Compute upper and lower confidence limits for the number of defective in an attribute sampling plan
%%  ~~function to do ... ~~
}
\description{
Attribute sampling is an important field of statistical quality control. When a lot is submitted for inspection of quality control, a sampling plan must specify both the number of samples to be drawn from the lot as well as the acceptance number which is the maximum number of defective items found in the sample that would still make the lot acceptable. Since the sample is not free of defective, there are probabilities of accepting a lot which is actually not acceptable and of rejecting one which is acceptable. The hypergeometric distribution using Chebyshev Polynomials forms the basis for calculating those exact probabilities.

This algorithm is proposed to compute upper and lower confidence limits for the number of defective in a lot given (n, x,N)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hypersampleplan.CL(n, x, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ sample size
%%     ~~Describe \code{n} here~~
}
  \item{x}{ the observed number of defective items in a random sample
%%     ~~Describe \code{x} here~~
}
  \item{N}{ lot size
%%     ~~Describe \code{N} here~~
}
}

\value{a matrix that provides one-sided confidence limits. For a lower confidence limit the confidence is CL.Lower; for an upper confidence limit the confidence is CL.Upper; for a two-sided limit it is CL.Upper - CL.Lower.

The value of k is interpreted as: For lower confidence limits, one can be (1 - Conf.Limits.1)\% sure that the number of defective units in the lot is at least k1. For upper confidence limits, one can be (Conf.Limits.2)\% sure that the number of defective units in the lot is at most k2.


}
\references{
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.

Odeh, R. (1983). Attribute sampling plans, tables of tests and confidence limits for proportions (Vol. 49). CRC Press.
%% ~put references to the literature/web site here ~
}

\note{
Since the real probability cannot be just equal to 0.95 or 975 and 0.025 or 0.05, the ranges are then taken to allow the output to include all possible values. It can be certainly changed to any ranges without interference with the execution of the program itself except the execution time may be shorter or longer.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# When one observes 12 defective items in a sample of size 160 taken from a lot size 1000,
# it will give the results for the one-sided 97.5\% upper and lower confidence limits for
# the number of defective items in the lot.
hypersampleplan.CL(160, 12, 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hypergeometic distribution}% use one of  RShowDoc("KEYWORDS")
\keyword{Attribute sampling}% __ONLY ONE__ keyword per line
