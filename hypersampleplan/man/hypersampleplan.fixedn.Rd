\name{hypersampleplan.fixedn}
\alias{hypersampleplan.fixedn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Attribute sampling plans when the sample size n is known and fixed
%%  ~~function to do ... ~~
}
\description{
Attribute sampling is an important field of statistical quality control. When a lot is submitted for inspection, a sampling plan must specify both the number of samples to be drawn from the lot as well as the acceptance number which is the maximum number of defective items found in the sample that would still make the lot acceptable. Since the sample is not free of defective, there are probabilities of accepting a lot which is actually not acceptable and of rejecting one which is acceptable. The hypergeometric distribution using Chebyshev Polynomials forms the basis for calculating those exact probabilities.

This program is designed to compute the acceptance number c when the sample size n is known and fixed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hypersampleplan.fixedn(a, b, k1, k2, n, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ Producer's risk alpha: the probability of rejecting a lot for proportion defective
%%     ~~Describe \code{a} here~~
}
  \item{b}{ Consumer's risk beta: the probability of accepting a lot for proportion defective
%%     ~~Describe \code{b} here~~
}
  \item{k1}{ Number of defective units in a satisfactory quality level;
%%     ~~Describe \code{k1} here~~
}
  \item{k2}{ Number of defective units in an unsatisfactory quality level;
%%     ~~Describe \code{k2} here~~
}
  \item{n}{ Fixed sample size
%%     ~~Describe \code{n} here~~
}
  \item{N}{ Lot Size
%%     ~~Describe \code{N} here~~
}
}

\value{
a matrix that contain the acceptance number c (number of defective units allowed in a lot which is accepted) in the row of sampling.plan.X.
}
\references{
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.

Odeh, R. (1983). Attribute sampling plans, tables of tests and confidence limits for proportions (Vol. 49). CRC Press.

%% ~put references to the literature/web site here ~
}

\seealso{
hypersampleplan
}
\examples{
# For the attribute sampling plan with fixed sample size n
hypersampleplan.fixedn(0.01, 0.10, 40, 80, 307, 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hypergeometic distribution}% use one of  RShowDoc("KEYWORDS")
\keyword{Attribute sampling}% __ONLY ONE__ keyword per line
