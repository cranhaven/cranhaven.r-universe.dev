\name{hypersampleplan}
\alias{hypersampleplan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Attribute sampling plans with Hypergeometric Probabilities using Chebyshev Polynomials
%%  ~~function to do ... ~~
}
\description{
Attribute sampling is an important field of statistical quality control. When a lot is submitted for inspection of quality control, a sampling plan must specify both the number of samples to be drawn from the lot as well as the acceptance number which is the maximum number of defective items found in the sample that would still make the lot acceptable. Since the sample is not free of defective, there are probabilities of accepting a lot which is actually not acceptable and of rejecting one which is acceptable. The hypergeometric distribution using Chebyshev Polynomials forms the basis for calculating those exact probabilities.

This algorithm is proposed to generate an online table which displays the values of the sample size and the acceptance number given the values of proportional defective, associated risks and lot size. In this table, one can check the values for sample size and acceptance number according to every possible group of values of risks, proportional defective, and lot size.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hypersampleplan(a, a.prime, b, b.prime, k1, k2, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ Producer's risk alpha: the probability of rejecting a lot for proportion defective
%%     ~~Describe \code{a} here~~
}
  \item{a.prime}{ the lower bound for pre-specified Producer's risk alpha;
%%     ~~Describe \code{a.prime} here~~
}
  \item{b}{ Consumer's risk beta: the probability of accepting a lot for proportion defective
%%     ~~Describe \code{b} here~~
}
  \item{b.prime}{ the lower bound for pre-specified Consumer's risk beta;
%%     ~~Describe \code{b.prime} here~~
}
  \item{k1}{ Number of defective units in a satisfactory quality level;
%%     ~~Describe \code{k1} here~~
}
  \item{k2}{ Number of defective units in an unsatisfactory quality level;
%%     ~~Describe \code{k2} here~~
}
  \item{N}{ Lot Size
%%     ~~Describe \code{N} here~~
}
}

\value{
a matrix that contains the values of the sample size n and acceptance number c (number of defective units allowed in a lot which is accepted).In this table, one can check the values for sample size and acceptance number according to every possible group of values of risks, proportional defective, and lot size.
}
\references{
Alvo, M., & Cabilio, P. (2000). Calculation of hypergeometric probabilities using Chebyshev polynomials. The American Statistician, 54(2), 141-144.

Odeh, R. (1983). Attribute sampling plans, tables of tests and confidence limits for proportions (Vol. 49). CRC Press.
%% ~put references to the literature/web site here ~
}

\note{The Calculateion of Hypergeometric Probabilities involved is using Chebyshev Polynomials which is exact calculation campared to other methods using approximations.

The values for a.prime and b.prime can always set to be zero. However, to save the execution time, it is not suggested to set them too far from the pre-specified a and b. If the ranges are too narrow and no sampling plan is returned, the message “the ranges for alpha and/or beta are too
narrow, please reselect them” will appear. Under this circumstance, one needs to widen the range for a or b or both.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
hypergeotable
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
hypersampleplan(0.01, 0.005, 0.05, 0.04, 20, 40, 400)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hypergeometic distribution}% use one of  RShowDoc("KEYWORDS")
\keyword{Attribute sampling}% __ONLY ONE__ keyword per line
