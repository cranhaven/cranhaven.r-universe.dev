\name{cubic}
\alias{cubic}
\title{
Function that can resolve the cubic equation numerical stable and any lower dimensional case except unsolvable cases.
%%  ~~function to do ... ~~
}
\description{
A*x^3+B*x^2+C*x+D=0. All coefficients had to be numeric or integers.
This function calculates from 4 coefficient all possible and senfully solutions. D=0 returns no values at all. This would be a impossibel case. It returns upto 3 potential complex solutions.
Less solutions are copied to get the tripple solution format.
}
\usage{
cubic(A, B, C, D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
  The coefficient of x^3.
}
  \item{B}{
   The coefficient of x^2.
}
  \item{C}{
   The coefficient of X.
}
  \item{D}{
   The constant.
}
}
\details{
This function is called by haplotypeit. The results are returned as vector of the three possible solutions: output[1],output[2],output[3].
Further data for checks of the roots.  p,q and the discriminat. 10 and 11 are only usable for symmetry checks.
}
\value{
Returns \code{cubic}(A,B,C,D)[c(1:3)] roots of the at most cubic equation.
}
\references{
Cardans formular as in e.g. The Mathematical Gazette (1993); 77 (Nov, No 480), 354-359 (jstor)
http://www.nickalls.org/dick/papers/maths/cubic1993.pdf
or any other book for algebraic solutions.
See also : http://de.wikipedia.org/wiki/Cardanische_Formeln
and http://en.wikipedia.org/wiki/Cubic_equation
}
\author{
Jan Wolfertz
}
\note{
Using cardenian formular, a well known method. 
}
\seealso{
haplotypeit,callhaplotype
}
\examples{
cubic(1,0,0,-1)[c(1:3)]
cubic(1,1,0,0)[c(1:3)]
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cubic}
\keyword{functions}
