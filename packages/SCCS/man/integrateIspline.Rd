\name{integrateIspline}
\alias{integrateIspline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Integral of I-splines
 %%  ~~function to do ... ~~
}
\description{
Evaluates design matrix for integrals of I-splines and integrals of the integrals. The function evaluates first, second and third integrals of I-splines. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
integrateIspline(x, knots1, m, int)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
      a numeric vector of values at which to evaluate the integrals of I-spline functions %% ~~Describe \code{formula} here~~
}
  \item{knots1}{
     a numeric vector of interior knot positions with non-decreasing values %%     ~~Describe \code{formula} here~~
}
  \item{m}{
     a positive integer giving the order of the spline function. %%     ~~Describe \code{formula} here~~
     }
  \item{int}{a positive integer (1, 2, or 3) for first, second or third integral of an I-spline function.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A matrix with length of (x) rows and length of (knots1) - m columns.
}
\references{
Ghebremichael-Weldeselassie Y. (2014). Smooth risk functions for self-cotrolled case series models. PhD thesis, The Open University.

Ghebremichael-Weldeselassie, Y., Whitaker, H. J., Farrington, C. P. (2015). Spline-based self controlled case series method. Statistics in Medicine 33:639-649.

}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. %%  ~~who you are~~
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{%\code{ispline}
%% ~~objects to See Also as \code{\link{help}}, ~~~}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Splines}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line




