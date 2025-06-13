\name{AIC.oglmx}
\alias{AIC.oglmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate Akaike Information Criterion
%%  ~~function to do ... ~~
}
\description{
  Calculates the Akaike Information Criterion for objects of class \code{oglmx}. Calculate using the formula \eqn{-2*loglikelihood + k*npar} where \eqn{npar} represents the number of parameters in the model and \eqn{k} is the cost of additional parameters, equal to 2 for the AIC, it is \eqn{k=\log(n)} with \eqn{n} the number of observations for the BIC.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  \method{AIC}{oglmx}(object, ..., k = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  object of class \code{oglmx}
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{
  additional arguments. Currently ignored.
%%     ~~Describe \code{\dots} here~~
}
  \item{k}{
  the penalty per parameter to be used.
%%     ~~Describe \code{k} here~~
}
}
\details{
When comparing models by maximium likelihood estimation the smaller the value of the AIC the better.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 A numeric value with the AIC.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Nathan Carroll, \email{nathan.carroll@ur.de}
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{AIC}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (object, ..., k = 2) 
%{
%    value <- k * length(object$coefficients) - 2 * logLik(object)
%    return(value)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
