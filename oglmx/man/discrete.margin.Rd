\name{discrete.margin_meanonly}
\alias{discrete.margin_meanonly}
\alias{discrete.margin_varonly}
\alias{discrete.margin_both}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate marginal effects for binary variables.
}
\description{
Calculate marginal effects for binary variables. Functions calculate for variables that are only in the mean equation, only in the variance equation, and variables in both.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
discrete.margin_meanonly(beta, X, whichVars, etas, link, std.dev)

discrete.margin_varonly(delta, Z, whichVars, sdmodel, etas, link, std.dev)

discrete.margin_both(beta, X, delta, Z, BothEqLocs, sdmodel, etas, link, std.dev)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{Coefficients for the mean equation.
%%     ~~Describe \code{x} here~~
}
  \item{X}{Variable values for the mean equation.}
  \item{whichVars}{Numeric vector stating indexes of variables that are binary and marginal effects are desired.}
  \item{etas}{Inputs to link functions.}
  \item{link}{specifies the link function for the estimated model.}
  \item{std.dev}{The calculated standard deviation of the error terms.}
  \item{delta}{Coefficients for the variance equation.
%%     ~~Describe \code{x} here~~
}
  \item{Z}{Variable values for the variance equation.}
  \item{sdmodel}{Expression used to calculate standard deviation.}
  \item{BothEqLocs}{Dataframe describing locations of binary variables that are in both the mean and variance equations.}
}
\value{
  Numeric vector of marginal effects. Has as attributes calculated components that are used to calculate derivatives of marginal effects.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Nathan Carroll, \email{nathan.carroll@ur.de}
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{margins.oglmx}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (x) 
%{
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
