\name{D_discrete.margin_meanonly.mean}
\alias{D_discrete.margin_meanonly.mean}
\alias{D_discrete.margin_mean.var}
\alias{D_discrete.margin_mean.alpha}
\alias{D_discrete.margin_varonly.var}
\alias{D_discrete.margin_var.alpha}
\alias{D_discrete.margin_var.mean}
\alias{D_discrete.margin_meanvar.mean}
\alias{D_discrete.margin_meanvar.var}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculate derivatives of marginal effects for binary variables.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates derivatives of marginal effects with respect to 
the estimated parameters for binary variables. Required
to calculate standard errors of marginal effects.

}
\usage{
D_discrete.margin_meanonly.mean(whichVars, whichXest, X, fouretas, link, std.dev)

D_discrete.margin_mean.var(whichZest, Z, fouretas, link, std.dev, gstd.dev)

D_discrete.margin_mean.alpha(estThresh, outcomematrix, fouretas, std.dev, link)

D_discrete.margin_var.mean(whichXest, X, fouretas, link, StdDevs)

D_discrete.margin_varonly.var(whichVars, whichZest, Z,fouretas, ZDinputs, link, 
                              StdDevs, gsdmodel)

D_discrete.margin_var.alpha(estThresh, outcomematrix, fouretas, StdDevs, link)

D_discrete.margin_meanvar.mean(whichXest, X, BothEqLocs, fouretas, StdDevs, link)

D_discrete.margin_meanvar.var(whichZest, Z, BothEqLocs, fouretas, ZDinputs, link,
                              StdDevs,gsdmodel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{whichVars}{Numeric vector stating indexes of variables that are binary and marginal effects are desired.}
  \item{whichXest}{Logical vector indicating the variables in X for which the relevant parameters were estimated.}
  \item{X}{Data matrix containing variables in mean equation.}
  \item{fouretas}{Inputs to link functions.}
  \item{link}{specifies the link function for the estimated model.}
  \item{std.dev}{The calculated standard deviation of the error terms.}
  \item{Z}{Data matrix containing variables in variance equation.}
  \item{whichZest}{Logical vector indicating the variables in Z for which the relevant parameters were estimated.}
  \item{gstd.dev}{The calculated derivative of the standard deviation of the error terms.}
  \item{estThresh}{Logical vector indicating which threshold parameters were estimated.}
  \item{outcomematrix}{A matrix that indicates the outcome variable.}
  \item{ZDinputs}{Values of inputs to function that gives standard deviation when binary variable is equal to 0 and 1.}
  \item{StdDevs}{Values of standard deviation when binary variable is equal to 0 and 1.}
  \item{gsdmodel}{Expression used to calculate derivative of standard deviation.}
  \item{BothEqLocs}{Dataframe describing locations of binary variables that are in both the mean and variance equations.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Numeric matrix of derivatives of marginal effects with respect to estimated parameters.
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
\code{\link{margins.oglmx}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
