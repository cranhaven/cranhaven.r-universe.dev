\name{D_continuous.margin.mean_mean}
\alias{D_continuous.margin.mean_mean}
\alias{D_continuous.margin.mean_mean}
\alias{D_continuous.margin.mean_var}
\alias{D_continuous.margin.mean_alpha}
\alias{D_continuous.margin.var_mean}
\alias{D_continuous.margin.var_var}
\alias{D_continuous.margin.var_alpha}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculate derivatives of marginal effects for continuous variables.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates derivatives of marginal effects with respect to 
the estimated parameters for variables that are treated as continuous. Required
to calculate standard errors of marginal effects.
}
\usage{
D_continuous.margin.mean_mean(whichMargins, whichXest, X, paramvec, etas,
                              link, std.dev)
                              
D_continuous.margin.mean_var(Z, paramvec, etas, link, std.dev, gstd.dev)

D_continuous.margin.mean_alpha(estThresh, outcomematrix, paramvec, etas,
                               link,std.dev)
                               
D_continuous.margin.var_mean(X, paramvec, etas, link,std.dev, gstd.dev)

D_continuous.margin.var_var(whichMargins, whichZest, Z, paramvec, etas,
                            link, std.dev, gstd.dev, hstd.dev)
                            
D_continuous.margin.var_alpha(estThresh, outcomematrix, paramvec, etas,
                              link, std.dev, gstd.dev)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{whichMargins}{Numeric vector indicating indexes of parameters in the relevant matrix for which margins are desired. 
%%     ~~Describe \code{x} here~~
}
  \item{whichXest}{Logical vector indicating the variables in X for which the relevant parameters were estimated.}
  \item{X}{Data matrix containing variables in mean equation.}
  \item{paramvec}{Coefficients related to variables for which marginal effects are desired.}
  \item{etas}{Inputs to link functions.}
  \item{link}{specifies the link function for the estimated model.}
  \item{std.dev}{The calculated standard deviation of the error terms.}
  \item{Z}{Data matrix containing variables in variance equation.}
  \item{whichZest}{Logical vector indicating the variables in Z for which the relevant parameters were estimated.}
  \item{gstd.dev}{The calculated derivative of the standard deviation of the error terms.}
  \item{hstd.dev}{The calculated second derivative of the standard deviation of the error terms.}
  \item{estThresh}{Logical vector indicating which threshold parameters were estimated.}
  \item{outcomematrix}{A matrix that indicates the outcome variable.}
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
