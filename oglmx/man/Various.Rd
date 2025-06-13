\name{InternalFunctions}
\alias{Probability}
\alias{calcBHHHmatrix}
\alias{calcMEstdErrors}
\alias{calcstartvalues}
\alias{collectmaxLikOutput}
\alias{getThresholds}
\alias{hessian_oglmx}
\alias{loglikelihood.oglmx}
\alias{mergeformulas}
\alias{oglmx.maxlik}
\alias{score_oglmx}
\alias{updateComponents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Various functions not intended for user.
}
\description{
Functions used in the process of estimating parameters and standard errors of ordered generalized linear models.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateComponents(Env,Parameters)

oglmx.maxlik(inputenv,start)

loglikelihood.oglmx(Env)

score_oglmx(Env)

hessian_oglmx(Env)

calcBHHHmatrix(Env)

mergeformulas(formula1,formula2)

calcstartvalues(whichparameter,gfunc,threshvec)

getThresholds(outcomematrix,thresholdvector)

Probability(eta_1,eta_0,link)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Env, inputenv}{environment, typically constructed by the \code{oglmx.fit} function, that contains all relevant information for the optimisation process.}
  \item{Parameters, start}{numeric vector of length equal to the number of estimated parameters.}
  \item{formula1, formula2}{items of class \code{formula}.}
  \item{whichparameter}{logical}
  \item{gfunc}{expression, function used to model the variance}
  \item{threshvec, thresholdvector}{numeric vectors of threshold values}
  \item{outcomematrix}{numeric matrix with binary variables indicating the outcome for each observation}
  \item{eta_1,eta_0}{input values for the link function}
  \item{link}{string value indicating which link function is to be used}
}

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{oglmx}}, \code{\link{getEtas}}
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
