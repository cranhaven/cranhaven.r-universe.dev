\name{continuous.margin.mean}
\alias{continuous.margin.mean}
\alias{continuous.margin.sd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate marginal effects for continuous variables.
}
\description{
Calculate marginal effects for continuous variables. Functions calculate for variables in the mean equation and in the variance equation, for a variable in both equations the effects should be summed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
continuous.margin.mean(paramvec,etas,link,std.dev)

continuous.margin.sd(paramvec,etas,link,std.dev,gstd.dev)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paramvec}{Coefficients related to variables for which marginal effects are desired.}
  \item{etas}{Inputs to link functions.}
  \item{link}{specifies the link function for the estimated model.}
  \item{std.dev}{The calculated standard deviation of the error terms.}
  \item{gstd.dev}{The calculated derivative of the standard deviation of the error terms.}
}
%}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Numeric vector of marginal effects. 
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
