\name{formula.oglmx}
\alias{formula.oglmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Obtain model formula for an \code{oglmx} object.
%%  ~~function to do ... ~~
}
\description{
  Given an object of class \code{oglmx} the function describes the estimated model via an expression of class \code{\link{formula}}. The function serves to provide a name of a model to the \code{lrtest} function in the \code{lmtest} package.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{formula}{oglmx}(x, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    object of class \code{oglmx}.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
  additional arguments, currently ignored.
%%     ~~Describe \code{\dots} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  an object of class \code{formula}.
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
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{oglmx}}, code{\link[lmtest]{lrtest}}, code{\link{formula}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (x, ...) 
%{
%    if (is.null(x$formula[[2]])) {
%        value <- x$formula[[1]]
%    }
%    else {
%        meannames <- names(attr(terms(x)[[1]], "dataClasses"))
%        varnames <- attr(terms(x)[[2]], "term.labels")
%        textoutput <- paste(meannames[1], "~", meannames[2])
%        if (length(meannames) > 2) {
%            for (j in 3:length(meannames)) {
%                textoutput <- paste(textoutput, "+", meannames[j])
%            }
%        }
%        textoutput <- paste(textoutput, "|", varnames[1])
%        if (length(varnames) > 1) {
%            for (j in 2:length(varnames)) {
%                textoutput <- paste(textoutput, "+", varnames[j])
%            }
%        }
%        value <- formula(textoutput)
%    }
%    return(value)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
