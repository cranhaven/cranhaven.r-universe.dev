\name{logLik.oglmx}
\alias{logLik.oglmx}
\alias{logLik.summary.oglmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract log likelihood value
}
\description{
Return the log likelihood value for objects of class \code{oglmx} and \code{summary.oglmx}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{logLik}{oglmx}(object, ... )

\method{logLik}{summary.oglmx}(object, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    object of class \code{oglmx} or \code{summary.oglmx}.
%%     ~~Describe \code{object} here~~
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
  A single numeric value, the log likelihood for the estimated model.
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
  Carroll, Nathan \email{nathan.carroll@ur.de}
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{logLik}}, \code{\link{oglmx}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (object, ...) 
%{
%    object$loglikelihood[1]
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
