\name{McFaddensR2.oglmx}
\alias{McFaddensR2.oglmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate McFadden's R-Squared.
}
\description{
Model evaluation methods based on the analogue of squared residuals do not work well when the outcome variable is discrete and ordered. A popular pseudo-R^2 measure due to McFadden (1973) is given by: 
  \deqn{R^2=1-\log{L_{fit}}/\log{L_0}}
    where \eqn{\log{L_{fit}}} is the log-likelihood for the fitted model and \eqn{\log{L_0}} is the log-likelihood from an intercept only model that estimates the probability of each alternative to be the sample average. This function calculates this term for objects of class \code{oglmx}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
McFaddensR2.oglmx(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of type \code{oglmx}
%%     ~~Describe \code{object} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  numeric value between 0 and a theoretical maximum of 1.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}
%
%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (object) 
%{
%    value <- 1 - object$loglikelihood[1]/attr(object$loglikelihood, 
%        "BaselineLL", exact = TRUE)
%    return(value)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
%