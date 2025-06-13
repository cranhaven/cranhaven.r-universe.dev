\name{vcov.oglmx}
\alias{vcov.oglmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Variance-Covariance Matrix for an oglmx Object
}
\description{
Returns the variance-covariance matrix of the estimated parameters of an oglmx object.
}
\usage{
\method{vcov}{oglmx}(object, tol = 1e-20, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
  an object of class "oglmx"
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
  argument passed to qr.solve, defines the tolerance for detecting linear dependencies in the hessian matrix to be inverted.
}
 \item{\dots}{
 further arguments, currently ignored.
 }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A matrix of the estimated covariances between the parameter estimates obtained from inverting the Hessian at the returned parameter values in an oglmx object.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (object, tol = 1e-20) 
%{
%    vcov <- qr.solve(-object$hessian, tol = tol)
%    colnames(vcov) <- rownames(vcov) <- names(object$coefficients)
%    return(vcov)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
