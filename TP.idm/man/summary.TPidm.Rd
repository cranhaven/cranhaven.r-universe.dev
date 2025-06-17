\name{summary.TPidm}
\alias{summary.TPidm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Summary method for a TPidm object
}
\description{
  Summary method for an object of class \sQuote{TPidm}.
}
\usage{
\method{summary}{TPidm}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
     An object of class \sQuote{TPidm}.
}
  \item{\dots}{
     Further arguments to print.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  A data.frame is returned with the following variables:
  
  \item{transition }{The initial and final states \dQuote{i j} for each particular transition.}
  \item{probs }{The transition probabilities \eqn{P_{ij}}{P_{ij}}(s,t).}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}
\note{
  The summary function prints the method used, the user-supplied times s and t, the possible transitions, the estimated transition probabilities and, optionally, confidence limits and variances.
  
    If \code{cov} is specified, the summary function provides the transition probabilities \eqn{P_{ij}}{P_{ij}}(s,t) for each level of covariate.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
data(colonTP)
# create a TPidm object with s = 365, t = 1095, cov = "rx",
# CI = FALSE and method = "NM" (default):
nm365cov <- TPidm(colonTP, s = 365, t = 1095, cov = "rx", CI=FALSE)
summary(nm365cov) # summarizing the results
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
