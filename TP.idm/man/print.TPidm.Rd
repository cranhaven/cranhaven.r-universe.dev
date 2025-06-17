\name{print.TPidm}
\alias{print.TPidm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print method for a TPidm object
}
\description{
Print method for an object of class \sQuote{TPidm}.
}
\usage{
\method{print}{TPidm}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \sQuote{TPidm}.
}
  \item{\dots}{
Further arguments to  print.
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
  The print function displays the method used, the user-supplied times s and t, the possible transitions, the estimated transition probabilities and, optionally, confidence limits and variances.
  
  If \code{cov} was specified, the print function provides the transition probabilities \eqn{P_{ij}}{P_{ij}}(s,t) for each level of covariate.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
%%\examples{
%%##---- Should be DIRECTLY executable !! ----
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
