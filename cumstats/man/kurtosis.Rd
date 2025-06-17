\name{kurtosis}
\alias{kurtosis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pearson's Measure of Kurtosis
}
\description{
This function computes the estimator of Pearson's measure of \emph{kurtosis}.
}
\usage{
kurtosis(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric vector.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A numeric value of skewness. Returns \code{NA} if \code{x} contains \code{NA} value(s), and \code{NaN} if \code{length(unique(x))==1} is \code{TRUE}.
}
\references{
Komsta, L. and Novomestky, F. (2015). \emph{moments: Moments, cumulants, skewness, kurtosis and related tests.} R package version 0.14. \url{https://CRAN.R-project.org/package=moments}
}
\author{
Adapted by Arturo Erdely from \code{moments} R package by Lukasz Komsta.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cumkurt}}
}
\examples{
kurtosis(c(9, 1, 3, 0))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~univar }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
