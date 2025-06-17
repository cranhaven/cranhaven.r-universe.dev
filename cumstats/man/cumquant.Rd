\name{cumquant}
\alias{cumquant}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cumulative Quantile
}
\description{
Returns a vector whose elements are the cumulative \emph{quantile} of the elements of the argument.
}
\usage{
cumquant(x, p, type = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric vector.
}
  \item{p}{
probability for the desired quantile.
}
  \item{type}{
See \code{quantile} in R base package.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A numeric vector of the same length as \code{x}. An \code{NA} value in \code{x} causes the corresponding and following elements of the return value to be \code{NA}.
}
\references{
Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006) \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
}
\author{
Arturo Erdely.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cummedian}}
}
\examples{
y <- c(9, 1, 3, 0, NA, 2, 5)
cummedian(y)
cumquant(y, 0.5)

z <- cumquant(rcauchy(10000), 0.75)
head(z); tail(z)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~univar }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
