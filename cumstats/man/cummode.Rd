\name{cummode}
\alias{cummode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cumulative Mode
}
\description{
Returns a list whose elements are the cumulative \emph{statistical mode(s)} of the elements of the argument.
}
\usage{
cummode(x)
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
A list of the same length as \code{x} with numeric vectors. \code{NA} values are also counted.
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
\code{\link{Mode}}
}
\examples{
cummode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), 55))

cummode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), rep(NA, 7), 55))

cummode(runif(5))

cummode(c(rep("a", 2), rep("b", 5), rep("d", 3), rep("e", 5), rep(NA, 5)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~univar }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
