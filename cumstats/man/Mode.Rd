\name{Mode}
\alias{Mode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistical Mode
}
\description{
This function computes the statistical \emph{mode} of given data.
}
\usage{
Mode(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric or character vector.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list containing the following components:

  \item{Values }{of statistical mode(s) found, in the order they appear in \code{x}}
	\item{Frequency }{number of times the mode(s) appear in \code{x}}
	
	\code{NA} values are also considered.
}
\references{
Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006) \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
}
\author{
Ian Castillo.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cummode}}
}
\examples{
Mode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), 55))

Mode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), rep(NA, 7), 55))

Mode(runif(5))

Mode(c(rep("a", 2), rep("b", 5), rep("d", 3), rep("e", 5), rep(NA, 5)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~univar }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
