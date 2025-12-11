\name{NodesInfo}
\alias{NodesInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Informations about terminal nodes
}
\description{
Retrieves informations about terminal nodes of a conditional inference tree : node id, rule set, frequency, prediction or class probabilities.
}
\usage{
NodesInfo(ct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ct}{A tree of class \code{constparty} (as returned by \code{ctree} from \code{partykit} package).}
}

\value{
A data frame.
}

\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.}

\author{
Nicolas Robette
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{ctree}
}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.ct = partykit::ctree(Species ~ ., data = iris2)
  NodesInfo(iris.ct)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
