\name{NiceTreePlot}
\alias{NiceTreePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots conditional inference trees.
}
\description{
Plots a \code{partykit} conditional inference tree in a pretty and simple way.
}
\usage{
NiceTreePlot(ct, inner_plots = FALSE, cex = 0.8, justmin = 15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ct}{A tree of class \code{constparty} (as returned by \code{ctree} from \code{partykit} package).}
  \item{inner_plots}{Logical. If TRUE, plots are displayed at each inner node. Default is FALSE.}
  \item{cex}{Numerical value. Multiplier applied to fontsize. Default is 0.8.}
  \item{justmin}{Numerical value. Minimum average edge label length to employ justification (see \code{panelfunctions} documentation from \code{partykit} package)}
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
  NiceTreePlot(iris.ct, inner_plots = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line