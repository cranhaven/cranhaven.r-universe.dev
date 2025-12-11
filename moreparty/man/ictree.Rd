\name{ictree}
\alias{ictree}

\title{
An interactive app for conditional inference trees
}

\description{
This function launches a shiny app in a web browser in order to build and analyse conditional inference trees.}

\usage{
ictree(treedata = NULL)
}

\arguments{
  \item{treedata}{The data frame to be used in the app. If NULL (default), a module is launched to import data from a file or from the global environment.}
}

\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{ctree-module}}
}

\examples{
if (interactive()) {
ictree(iris)
}
}

\keyword{tree}
