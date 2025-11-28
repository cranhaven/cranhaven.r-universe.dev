\name{Cor.vec}
\alias{Cor.vec}
\title{Calculates correlation and converts results into the named long vector}
\description{Calculates correlation and converts results into the named long vector}

\usage{

Cor.vec(X, ...)

}

\arguments{

\item{X}{Data frame or matrix with values}

\item{...}{Arguments to 'cor()'}

}

\details{

Calculates correlation and converts results into the named long vector
(Rostova, 1999; Rostova, 2002).

}

\value{Named numerical vector of correlations.}

\references{

Rostova N.S. 1999. The variability of correlations systems between the
morphological characters. Part 1. Natural populations of Leucanthemum
vulgare (Asteraceae). Botanicheskij Zhurnal. 84(11): 50--66.

Rostova N.S. 2002. Correlations: Structure and Variability. Saint
Petersburg, St. Petersburg University Publisher.

}

\author{Alexey Shipunov}

\seealso{\code{\link{Rostova.tbl}}}

\examples{

Cor.vec(trees, method="spearman")

}

\keyword{correlation}
