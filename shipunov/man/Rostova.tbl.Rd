\name{Rostova.tbl}
\alias{Rostova.tbl}
\title{Calculates multiple correlation matrices (via 'factor1') and stacks them together}

\description{Calculates multiple correlation matrices (via 'factor1') and stacks them together}

\usage{

Rostova.tbl(X, GROUP, ...)

}

\arguments{

\item{X}{Data frame or matrix with values}

\item{GROUP}{Number of grouping variable}

\item{...}{Arguments to 'Cor.vec()'}

}

\details{

Calculates multiple correlation matrices (via GROUP) and stacks them
together.

Output is suitable for PCA, distance calculations and other multivariate
methods (Rostova, 1999; Rostova, 2002).

}

\value{Data frame with correlation structure}

\references{

Rostova N.S. 1999. The variability of correlations systems between the
morphological characters. Part 1. Natural populations of Leucanthemum
vulgare (Asteraceae). Botanicheskij Zhurnal. 84(11): 50--66.

Rostova N.S. 2002. Correlations: Structure and Variability. Saint
Petersburg, St. Petersburg University Publisher.

}


\author{Alexey Shipunov}

\seealso{\code{\link{Cor.vec}}}

\examples{

Trees <- trees
Trees[, 4] <- sample(letters[1:3], nrow(Trees), replace=TRUE)
(rr <- Rostova.tbl(Trees, 4))
plot(hclust(dist(rr)))

}

\keyword{correlation}
