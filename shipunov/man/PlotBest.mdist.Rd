\name{PlotBest.mdist}
\alias{PlotBest.mdist}
\title{Dotchart which reflects the "best" of non-base distances}

\description{Plots dotchart which shows correspondences between data and various non-base distances}

\usage{

PlotBest.mdist(data, distances=c("manhattan", "euclidean", "canberra",
"clark",  "bray", "kulczynski", "jaccard", "gower", "altGower",
"morisita", "horn", "binomial",  "chao", "cao", "mahalanobis",
"cor.pearson", "cor.spearman", "cor.kendall",  "gower_dist",
"simple_match_dist", "daisy.gower", "smirnov"),
binary.only=FALSE)

}

\arguments{
\item{data}{Data frame with values}
\item{distances}{Distances to use}
\item{binary.only}{Use binary only distances?}
}

\details{

Shows the "best" distance method using many non-base distances from
several packages (namely, "cluster", "smirnov" and "vegan" -- but does
not include "mountford" and "raup" as they are very special). Please note
that this is a mere visualization, and numbers are used only to
understand the relative correspondence between raw data and distances.

Uses maximal correlations between multidimensional scaling of distance
object (converted internally to Euclidean) and PCA of data. Both MDS and
PCA use two dimensions.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{PlotBest.dist}}}

\examples{

PlotBest.mdist(iris[, -5])

m1 <- t((moldino > 0) * 1)
PlotBest.mdist(m1, binary.only=TRUE)

}

\keyword{hplot}
