\name{PlotBest.hclust}
\alias{PlotBest.hclust}
\title{Plots dotchart with best clustering method}

\description{Plots dotchart with best clustering method}

\usage{

PlotBest.hclust(dist, clust=c("ward.D", "ward.D2", "single", "complete",
"average", "mcquitty", "median", "centroid"), plot=TRUE)

}

\arguments{
\item{dist}{Distance matrix}
\item{clust}{Clustering method}
\item{plot}{Plot?}
}

\details{

Shows the "best" hierarchical clustering method. Uses cophenetic
correlation.

}

\value{

Numeric vector with correlation values (equal to the number of
clusterings involved)

}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{

PlotBest.hclust(dist(iris[, -5]))

PlotBest.hclust(dist(t(moldino)))

}

\keyword{hplot}
