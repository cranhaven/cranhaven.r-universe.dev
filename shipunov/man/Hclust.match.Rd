\name{Hclust.match}
\alias{Hclust.match}
\title{Counts matches between two hierarchical clusterings}
\description{Counts matches between two hierarchical clusterings}

\usage{Hclust.match(hc1, hc2, scale=FALSE)}

\arguments{
\item{hc1}{First hclust object}
\item{hc2}{Second hclust object}
\item{scale}{Scale by the sum size of trees?}
}

\details{'Hclust.match()' counts matches between two hierarchical clusterings (based on 'cutree()').

Result is a sort of consensus distances. Useful, for example, for clustering heatmaps.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
aa.d1 <- hclust(dist(t(atmospheres)))
aa.d2 <- hclust(as.dist(1 - abs(cor(atmospheres, method="spearman"))),
 method="ward.D")
aa12.match <- Hclust.match(aa.d1, aa.d2)
heatmap(aa12.match, scale="none")
}

\keyword{cluster}
