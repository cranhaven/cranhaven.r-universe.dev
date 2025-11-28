\name{Ploth}
\alias{Ploth}
\title{Changes the appearance of cluster dendrogram}

\description{Modifies several aspects of the cluster dendrogram}

\usage{

  Ploth(hclust, labels=hclust[["labels"]], lab.font=1, lab.col=1, col=1,
   pch.cex=1, pch=NA, bg=0, col.edges=FALSE, hang=-1, ...)

}

\arguments{

  \item{hclust}{Hclust object}

  \item{labels}{Labels}

  \item{lab.font}{Label font}

  \item{lab.col}{Label colors}

  \item{col}{Colors of edges and points}

  \item{pch.cex}{Scale of points}

  \item{pch}{Point types}

  \item{bg}{Points backgrounds}

  \item{col.edges}{Colorize edges?}

  \item{hang}{Makes leaves hang, see plot.hclust(); -1 is default here
  whereas 0.1 is default for 'hclust'}

  \item{...}{Further arguments to plot.dendrogram()}

}

\details{

  Changes the appearance of cluster dendrogram. If labels are long, you
  might need to modify the plot margins.

  Please take into account that supplied labels are meant to be in their
  _initial_ order, not in order of their appearance on the dendrogram.

  Ploth() does not change the text size of labels, please use Tctext()
  and Tcoords() for this (and other) purposes.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{

 \code{\link{Tcoords}}

}

\examples{

iris.dist <- dist(iris[, 1:4], method="manhattan")
iris.hclust <- hclust(iris.dist)
Ploth(iris.hclust, col=as.numeric(iris[, 5]), pch=16, col.edges=TRUE, horiz=TRUE,
 leaflab="none")
legend("topleft", fill=1:nlevels(iris[, 5]), legend=levels(iris[, 5]))

Ploth(hclust(UScitiesD, "ward.D2"), labels=abbreviate(attr(UScitiesD, "Labels")),
 lab.col=c(1, rep(2, 9)), lab.font=c(2, rep(1, 9)), hang=0.1)

}

\keyword{hplot}
