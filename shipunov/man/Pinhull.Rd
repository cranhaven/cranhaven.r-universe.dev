\name{Pinhull}
\alias{Pinhull}
\title{Point in hull}
\description{For each observation, returns if it is within a polygon}

\usage{

Pinhull(pts, ppts)

}

\arguments{

\item{pts}{Data points, 2-dimensional}

\item{ppts}{List with polygon information (e.g., output from Hulls() or
Ellipses())}

}

\details{

For each 'pts' observation, Pinhull() uses PBSmapping::findPolys() to
find if it is within (or on the border) of each polygon described in
'ppts'.

The output or Pinhull is easy to use to calculate the "observation
overlap", it also allows to reveal "outliers" (points outside all
polygons) and all polygon membership features (e.g., which points belong
to more than one polygon).

}

\value{

Logical matrix, each column is the hull (polygon) name, rows correspond
with rows of data points.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}, \code{\link{Ellipses}}, \code{\link{Overlap}}}

\examples{

iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.h <- Hulls(iris.p, iris$Species, plot=FALSE)
iris.e <- Ellipses(iris.p, iris$Species, plot=FALSE)

## convex hulls
iris.pih <- Pinhull(iris.p, iris.h)

## confidence ellipses
iris.pie <- Pinhull(iris.p, iris.e)
## membership overlap
dist(t(iris.pie), method="binary")
## how to find outliers (points outside of all ellipses)
which(apply(iris.pie, 1, sum) == 0) # outliers
## how to make membership table
iris.pie.g <- cbind(iris.pie, group=Alldups(iris.pie, groups=TRUE))
key <- iris.pie.g[!duplicated(iris.pie), ]
key <- key[order(key[, "group"]), ]
mem <- aggregate(1:nrow(iris.p), list(group=iris.pie.g[, "group"]), paste0, collapse=", ")
mem <- cbind(key, mem)
mem[, mem \%-\% "group"] # all memberships

## distance based on membership intersection, Overlap() analog
dist(t(iris.pie), method="binary") # asymmetric binary
SM.dist(t(iris.pie)) # symmetric binary

## uniqueness of species
lapply(1:3, function(.x) sum(rowSums(iris.pie[as.numeric(iris$Species) == .x,
 ]) > 1)/table(iris$Species)[.x]) ## versicolor is least unique

}

\keyword{multivariate}
