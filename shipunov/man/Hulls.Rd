\name{Hulls}
\alias{Hulls}
\title{Convex hulls for multiple groups}
\description{Calculates and plots groups hulls and related information}

\usage{Hulls(pts, groups, match.colors=TRUE, usecolors=NULL,
 plot=TRUE, centers=FALSE, c.pch=0, c.cex=3,
 outliers=TRUE, coef=1.5, ...)}

\arguments{
\item{pts}{Data points to plot, 2-dimensional}
\item{groups}{Grouping variable, any type}
\item{match.colors}{Match colors with groups}
\item{usecolors}{Which group colors to use (does not rotate)}
\item{plot}{Plot?}
\item{centers}{Show centers?}
\item{c.pch}{Type of center points}
\item{c.cex}{Scale of center points}
\item{outliers}{Include outliers?}
\item{coef}{Determines how to detect outliers, see 'coef' from 'boxplot.stats()' }
\item{...}{Arguments to 'lines()'}
}

\details{

If 'centers=TRUE', Hulls() calculates centroids of polygons corresponding
with convex hulls.

If 'outliers=FALSE', Hulls() uses boxplot.stats() to detect outliers
(points which are most distant from centers). This option could be used
for cluster sharpening. It also automatically switches to 'centers=TRUE'
so if you want to plot smoothed hulls but do not want to plot their
centers, use something like 'c.pch=NA' or 'c.cex=0' (see examples).

Please also check Ellipses() function which uses confidence ellipses
based on F-distribution.

Note that (at least at the moment), polygons are plotted with line()
function, therefore shading is not straightforward (but possible, see
examples).

}

\value{

Invisibly outputs list of hulls (polygons) with coordinates, and possibly
also with 'centers' and 'outliers' attributes. Indices of margin points
returned as row names of each polygon.

See also package 'cluster' for ellipsoidhulls() function that allows to
draw ellipse-like hulls.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Ellipses}}, \code{\link{Overlap}}, \code{\link{boxplot.stats}}}

\examples{
iris.p <- prcomp(iris[, -5])$x[, 1:2]
plot(iris.p, type="n", xlab="PC1", ylab="PC2")
pal <- rainbow(3)
text(iris.p, labels=abbreviate(iris[, 5], 1, method="both.sides"),
 col=pal[as.numeric(iris[, 5])])
Hulls(iris.p, iris[, 5], centers=TRUE, usecolors=pal)

## smoothed hulls
plot(iris.p, col=iris$Species, xlab="PC1", ylab="PC2")
ppts <- Hulls(iris.p, iris[, 5], centers=TRUE, outliers=FALSE, c.pch=NA)
## reveal outliers:
(out <- attr(ppts, "outliers"))
points(iris.p[out, ], pch=4, cex=1.4)

## this might complement Overlap()
cnts <- attr(ppts, "centers")
dist(cnts)
## how to use centers for clustering groups
plot(hclust(dist(cnts)))

## this is how to plot shaded hulls
plot(iris.p, pch=as.numeric(iris$Species))
for (i in seq_along(ppts))
 polygon(ppts[[i]], border=NA, col=adjustcolor(i, alpha.f=0.2))
}

\keyword{aplot}
