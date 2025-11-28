\name{Ellipses}
\alias{Ellipses}
\title{Confidence ellipses}
\description{Calculates and plots group confidence ellipses}

\usage{
Ellipses(pts, groups, match.color=TRUE, usecolors=NULL,
 centers=FALSE, c.pch=0, c.cex=3,
 level=0.95, df=1000, prec=51,
 coords=NULL, plot=TRUE, ...)
}

\arguments{
\item{pts}{Data points to plot}
\item{groups}{Grouping variable}
\item{match.color}{Match colors}
\item{usecolors}{Use colors (palette)}
\item{centers}{Show centers?}
\item{c.pch}{Color of center points}
\item{c.cex}{Scale of center points}
\item{level}{Confidence level for F-distribution}
\item{df}{Used in calculation of P-content according to F(2, df) distribution}
\item{prec}{Precision of ellipse plotting (default is 51 points)}
\item{coords}{Pre-calculated ellipses coordinates: list of two-column matrices named as groups (by default, not required)}
\item{plot}{Plot?}
\item{...}{Arguments to lines()}
}

\details{

Note that (at least at the moment), ellipses are plotted with line(),
therefore shading is not straightforward (but possible, see examples).

Also, with a help from Pinhull() (see its help), it is possible to reveal
"outliers", points outside of each ellipse borders.

See also package 'cluster' for ellipsoidhulls() function that allows to
draw ellipse-like hulls.

}

\value{

Invisibly returns the list in the form similar to Hulls(), to use as a
list of polygons or with Overlap().

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}, \code{\link{Overlap}}, \code{\link{Pinhull}}}

\examples{

iris.p <- prcomp(iris[, -5])$x[, 1:2]
plot(iris.p, type="n", xlab="PC1", ylab="PC2")
text(iris.p, labels=abbreviate(iris[, 5], 1, method="both.sides"))
iris.e <- Ellipses(iris.p[, 1:2], iris[, 5], centers=TRUE)

## calculate overlap between ellipses
Overlap(iris.e)

## how to plot filled ellipses
plot(iris.p, type="n", xlab="PC1", ylab="PC2")
text(iris.p, labels=abbreviate(iris[, 5], 1, method="both.sides"))
for (i in seq_along(iris.e))
 polygon(iris.e[[i]], border=NA, col=adjustcolor(i, alpha.f=0.2))

## how to reveal (and label) "outliers", points outside of _all_ ellipses
iris.pie <- Pinhull(iris.p, iris.e)
outs <- which(apply(iris.pie, 1, sum) == 0)
points(iris.p[outs, ], cex=2, pch=4)

## embedded convex hulls
plot(iris.p, col=iris$Species)
for (i in seq_along(iris.e)) lines(iris.e[[i]], col=i, lty=2)
mi <- cbind(seq_len(nrow(iris)), as.numeric(iris$Species)) # indexing matrix
## remove "outliers" in broad sense, points which are outside of its "own" ellipse:
emb <- rowSums(iris.pie) == 1 & iris.pie[mi]
Hulls(iris.p[emb, ], iris$Species[emb])

## LDA ellipes
library(MASS)
ch.lda <- lda(Species ~ ., data=chaetocnema[, -2])
ch.lda.pred <- predict(ch.lda, chaetocnema[, -(1:2)])
## ellipses here are by default bigger then plot so use workaround:
ee <- Ellipses(ch.lda.pred$x, chaetocnema$Species, plot=FALSE)
xx <- range(c(do.call(rbind, ee)[, 1], ch.lda.pred$x[, 1]))
yy <- range(c(do.call(rbind, ee)[, 2], ch.lda.pred$x[, 2]))
plot(ch.lda.pred$x, col=chaetocnema$Species, xlim=xx, ylim=yy)
Ellipses(ch.lda.pred$x, chaetocnema$Species, coords=ee)

## search for the maximal level which gives zero overlap
plot(x5 ~ x17, data=haltica, pch=as.numeric(haltica$Species))
for (i in (99:59)/100) {
cat(i, "\n")
ee <- Ellipses(haltica[, c("x17", "x5")], haltica$Species, level=i, plot=FALSE)
print(mean(Overlap(ee), na.rm=TRUE))
cat("\n")
}
Ellipses(haltica[, c("x17", "x5")], haltica$Species, level=.62)
}

\keyword{aplot}
