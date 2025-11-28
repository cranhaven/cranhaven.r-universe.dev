\name{Vicinities}
\alias{Vicinities}

\title{Arrange observations by the distance from center}

\description{Uses group centers to order all observations within group}

\usage{Vicinities(data, groups, num=NULL, centers=NULL, method.c="median", method.d="manhattan")}

\arguments{

\item{data}{Numeric data frame or matrix}
\item{groups}{Grouping factor}
\item{num}{How many indices per group to return, default is all}
\item{centers}{Matrix or data frame with group centers, one row per 'groups' level}
\item{method.c}{How to calculate group centers, name of function}
\item{method.d}{How to calculate distances between centers and individual observations, dist() mehtod}
}

\details{

This function takes data (data frame or matrix), grouping factor and
(optionally) matrix or data frame with group centers. Then it calculates
proximities between all observations and corresponding center, group by
group. Result is a list of proximity indices (row numbers). This list
allows, for example, to find "central" ("typical", "nuclear")
observations useful, e.g., as centroids or medoids, and also "peripheral"
observations, "outliers".

Distances by default are calculated with dist(..., method="manhattan")
but it is possible to specify any other dist() method via "method.d".

Pre-defined centers might be taken from many sources, e.g., Hulls(),
Ellipses(), Classproj(), see examples.

If "centers" data is not supplied, then Vicinities() will perform a naive
computation of group centers via univariate medians. It is also possible
to use (via "method.c") mean or any similar function which works within
sapply() and accepts "na.rm=TRUE" option.

If size of the group is less then "num", the resulted list will contain
NAs. If this is not a desired behavior, use something like lapply(res,
head, num).

}

\value{

The list of nlevels(as.factor(groups)) size, components named from these
levels and contained "num" numeric indices, corresponding with the row
numbers of original data.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}, \code{\link{Ellipses}}, \code{\link{Classproj}}}

\examples{

## use for MDS
iris.d <- dist(iris[, -5])
iris.c <- cmdscale(iris.d)
iris.sc <- as.data.frame(iris.c)
## naive calculation
first3n <- unlist(Vicinities(iris.sc, iris$Species, num=3))
last10n <- unlist(lapply(Vicinities(iris.sc, iris$Species), tail, 10))
##
plot(iris.sc, col=iris$Species)
points(iris.sc[first3n, ], pch=19, col=iris$Species[first3n])
points(iris.sc[last10n, ], pch=4, cex=2, col="black")

## use pre-defined centers from Hulls()
plot(iris.sc, col=iris$Species)
iris.h <- Hulls(iris.sc, groups=iris$Species, centers=TRUE)
first3h <- unlist(Vicinities(iris.sc, iris$Species, centers=iris.h$centers, num=3))
points(iris.sc[first3h, ], pch=19, col=iris$Species[first3h])

## use pre-defined centers from Ellipses()
plot(iris.sc, col=iris$Species)
iris.e <- Ellipses(iris.sc, groups=iris$Species, centers=TRUE)
first3e <- unlist(Vicinities(iris.sc, iris$Species, centers=iris.e$centers, num=3))
points(iris.sc[first3e, ], pch=19, col=iris$Species[first3e])

## ===

## plot and use pre-defined centers from Classproj()
iris.cl <- Classproj(iris[, -5], iris[, 5])
first3cc <- unlist(Vicinities(iris.cl$proj, iris[, 5], centers=iris.cl$centers, num=3))
plot(iris.cl$proj, col=iris$Species)
points(iris.cl$proj[first3cc, ], pch=19, col=iris$Species[first3cc])
## now calculate centers naively from projection data
first3cn <- unlist(Vicinities(iris.cl$proj, iris[, 5], num=3))
points(iris.cl$proj[first3cn, ], pch=1, cex=1.5, col=iris$Species[first3cn])

## ===

## use as medoids for PAM
library(cluster)
iris.p <- pam(iris.d, k=3)
Misclass(iris.p$clustering, iris[, 5], best=TRUE) # to compare
## naive method, raw data (4 columns)
first1nr <- unlist(Vicinities(iris[, -5], iris$Species, 1))
iris.pm <- pam(iris.d, k=3, medoids=first1nr)
Misclass(iris.pm$clustering, iris[, 5], best=TRUE) # slightly better!
## ... or as centers for k-means, for stability
first1h <- unlist(Vicinities(iris.sc, iris$Species, centers=iris.h$centers, num=1))
iris.km <- kmeans(iris[, -5], centers=iris[first1h, -5])
Misclass(iris.km$cluster, iris[, 5], best=TRUE)

## ===

## PCA and different vicinities methods
iris.p <- prcomp(iris[, -5])$x[, 1:2]
plot(iris.p, col=iris$Species)
first3p1 <- unlist(Vicinities(iris.p, iris[, 5], num=3))
first3p2 <- unlist(Vicinities(iris.p, iris[, 5], num=3,
 method.c="mean", method.d="euclidean")) # mean, Euclidean
points(iris.p[first3p1, ], pch=19, col=iris[first3p1, 5])
points(iris.p[first3p2, ], pch=1, cex=1.5, col=iris[first3p2, 5])

## ===

## use MDS vicinities to reduce dataset for hierarchical clustering with bootstrap
iris3 <- iris[first3n, ]
iris3b <- Bclust(iris3[, -5], method.d="euclidean", method.c="average", iter=100)
plot(iris3b$hclust, labels=iris3[, 5])
Bclabels(iris3b$hclust, iris3b$values)

iris3j <- Jclust(iris3[, -5], method.d="euclidean", method.c="average",
 n.cl=3, iter=100)
plot(iris3j, labels=iris3[, 5])

## ===

## use of external function to compute naive distances
Mode <- function(x, na.rm=TRUE) {
 if (length(x) <= 2) return(x[1])
 if (na.rm & anyNA(x)) x <- x[!is.na(x)]
 ux <- unique(x)
 ux[which.max(tabulate(match(x, ux)))]
}
Vicinities(iris[, -5], iris[, 5], method.c="Mode", 3)

}

\keyword{multivariate}
