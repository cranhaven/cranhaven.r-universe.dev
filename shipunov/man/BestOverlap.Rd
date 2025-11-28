\name{BestOverlap}
\alias{BestOverlap}

\title{Calculates the best overlap}

\description{

Uses multiple datasets, measures overlaps between class-related convex
hulls and reports the best dataset, the best overlap table and summary
with confidence intervals. Can be used to assess bootstrap or jackknife
results, to compare different dimension reduction and/or clustering
methods, and to average results of stochastic methods.

}

\usage{
BestOverlap(xylabels, ci="95\%", round=4)
}

\arguments{

\item{xylabels}{List of data frames, each with at least 3 columns named
exactly as: "x" for x coordinates, "y" for y coordinates and "labels"
for class labels}

\item{ci}{Confidence interval (character string with percent sign)}

\item{round}{How to round numbers in summary table}

}

\details{

'BestOverlap()' requires object, typically created after bootstrapping or
similar procedure (see below for examples). This 'xylabels' object must
contain at least three columns named exactly as c("x", "y", "labels"), in
any order.

Please note that label types must be the same between data frames inside
'xylabels' list. For consistency, first data frame is used as a label
standard. If any next data frame contain label types different from
standard, it will be ignored.

}

\value{

List with three components: 'best' data frame, 'best.overlap' table and
'summary' data frame.

}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{

\donttest{ % because they are slow
## Bootstrap PCA
B <- 100
xylabels <- vector("list", length=0)
for (n in 1:B) {
ROWS <- sample(nrow(iris), replace=TRUE)
tmp <- prcomp(iris[ROWS, -5])$x[, 1:2]
xylabels[[n]] <- data.frame(x=tmp[, 1], y=tmp[, 2], labels=iris[ROWS, 5])
}
BestOverlap(xylabels)

## Jacknife PCA
B <- nrow(iris)
xylabels <- vector("list", length=0)
for (n in 1:B) {
ROWS <- (1:B)[-n]
tmp <- prcomp(iris[ROWS, -5])$x[, 1:2]
xylabels[[n]] <- data.frame(x=tmp[, 1], y=tmp[, 2], labels=iris[ROWS, 5])
}
BestOverlap(xylabels)

## Stochastic method: Stochastic Proximity Embedding
library(tapkee)
B <- 100
xylabels <- vector("list", length=0)
for (n in 1:B) {
tmp <- Tapkee(iris[, -5], method="spe")
xylabels[[n]] <- data.frame(x=tmp[, 1], y=tmp[, 2], labels=iris[, 5])
}
BestOverlap(xylabels)

## Diverse dimension reduction methods
library(tapkee)
B <- c("lle", "npe", "ltsa", "lltsa", "hlle", "la", "lpp", "dm", "isomap", "l-isomap")
xylabels <- vector("list", length=0)
for (n in B) {
tmp <- Tapkee(iris[, -5], method=n, add="-k 50")
xylabels[[n]] <- data.frame(x=tmp[, 1], y=tmp[, 2], labels=iris[, 5])
}
BestOverlap(xylabels)

## One dimension reduction but many clusterings
B <- 100
xylabels <- vector("list", length=0)
tmp1 <- prcomp(iris[, -5])$x[, 1:2]
for (n in 1:B) {
tmp2 <- kmeans(iris[, -5], centers=3)$cluster
xylabels[[n]] <- data.frame(x=tmp1[, 1], y=tmp1[, 2], labels=letters[tmp2])
}
BestOverlap(xylabels)

}

}

\keyword{multivariate}
