\name{Biarrows}
\alias{Biarrows}
\title{Adds correlation arrows to the scatterplot}
\description{Plots 'orig' variables as arrows on the 'deriv' variables 2D scatterplot}

\usage{Biarrows(deriv, orig, coeffs=NULL, shrink=0.45, closer=0.9,
 pt.col="forestgreen", pt.cex=1, pt.pch=NA, tx=colnames(orig),
 tx.col="forestgreen", tx.cex=0.8, tx.font=1, tx.pos=NULL, tx.off=0.5, xpd=TRUE,
 ar.col="forestgreen", ar.len=0.05, shift="auto", ...)}

\arguments{
\item{deriv}{Data derived from, e.g., dimension reducion of 'orig'}
\item{orig}{Original data}
\item{coeffs}{(Optional) two-column matrix with proposed coordinates of arrow tips, row names must represent 'orig' variables}
\item{shrink}{How to shrink arrows in relation to 'deriv' ranges, default is 45\% (0.45)}
\item{closer}{How closer to the center (in relation to the text label) is the arrow tip, default is 0.9}
\item{pt.col}{Color of points, default is "forestgreen"}
\item{pt.cex}{Size of points, default is 1}
\item{pt.pch}{Type of points, default is NA (no points)}
\item{tx}{Text labels, default are 'colnames(orig)'}
\item{tx.col}{Color of text labels, default is "forestgreen"}
\item{tx.cex}{Size of text, default is 0.8}
\item{tx.font}{Font of text, default is 1 (plain)}
\item{tx.pos}{Position of text, default is NULL (in the center)}
\item{tx.off}{Offest for text labels, default 0.5 (works only if 'tx.pos' is not NULL)}
\item{xpd}{Allow text to go outside of plotting region?}
\item{ar.col}{Color of arrows, default is "forestgreen"}
\item{ar.len}{Length of the edges of the arrow head (in inches)}
\item{shift}{Shift from the center which is c(0, 0); default is "auto" which is colMeans(deriv)}
\item{...}{Further arguments to arrows()}
}

\details{

Biarrows() calculates correlations between two sets of variables which
generally belong to the same data: more then one 'orig' variables and
exactly two 'deriv' variables. These correlations might be understood as
importances of the 'orig' variables. Then Biarrows() scales correlations
to the 'deriv' ranges and adds text labels and arrows (possibly also
points) to the scatterplot of derived variables. These arrows represent
the original variables in relation with derived variables. Resulted plot
may be seen as a biplot which simultaneously shows two sets of variables.
In fact, it is possible to show three and more sets of variables (see
examples).

This approach might work for data derived from (almost) any kind of
dimensional reduction. Biarrows() is also much more flexible than
standard biplot(). Please note, however, that Biarrows() is only
visualization, and numerical conclustions might not be justified.

If 'deriv' data contains more then 2 variables, the rest will be
discarded. Both 'deriv' and 'orig' should be either data frames or matrices
with column names and compatible dimensions, possibly with NAs.

Biarrows(dr, coeffs=...) allows to use pre-calculated coefficients. In
that case, 'data' is ignored (except for column names, but they might be
supplied separately as 'tx' value), and 'coeffs' will be scaled. See
examples to understand better how it works.

To suppress arrows or text, use zero color. Points are suppressed by
default.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{biplot}}}

\examples{

iris.cmd <- cmdscale(dist(iris[, -5]))
plot(iris.cmd, xlab="Dim 1", ylab="Dim 2")
Biarrows(iris.cmd, iris[, -5])
title(main="MDS biplot with Biarrows()")

## ===

library(MASS)
iris.mds <- isoMDS(dist(unique(iris[, -5])))
plot(iris.mds$points, xlab="Dim 1", ylab="Dim 2")
Biarrows(iris.mds$points, unique(iris[, -5]))
title(main="Non-metric MDS biplot with Biarrows()")

## ===

library(MASS)
iris.smm <- sammon(dist(unique(iris[, -5])))
plot(iris.smm$points, xlab="Dim 1", ylab="Dim 2")
Biarrows(iris.smm$points, unique(iris[, -5]))
title(main="Sammon mapping biplot with Biarrows()")

## ===

iris.p <- prcomp(iris[, -5], scale=TRUE)
biplot(iris.p, xpd=TRUE, main="Original PCA biplot")
plot(iris.p$x)
Biarrows(iris.p$x, iris[, -5])
title(main="PCA biplot with Biarrows()")

## ===

plot(iris.p$x, xlab="PCA1", ylab="PCA2")
## how to use 'coeffs'
## they also useful as surrogates of variable importances
(coeffs <- cor(iris[, -5], iris.p$x, method="spearman"))
Biarrows(iris.p$x, tx=rownames(coeffs), coeffs=coeffs)

## ===

plot(iris[, c(1, 3)])
Biarrows(iris[, c(1, 3)], iris.p$x)
title(main="\"Reversed biplot\"")

## ===

plot(iris[, c(1, 3)])
Biarrows(iris[, c(1, 3)], iris[, c(2, 4)])
title(main="Iris flowers: lengths vs. widths")

## ===

plot(iris.p$x)
Biarrows(iris.p$x[, 1:2], iris.p$x[, 1:2])
title(main="\"Self-biplot\" on PCA")

## ===

library(MASS)
iris.ldap <- predict(lda(Species ~ ., data=iris), iris[, -5])
plot(iris.ldap$x)
Biarrows(iris.ldap$x, iris[, -5])
Biarrows(iris.ldap$x, iris.p$x[, 1:2], shift=c(9, 2.5),
 shrink=0.95, lty=2, ar.col="darkgrey", tx.col="darkgrey")
title(main="Triplot: LDA, original variables and PCA axes")

## ===

iris.cl <- Classproj(iris[, -5], iris$Species)
plot(iris.cl$proj, col=iris$Species)
Biarrows(iris.cl$proj, iris[, -5])
title(main="Classproj biplot")

}

\keyword{aplot}
