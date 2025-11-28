\name{Updist}
\alias{Updist}
\title{Educated distances for semi-supervised clustering}

\description{Updates distance matrix to help link or unlink objects}

\usage{
  Updist(dst, link=NULL, unlink=NULL, dmax=max(dst), dmin=min(dst))
}

\arguments{
  \item{dst}{\code{dist} object}
  \item{link}{1-level list with the arbitrary number of components,
   each component is a numeric vector of row numbers for objects which you prefer to be linked}
  \item{unlink}{1-level list with the arbitrary number of components,
   each component is a numeric vector of row numbers for objects which you prefer to be not linked}
  \item{dmax}{Distance to set for not linked objects}
  \item{dmin}{Distance to set for linked objects}
}

\details{
  This function borrows the idea of MPCKM semi-supervised k-means (Bilenko et al., 2004)
  but instead of updating distances on the run, it simply updates the distances object
  beforehand in accordance with 'link' and 'unlink' constraints.

  Amazingly, it works as expected :) Please see the examples below.
}

\references{
  Bilenko M., Basu S., Mooney R.J. 2004. Integrating constraints and metric learning
  in semi-supervised clustering. In: Proceedings of the twenty-first international
  conference on Machine learning. P. 11. ACM.
}

\seealso{
  \code{\link{dist}}
}

\examples{

iris.d <- dist(iris[, -5])
iris.km <- kmeans(iris.d, 3)
iris.h <- cutree(hclust(iris.d, method="ward.D"), k=3)

Misclass(iris.km$cluster, iris$Species, best=TRUE)
Misclass(iris.h, iris$Species, best=TRUE)

i.vv <- cbind(which(iris$Species == "versicolor"), which(iris$Species == "virginica"))
i.link <- list(sample(i.vv[, 2], 25), sample(i.vv[, 1], 25))
i.unlink <- list(i.vv[1, ], i.vv[2, ])

iris.upd <- Updist(iris.d, link=i.link, unlink=i.unlink)

iris.ukm <- kmeans(iris.upd, 3)
iris.uh <- cutree(hclust(iris.upd, method="ward.D"), k=3)

Misclass(iris.ukm$cluster, iris$Species, best=TRUE)
Misclass(iris.uh, iris$Species, best=TRUE)

## ===

aad <- dist(t(atmospheres))
plot(hclust(aad))

aadu <- Updist(aad, unlink=list(c("Earth", "Mercury")))
plot(hclust(aadu))
}

\keyword{cluster}
