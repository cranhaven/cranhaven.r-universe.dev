\name{Classproj}
\alias{Classproj}
\title{Class projection}
\description{Class projection which preserves distances between class centers}

\usage{
Classproj(data, classes, method="DMS")
}

\arguments{
\item{data}{Data: must be numeric and convertible into matrix}
\item{classes}{Class labels (correspond to data rows), NAs are allowed (sic!)}
\item{method}{Either "DMS" for Dhillon et al., 2002 or "QJ" for Qiu and Joe, 2006}
}

\details{

'Classproj' is the leveraged (supervised) or educated (semi-supervised)
manifold learning (dimension reduction). See examples for the variety of
its uses.

It uses classes to determine centers and then tries to preserve distances
between centers; two methods are possible: "DMS" which is slightly
faster, and "QJ" which frequently finds a better projection.

The code is based on the functions from 'clusterGeneration' package from
Weiliang Qiu.

}

\value{

Returns list with 'proj' coordinates of projected data points and
'centers' coordinates of class centers.

}

\references{

Dhillon I.S., Modha D.S., Spangler W.S. 2002. Class visualization of
high-dimensional data with applications. Computational Statistics and
Data Analysis. 41: 59--90.

Qiu W.-L., Joe H. 2006. Separation index and partial membership for
clustering. Computational Statistics and Data Analysis. 50: 585--603.

}

\author{Alexey Shipunov}

% \seealso{}

\examples{

## Leveraged approach (all classes are known)
iris.dms <- Classproj(iris[, -5], iris$Species, method="DMS")
plot(iris.dms$proj, col=iris$Species)
text(iris.dms$centers, levels(iris$Species), col=1:3)

iris.qj <- Classproj(iris[, -5], iris$Species, method="QJ")
plot(iris.qj$proj, col=iris$Species)
text(iris.qj$centers, levels(iris$Species), col=1:3)

## Educated approach (classes are known only for 10 data points per class)
sam <- Class.sample(iris$Species, 10)
newclasses <- iris$Species
newclasses[!sam] <- NA

iris.dms <- Classproj(iris[, -5], newclasses)
plot(iris.dms$proj, col=iris$Species, pch=ifelse(sam, 19, 1))
text(iris.dms$centers, levels(iris$Species), col=1:3)

iris.qj <- Classproj(iris[, -5], newclasses, method="QJ")
plot(iris.qj$proj, col=iris$Species, pch=ifelse(sam, 19, 1))
text(iris.qj$centers, levels(iris$Species), col=1:3)

## Automated approach (classes calculated automatically)
## Good to visualize _any_ clustering or learning
iris.km <- kmeans(iris[, -5], 3)

iris.dms <- Classproj(iris[, -5], iris.km$cluster)
plot(iris.dms$proj, col=iris.km$cluster)
text(iris.dms$centers, labels=1:3, col=1:3, cex=2)

iris.qj <- Classproj(iris[, -5], iris.km$cluster, method="QJ")
plot(iris.qj$proj, col=iris.km$cluster)
text(iris.qj$centers, labels=1:3, col=1:3, cex=2)
}

\keyword{multivariate}
