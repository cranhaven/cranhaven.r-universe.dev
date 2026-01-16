\name{initializePartition}
\Rdversion{1.1}
\alias{initializePartition}
%\alias{initializePartition,numeric,numeric,character,ANY}
%\alias{initializePartition,numeric,numeric,character,array}
\alias{initializePartition,numeric,numeric,character,ANY-method}
\alias{initializePartition,numeric,numeric,character,array-method}

\title{
~ Function: initializePartition ~
}
\description{
  This function provide different way of setting the initial partition
  for an EM algoritm.
}
\usage{
initializePartition(nbClusters, lengthPart, method = "kmeans++", data)
}
\arguments{
  \item{nbClusters}{[numeric]: number of clusters of that the initial
    partition should have.}
  \item{lengthPart}{[numeric]: number of individual in the partition.}
  \item{method}{[character]: one off "randomAll", "randomK", "maxDist",
    "kmeans++", "kmeans+", "kmeans--" or "kmeans-".}
  \item{data}{[matrix]: \code{data} is the matrix of the individuals (usefull for the methods that
    need to compute distance between
    individual). If \code{data} is an array, the distance is computed
    using "maxDist" is used, the
    function needs to know the matrix of the distance between each individual.}
}

\details{
  Before alternating the phase Esperance and Maximisation, the EM
  algorithm needs to initialize a starting configuration. This initial
  partition has been proven to have an important impact on the
  final result and the convergence time.

  This function provides different ways of setting the initial partition.
  \itemize{
    \item randomAll: all the individual are randomly assigned to a
    cluster with at least one individual in each clusters.
    \item randomK: K individuals are randomly assigned to a
    cluster, all the other are not assigned (each cluster has only one
    individual).
    \item maxDist: K indivuals are chosen. The two formers are the
    individual separated by the highest distance. The latter are added one
    by one, they are the "farthest" individual among those that are already been
    selected. "farthest" is the individual with the highest distance (min)
    to the selected individuals (if "t" are the individual already selected, the
    next selected individual is "i" such that
    max_i(min_t(dist(IND_i,IND_t))) ). This method is efficient but time
    consuming.
    \item kmeans++: see [3]
    \item kmeans+, kmeans--, kmeans-: experimental methods derived from [3].
  }
}
\value{
vecteur of numeric.
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr

  [3] D. Arthur and S. Vassilvitskii\cr
  "k-means++: the advantages of careful seeding"\cr
  Proceedings of the eighteenth annual ACM-SIAM symposium on Discrete
  algorithms. pp. 1027-1035, 2007.
}


\examples{
par(ask=TRUE)
###################
### Constrution of some longitudinal data
data(artificialLongData)
dn <- longData(artificialLongData)
plotTrajMeans(dn)

###################
### partition using randamAll
pa1a <- initializePartition(3,lengthPart=200,method="randomAll")
plotTrajMeans(dn,partition(pa1a),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))
pa1b <- initializePartition(3,lengthPart=200,method="randomAll")
plotTrajMeans(dn,partition(pa1b),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))

###################
### partition using randamK
pa2a <- initializePartition(3,lengthPart=200,method="randomK")
plotTrajMeans(dn,partition(pa2a),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))
pa2b <- initializePartition(3,lengthPart=200,method="randomK")
plotTrajMeans(dn,partition(pa2b),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))

###################
### partition using maxDist
pa3 <- initializePartition(3,lengthPart=200,method="maxDist",data=dn["traj"])
plotTrajMeans(dn,partition(pa3),parMean=parMEAN(type="n"),parTraj=parTRAJ(col="clusters"))
### maxDist is deterministic, so no need for a second example


###################
### Example to illustrate "maxDist" method on classical clusters
point <- matrix(c(0,0, 0,1, -1,0, 0,-1, 1,0),5,byrow=TRUE)
points <- rbind(point,t(t(point)+c(10,0)),t(t(point)+c(5,6)))
points <- rbind(points,t(t(points)+c(30,0)),t(t(points)+c(15,20)),t(-t(point)+c(20,10)))
plot(points,main="Some points")

paInit <- initializePartition(2,nrow(points),method="maxDist",points)
plot(points,main="Two farest points")
lines(points[!is.na(paInit),],col=2,type="p",pch=16)

paInit <- initializePartition(3,nrow(points),method="maxDist",points)
plot(points,main="Three farest points")
lines(points[!is.na(paInit),],col=2,type="p",pch=16)

paInit <- initializePartition(4,nrow(points),method="maxDist",points)
plot(points, main="Four farest points")
lines(points[!is.na(paInit),],col=2,type="p",pch=16)

par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
