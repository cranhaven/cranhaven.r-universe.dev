\name{partition}
\alias{partition}
\alias{partition,ANY,missing,ANY-method}
\alias{partition,ANY,array,ANY-method}
\alias{partition,ANY,matrix,ANY-method}
\alias{partition,ANY,LongData,ANY-method}
\alias{partition,ANY,LongData3d,ANY-method}
\alias{partition,missing,missing,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: partition ~ }
\description{
\code{partition} is the constructor of the class
\code{\linkS4class{Partition}}. It can be build either alone or
relatively to a object \code{LongData}.

}
\usage{
partition(clusters, traj, details=character())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusters}{\code{[vector(factor)]}: cluters to which each
    individual belongs. Each clusters is represented by an upper letters.}
  \item{traj}{\code{[matrix]} or \code{[array]}: if an object \code{LongData} is
    provide, it will be used to compute the quality criterion of the
    clustering. \code{array} are simply turn into matrix by "sticking"
    all the variables one behind the other.}
  \item{details}{\code{[vector(character)]}: the slot \code{details} is
    used to store various informations. If the \code{Partition} has been
    find using an algorithm, it can store the name of the algorithm, the
    time before convergence, the number of iteration and any other
    informations. The syntaxe is \code{details=c(algoritm="kmeans",convergenceTime="6",otherInfo="WhateverIWant")}.}
}
\details{
  \code{partition} construct a object of class
  \code{\linkS4class{Partition}}. It does not provide any default
  values. \code{yLongData} and \code{details} are optional.
}
\value{
  An object of class \code{\linkS4class{Partition}}.
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011
}

\seealso{\linkS4class{Partition},\code{\link{ordered}}}


\examples{
### Empty partition
partition()

### Small partition
partition(clusters=c("A","B","A","C","C"))

### Random partition
partition(clusters=LETTERS[floor(runif(100,1,5))])

### Partition that clusters correctly some data
###   Quality criterion are high
data(artificialLongData)
traj <- as.matrix(artificialLongData[,-1])
partition(clusters=rep(1:4,each=50),traj)

### Partition that does not cluster correctly the data
###   Quality criterion are low
partition(clusters=rep(1:4,50),traj)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}  % Clustering
\keyword{methods}
