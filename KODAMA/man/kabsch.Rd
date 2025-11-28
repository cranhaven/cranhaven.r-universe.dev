\name{kabsch}

\alias{kabsch}

\title{Kabsch Algorithm}

\description{
Aligns two sets of points via rotations and translations. Given two sets of points, with one specified as the reference set, the other set will be rotated so that the RMSD between the two is minimized. The format of the matrix is that there should be one row for each of n observations, and the number of columns, d, specifies the dimensionality of the points. The point sets must be of equal size and with the same ordering, i.e. point one of the second matrix is mapped to point one of the reference matrix, point two of the second matrix is mapped to point two  of the reference matrix, and so on.
}

\usage{
kabsch (pm, qm)
}

\arguments{
  \item{pm}{n x d matrix of points to align to to \code{qm}.}
  \item{qm}{n x d matrix of reference points.}
}


\value{
    Matrix \code{pm} rotated and translated so that the ith point is aligned to the ith point of \code{qm} in the least-squares sense.
  }

\author{James Melville}


\examples{
data=iris[,-5]
pp1=pca(data)$x
pp2=pca(scale(data))$x
pp3=kabsch(pp1,pp2)
plot(pp1,pch=21,bg=rep(2:4,each=50))
points(pp3,pch=21,bg=rep(2:4,each=50),col=5)
}

\keyword{pca}
