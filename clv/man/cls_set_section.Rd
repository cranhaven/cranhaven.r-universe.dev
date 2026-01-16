\name{cls.set.section}
\title{Section of two subsets - External Measure utilities}
\description{
	Function finds section of two different subsets comming from the same data set.
}
\usage{
cls.set.section(clust1, clust2)
}
\alias{cls.set.section}
\arguments{
  \item{clust1}{
		n x 2 integer \code{matrix} or \code{data.frame}. 
		First column gives information about object number in data set in increasing order.
		Second column store information about cluster id the object is assigned to.
		If matrix is not an integer type, it will be coerced with warning.
	}
  \item{clust2}{
		n x 2 integer \code{matrix} or \code{data.frame}.
		First column gives information about object number in data set in increasing order.
		Second column store information about cluster id the object is assigned to.
		If matrix is not an integer type, it will be coerced with warning.
	}
}
\details{
	Let A and B be two different subsamples of the same data set. 
	Each subset is partitioned into P(A) and P(B) cluster sets. Information about object and cluster id's 
	for pairs (A,P(A)) and (B,P(B)) are stored in \code{matrices clust1} and \code{clust2}. 
	Function creates matrix which represents section of A and B. 
	
}
\value{
	\code{cls.set.section} returns a n x 3 integer \code{matrix}.
	First column gives information about object number in dataset in increasing order.
	Second column store information about cluster id the object is assigned to. 
	Information is taken from \code{clust1 vector}
	The same is for the third column but cluster id is taken from \code{vector clust2}.
}
\examples{
# create two different subsamples 
mx1 <- matrix(as.integer( c(1,2,3,4,5,6,1,1,2,2,3,3) ), 6, 2 )
mx2 <- matrix(as.integer( c(1,2,4,5,6,7,1,1,2,2,3,3) ), 6, 2 )
# find section
m = cls.set.section(mx1,mx2)
}
\seealso{
		Function preapres data for further computation. Result mostly is used in: 
		\code{\link{std.ext}}, \code{\link{dot.product}}, \code{\link{confusion.matrix}}
	}
\author{Lukasz Nieweglowski}
\keyword{cluster}