\name{confusion.matrix}
\title{Confusion Matrix - External Measures, Cluster Stability}
\description{
	For two different partitioning function computes \emph{confusion matrix}.
}
\usage{
confusion.matrix(clust1, clust2)
}
\alias{confusion.matrix}
\arguments{
  \item{clust1}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning. 
	}
  \item{clust2}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning.
	}
}
\details{
	Let P and P' be two different partitioning of the same data. Partitionings are represent as two  
	vectors \code{clust1, clust2}. Both vectors should have the same length. 
	Confusion matrix measures the size of intersection between clusters comming from P and P'
	according to equation:
	
	M[i,j] = | intersection of P(i) and P'(j) |
	
	where:
	\tabular{ll}{
		P(i)  \tab - cluster which belongs to partitioning P, \cr 
		P'(j) \tab - cluster which belongs to partitioning P', \cr
		|A|   \tab - cardinality of set A.	
	}
}
\value{
	\code{cls.set.section} returns a n x m integer \code{matrix}
	where n = |P| and m = |P'| defined above.
}
\examples{
# create two different subsamples 
mx1 <- matrix(as.integer( c(1,2,3,4,5,6,1,1,2,2,3,3) ), 6, 2 )
mx2 <- matrix(as.integer( c(1,2,4,5,6,7,1,1,2,2,3,3) ), 6, 2 )
# find section
m = cls.set.section(mx1,mx2)
confusion.matrix(as.integer(m[,2]),as.integer(m[,3]))
}
\seealso{
	Result used in \code{\link{similarity.index}}.
	}
\author{Lukasz Nieweglowski}
\keyword{cluster}