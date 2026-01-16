\name{dot.product}
\title{Cosine similarity measure - External Measure, Cluster Stability}
\description{
	Similarity index based on dot product is the measure which estimates how those two different partitionings, that comming from one 
	dataset, are different from each other.
}
\usage{
dot.product(clust1, clust2)
}
\alias{dot.product}
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
	Two input \code{vectors} keep information about two different partitionings of the same 
	subset comming from one data set. For each partitioning (let say P and P') its matrix 
	representation is created. Let P[i,j] and P'[i,j] each defines as:

	P[i,j] = 1 when object i and j belongs to the same cluster and i != j \cr
	P[i,j] = 0 in other case

	Two matrices are needed to compute \emph{dot product} using formula: 

	<P,P'> = sum(forall i and j) P[i,j]*P'[i,j]

	This \emph{dot product} satisfy Cauchy-Schwartz inequality <P,P'> <= <P,P>*<P',P'>.
	As result we get \emph{cosine similarity measure}: <P,P'>/sqrt(<P,P>*<P',P'>) 
}
\value{
	\code{dot.product} returns a \emph{cosine similarity measure} of two partitionings. 
	\code{NaN} is returned when in any partitioning each cluster contains only one object.
}
\seealso{ 
		Other external measures:
		\code{\link{std.ext}}, \code{\link{similarity.index}}
	}
\references{
	A. Ben-Hur and I. Guyon \emph{Detecting stable clusters using principal component analysis},
	\url{http://citeseer.ist.psu.edu/528061.html}

	T. Lange, V. Roth, M. L. Braun and J. M. Buhmann
	\emph{Stability-Based Validation of Clustering Solutions},
	\doi{10.1162/089976604773717621}
}
\examples{
# dot.product function(and also similarity.index) is used to compute 
# cluster stability, additional stability functions will be 
# defined - as its arguments some additional functions (wrappers) 
# will be needed

# define wrappers
pam.wrapp <-function(data)
{
	return( as.integer(data$clustering) )
}

identity <- function(data) { return( as.integer(data) ) }

agnes.average <- function(data, clust.num)
{
	return( cutree( agnes(data,method="average"), clust.num ) )
}

# define cluster stability function - cls.stabb

# cls.stabb arguments description:
# data - data to be clustered
# clust.num - number of clusters to which data will be clustered
# sample.num - number of pairs of data subsets to be clustered,
#              each clustered pair will be given as argument for 
#              dot.product and similarity.index functions 
# ratio - value comming from (0,1) section: 
#		  0 - means sample emtpy subset,
#		  1 - means chose all "data" objects
# method - cluster method (see wrapper functions)
# wrapp - function which extract information about cluster id assigned 
#         to each clustered object 

# as a result mean of dot.product (and similarity.index) results,
# computed for subsampled pairs of subsets is given
cls.stabb <- function( data, clust.num, sample.num , ratio, method, wrapp  )
{
	dot.pr  = 0
	sim.ind = 0
	obj.num = dim(data)[1]

	for( j in 1:sample.num )
	{
		smp1 = sort( sample( 1:obj.num, ratio*obj.num ) )
		smp2 = sort( sample( 1:obj.num, ratio*obj.num ) )

		d1 = data[smp1,]
		cls1 = wrapp( method(d1,clust.num) )

		d2 = data[smp2,]
		cls2 = wrapp( method(d2,clust.num) )

		clsm1 = t(rbind(smp1,cls1))
		clsm2 = t(rbind(smp2,cls2))

		m = cls.set.section(clsm1, clsm2)
		cls1 = as.integer(m[,2])
		cls2 = as.integer(m[,3])
		cnf.mx = confusion.matrix(cls1,cls2)
		std.ms = std.ext(cls1,cls2)
		
		# external measures - compare partitioning
		dt = dot.product(cls1,cls2)
		si = similarity.index(cnf.mx)

		if( !is.nan(dt) ) dot.pr = dot.pr + dt/sample.num 
		sim.ind = sim.ind + si/sample.num 
	}
	return( c(dot.pr, sim.ind) )
}

# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# fix arguments for cls.stabb function
iter = c(2,3,4,5,6,7,9,12,15)
smp.num = 5
sub.smp.ratio = 0.8

# cluster stability for PAM
print("PAM method:")
for( i in iter )
{
	result = cls.stabb(iris.data, clust.num=i, sample.num=smp.num,
           ratio=sub.smp.ratio, method=pam, wrapp=pam.wrapp)
	print(result)
}

# cluster stability for Agnes (average-link)
print("Agnes (single) method:")
for( i in iter )
{
	result = cls.stabb(iris.data, clust.num=i, sample.num=smp.num,
            ratio=sub.smp.ratio, method=agnes.average, wrapp=identity)
	print(result)
}
}
\author{Lukasz Nieweglowski}
\keyword{cluster}
