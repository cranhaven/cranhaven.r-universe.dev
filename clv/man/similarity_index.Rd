\name{similarity.index}
\title{Similarity index based on confusion matrix - External Measure, Cluster Stability}
\description{
	Similarity index based on confusion matrix is the measure which estimates how those two different partitionings, that comming from one 
	dataset, are different from each other.
	For given \code{matrix} returned by \code{\link{confusion.matrix}} function 
	\emph{similarity index} is found.
}
\usage{
similarity.index(cnf.mx)
}
\alias{similarity.index}
\arguments{
  \item{cnf.mx}{
		not negative, integer \code{matrix} or \code{data.frame} which represents 
		object returned by \code{\link{confusion.matrix}} function.
	}
}
\details{
	Let M is n x m (n <= m) \emph{confusion matrix} for partitionings P and P'.
	Any one to one function \emph{sigma}: \{1,2,...,n\} -> \{1,2,... ,m\}.
	is called \emph{assignment} (or also \emph{association}).
	Using set of \emph{assignment} functions, A(P,P') index defined as:
	
	A(P,P') = max\{ sum( forall i in 1:length(\emph{sigma}) )
	M[i,\emph{sigma(i)}]: \emph{sigma} is an \emph{assignment} \}
	
	is found. (\emph{Assignment} which satisfy above equation is called \emph{optimal assignment}).
	Using this value we can compute \emph{similarity index} S(P.P') = (A(P,P') - 1)/(N - 1) where 
	N is quantity of partitioned objects (here is equal to \code{sum(M)}).
}
\value{
	\code{similarity.index} returns value from section [0,1] which is a measure of similarity 
	between two different partitionings. Value 1 means that we have two the same partitionings.
}
\seealso{
		\code{\link{confusion.matrix}} as matrix representation of two partitionings.
		Other functions created to compare two different partitionings:
		\code{\link{std.ext}}, \code{\link{dot.product}}
	}
\references{
	C. D. Giurcaneanu, I. Tabus, I. Shmulevich, W. Zhang \emph{Stability-Based Cluster Analysis Applied To Microarray Data}, 
	\url{http://citeseer.ist.psu.edu/577114.html}.

	T. Lange, V. Roth, M. L. Braun and J. M. Buhmann
	\emph{Stability-Based Validation of Clustering Solutions},
	\doi{10.1162/089976604773717621}
}
\examples{
# similarity.index function(and also dot.product) is used to compute 
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

# as a result mean of similarity.index (and dot.product) results,
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
