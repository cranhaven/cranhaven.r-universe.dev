\name{cls.stab.sim.ind.usr}
\title{Cluster Stability - Similarity Index and Pattern-wise Stability Approaches with User Defined Cluster Algorithms}
\description{

	\code{cls.stab.sim.ind.usr} and \code{cls.stab.opt.assign.usr} reports validation measures for clustering results. Both functions return lists of  
	cluster stability results computed for user defined cluster algorithms according to similarity index and pattern-wise stability approaches.
}
\usage{
cls.stab.sim.ind.usr( data, cl.num, clust.alg, sim.ind.type, rep.num, subset.ratio )
cls.stab.opt.assign.usr( data, cl.num, clust.alg, rep.num, subset.ratio )
cls.alg( clust.method, clust.wrap, fast )
}
\alias{cls.stab.sim.ind.usr}
\alias{cls.stab.opt.assign.usr}
\alias{cls.alg}
\arguments{
  \item{data}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and rows to 
		observations.
	}
  \item{cl.num}{
		integer \code{vector} with information about numbers of cluster to which \code{data} will be partitioned.
		If vector is not an integer type, it will be coerced with warning.
	}
  \item{clust.alg}{
    there are two possible types of input:

      1. clustering function that takes two arguments: "data" to be partitioned described in \code{data} section 
         and "clust.num" that represents number of cluster to which data will be partitioned. Function represents partitioning algorithm.

      2. an object of type "cls.alg" returned by \code{cls.alg} function (see "Details" for explanation). Object represents 
         hierarchical algorithm.
  } 
  \item{clust.method}{
    hierarchical clustering function that takes only one argument named "data" described in \code{data} section.
    Function should return hierarchical structure that might be applied as parameter to \code{clust.wrap} function. 
  }
  \item{clust.wrap}{
    cluster function that takes exactly two arguments: "clust.res" that represents the result of \code{clust.method} function and
    "clust.num" which is the number of clusters to which "clust.res" is going to be cut. Function should return integer vector that 
    represents object id (comming from \code{data} set) to cluster id (integer between 1 and \code{clust.num}) association.
  } 
  \item{sim.ind.type}{
		string vector with information useful only for \code{cls.stab.sim.ind.usr} function. User is able to choose which 
		similarity indicies (external measures) to use to compare two partitionings. Available are:
		"dot.pr", "sim.ind", "rand", "jaccard" (for more details see \code{\link{similarity.index}},
		\code{\link{dot.product}}, \code{\link{std.ext}}). Combinations are also possible.
		By default \code{c("dot.pr","sim.ind")} vector is applied.
	}
  \item{rep.num}{
    integer number which tells how many pairs of data subsets will be partitioned for particular number of clusters.
    The results of partitioning for given pair of subsets is used to compute similarity indices (in case of \code{cls.stab.sim.ind.usr}) 
    or \emph{pattern-wise stability} (in case of \code{cls.stab.opt.assign.usr}, for more details see references). 
    By default \code{rep.num} value is 10. If wrong argument is applied it will be repaced with default value.
  }
  \item{subset.ratio}{
    a number comming from (0,1) section which tells how big data subsets should be. 0 means empty subset, 1 means all data. 
    By default \code{subset.ratio} is set to 0.75. If wrong argument is applied it will be repaced with default value.
  }
  \item{fast}{
    logical argument which sets the way of computing cluster stability for hierarchical algorithms. By default it is set to
    TRUE, which means that each result produced by hierarchical algorithm is partitioned for the number of clusters chosen in 
    \code{cl.num} argument and given clustering results are put for further computation. In this way computation of cluster 
    stability is faster. If wrong argument is applied it will be repaced with default value.
}
}
\details{
	Both functions realize cluster stability approaches described in \emph{Detecting stable clusters using principal component analysis} 
  chapters \emph{3.1} and \emph{3.2} (see references).

  The \code{cls.stab.sim.ind.usr} as well as \code{cls.stab.opt.assign.usr} do the same thing as \code{\link{cls.stab.sim.ind}} and 
  \code{\link{cls.stab.opt.assign}} functions. Main difference is that using this functions user is able to define and apply its own cluster 
  algorithm to measure its cluster stability. For that reason \code{clust.alg} argument is introduced. This argument may represent partitioning 
  algorithm (by passing it directly as a function) or hierarchical algorithm (by passing an object of "cls.alg" type produced by \code{cls.alg} 
  function).
  
  If a partitioning algorithm is going to be used the decalration of this function that represents this algorithm should always look 
  like this: \code{ function(data, clust.num) { ... return(integer.vector)} }.
  As an output function should always return integer vector that represents single clustering result on \code{data}. 
  
  If a hierarchical algorithm is going to be used user has to use helper \code{cls.alg} function that produces an object of "cls.alg" type.
  This object encapsulates a pair of methods that are used in hierarchical version (which is faster if the \code{fast} argument is not FALSE) 
  of cluster stability approach. These methods are:\cr
  1. \emph{clust.method} - which builds hierarchical structure that might be cut. The declaration of this function should always look like 
     this one: \kbd{ function(data) \{ ... return(hierarchical.struct) \} },\cr
  2. \emph{clust.wrap} - which cuts this hierarchical structure to \code{clust.num} clusters. This function definition should always look 
     like this one: \kbd{ function(clust.res, clust.num) \{ ... return(integer.vector)\} }. As an output function should 
     always return integer vector that represents single clustering result on \code{clust.res}.

  \code{cls.alg} function has also third argument that indicates if fast computation should be taken (when \code{TRUE}) or if these two 
  methods should be converted to one partitioning algorithm and to be run as a normal partitioning algorithm.

  Well defined cluster functions "f" should always follow this rules (size(data) means number of object to be partitioned, 
  res - integer vector with cluster ids):\cr
  1. when \code{data} is empty or \code{cl.num} is less than 2 or more than \code{size(data)} then \code{f(data, cl.num)} returns error.
  2. if \code{f(data, cl.num) -> res} then length(res) == size(data),\cr
  3. if \code{f(data, cl.num) -> res} then for all "elem" in "res" the folowing condition is true: \code{0 < elem <= cl.num}.

  It often happens that clustering algorithms can't produce amount of clusters that user wants. In this situation only the warning is 
  produced and cluster stability is computed for partitionings with unequal number of clusters.
  
  The cluster stability will not be calculated for all cluster numbers that are bigger than the subset size.
  For example if \code{data} contains about 20 objects and the \code{subset.ratio} equals 0.5 then the highest cluster number to 
  calculate is 10. In that case all elements above 10 will be removed from \code{cl.num} vector.
}
\value{
	\code{cls.stab.sim.ind.usr} returns a lists of matrices. Each matrix consists of the set of external similarity indices (which one similarity 
	index see below) where number of columns is equal to \code{cl.num} vector length and row number is equal to \code{rep.num} value what means
	that each column contain a set of similarity indices computed for fixed number of clusters. The order of the matrices depends on 
  \code{sim.ind.type} argument. Each element of this list correspond to one of similarity index type chosen thanks to \code{sim.ind.type} argument. 
	The order of the names exactly match to the order given in those arguments description.\cr

	\code{cls.stab.opt.assign.usr} returns a vector. The vector consists of the set of cluster stability indices described in 
	\emph{Detecting stable clusters using principal component analysis} chapter \emph{3.2} (see references). Vector length is equal to \code{cl.num} vector length what 
	means that each position in vector is assigned to proper clusters' number given in \code{cl.num} argument.
}
\seealso{
    Other cluster stability methods:
    \code{\link{cls.stab.sim.ind}}, \code{\link{cls.stab.opt.assign}}.

		Functions that compare two different partitionings:
		\code{\link{clv.Rand}}, \code{\link{dot.product}},\code{\link{similarity.index}}.
	}
\references{
	A. Ben-Hur and I. Guyon \emph{Detecting stable clusters using principal component analysis},
	\url{http://citeseerx.ist.psu.edu/}

	C. D. Giurcaneanu, I. Tabus, I. Shmulevich, W. Zhang \emph{Stability-Based Cluster Analysis Applied To Microarray Data}, 
	\url{http://citeseerx.ist.psu.edu/}.

	T. Lange, V. Roth, M. L. Braun and J. M. Buhmann
	\emph{Stability-Based Validation of Clustering Solutions},
	\doi{10.1162/089976604773717621}
}
\examples{

# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# example of wrapper for partitioning algorithm 
pam.clust <- function(data, clust.num) pam(data, clust.num, cluster.only=TRUE)

# example of wrapper for hierarchical algorithm
cutree.wrap <- function(clust.res, clust.num)  cutree(clust.res, clust.num)
agnes.single <- function(data) agnes(data, method="single") 

# converting hierarchical algorithm to partitioning one
agnes.part1 <- function(data, clust.num) cutree.wrap( agnes.single(data), clust.num )
# the same using "cls.alg"
agnes.part2 <- cls.alg(agnes.single, cutree.wrap, fast=FALSE)

# fix arguments for cls.stab.* function
iter = c(2,4,5,7,9,12,15)

res1 = cls.stab.sim.ind.usr( iris.data, iter, pam.clust, 
    sim.ind.type=c("rand","dot.pr","sim.ind"), rep.num=5, subset.ratio=0.7 )
res2 = cls.stab.opt.assign.usr( iris.data, iter, clust.alg=cls.alg(agnes.single, cutree.wrap) )

res3 = cls.stab.sim.ind.usr( iris.data, iter, agnes.part1,
     sim.ind.type=c("rand","dot.pr","sim.ind"), rep.num=5, subset.ratio=0.7 )
res4 = cls.stab.opt.assign.usr( iris.data, iter, clust.alg=agnes.part2 )

print(res1)
boxplot(res1$sim.ind)
plot(res2)


}
\author{Lukasz Nieweglowski}
\keyword{cluster}
