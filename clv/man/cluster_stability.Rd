\name{cls.stab.sim.ind}
\title{Cluster Stability - Similarity Index and Pattern-wise Stability Approaches}
\description{

	\code{cls.stab.sim.ind} and \code{cls.stab.opt.assign} reports validation measures for clustering results. Both functions return lists of  
	cluster stability results computed according to similarity index and pattern-wise stability approaches.
}
\usage{
cls.stab.sim.ind( data, cl.num, rep.num, subset.ratio, clust.method,
                   method.type, sim.ind.type, fast, ... )
cls.stab.opt.assign( data, cl.num, rep.num, subset.ratio, clust.method,
                      method.type, fast, ... )
}
\alias{cls.stab.sim.ind}
\alias{cls.stab.opt.assign}
\arguments{
  \item{data}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and rows to 
		observations.
	}
  \item{cl.num}{
		integer \code{vector} with information about numbers of cluster to which \code{data} will be partitioned.
		If vector is not an integer type, it will be coerced with warning.
	}
  \item{rep.num}{
		integer number which tells how many pairs of data subsets will be partitioned for particular number of clusters.
		The results of partitioning for given pair of subsets is used to compute similarity indices (in case of \code{cls.stab.sim.ind}) 
		or \emph{pattern-wise stability} (in case of \code{cls.stab.opt.assign}, for more details see references). 
		By default \code{rep.num} value is 10. If wrong argument is applied it will be repaced with default value.
		
	}
  \item{subset.ratio}{
		a number comming from (0,1) section which tells how big data subsets should be. 0 means empty subset, 1 means all data. 
		By default \code{subset.ratio} is set to 0.75. If wrong argument is applied it will be repaced with default value.
	}
  \item{clust.method}{
		string vector with names of cluster algorithms to be used. Available are:
		"agnes", "diana", "hclust", "kmeans", "pam", "clara". Combinations are also possible.
		By default \code{c("agnes","pam")} vector is applied.
	}
  \item{method.type}{
		string vector with information useful only in context of "agnes" and "hclust" algorithms . Available are:
		"single", "average", "complete", "ward" and "weighted" (for more details see \code{\link[cluster]{agnes}},
		\code{\link{hclust}} ). The last type is applicable only for "agnes". Combinations are also possible.
		By default \code{c("single","average")} vector is applied.
	}
  \item{sim.ind.type}{
		string vector with information useful only for \code{cls.stab.sim.ind} function. User is able to choose which 
		similarity indices (external measures) to use to compare two partitionings. Available are:
		"dot.pr", "sim.ind", "rand", "jaccard" (for more details see \code{\link{similarity.index}},
		\code{\link{dot.product}}, \code{\link{std.ext}}). Combinations are also possible.
		By default \code{c("dot.pr","sim.ind")} vector is applied.
	}
  \item{fast}{
		logical argument which sets the way of computing cluster stability for hierarchical algorithms. By default it is set to
		TRUE, which means that each result produced by hierarchical algorithm is partitioned for the number of clusters chosen in 
		\code{cl.num} argument and given clustering results are put for further computation. In this way computation of cluster 
		stability is faster. If wrong argument is applied it will be repaced with default value.
	}
  
  \item{...}{
		additional parameters for clustering algorithms. Note: use with caution! Different clustering methods chosen in \code{clust.method} have 
		different set of parameter names - mixing them often disallow any cluster algorithm to run.
	}
}
\details{
	Both functions realize cluster stability approaches described in \emph{Detecting stable clusters using principal component analysis} (see references).
	
	The \code{cls.stab.sim.ind} function realizes algorithm given in \emph{chapter 3.1} where only cosine similarity index (see \code{\link{dot.product}}) 
	is introduced as a similarity index between two different partitionings. This function realize this cluster stability approach also for other 
	similarity indices such us \code{\link{similarity.index}}, \code{\link{clv.Rand}} and \code{\link{clv.Jaccard}}.
	The important thing is that \code{similarity index} (if chosen) produced by this function is not exactly the same as index produced by
	\code{\link{similarity.index}} function. The value of the \code{\link{similarity.index}} is a number which depends on number of clusters.
	Eg. if two "n-clusters" partitionings are compared the value always will be a number which belong to the \code{[1/n, 1]} section. That means the 
	results produced by this similarity index are not comparable for different number of clusters. That's why each result is scaled thanks to 
	the linear function \code{f:[1/n, 1] -> [0, 1]} where "n" is a number of clusters.
	The results' layout is described in \emph{Value} section.

	The \code{cls.stab.opt.assign} function realizes algorithm given in \emph{chapter 3.2} where \emph{pattern-wise agreement} and 
	\emph{pattern-wise stability} was introduced. Function returns the lowest \emph{pattern-wise stability} value for given number of
	clusters. The results' layout is described in \emph{Value} section.

	It often happens that clustering algorithms can't produce amount of clusters that user wants. In this situation only the warning is 
	produced and cluster stability is computed for partitionings with unequal number of clusters.

  The cluster stability will not be calculated for all cluster numbers that are bigger than the subset size.
  For example if \code{data} contains about 20 objects and the \code{subset.ratio} equals 0.5 then the highest cluster number to 
  calculate is 10. In that case all elements above 10 will be removed from \code{cl.num} vector.
}
\value{
	\code{cls.stab.sim.ind} returns a list of lists of matrices. Each matrix consists of the set of external similarity indices (which one similarity 
	index see below) where number of columns is equal to \code{cl.num} vector length and row number is equal to \code{rep.num} value what means
	that each column contain a set of similarity indices computed for fixed number of clusters.
	The order of the matricides depends on three input arguments: \code{clust.method}, \code{method.type}, and \code{sim.ind.type}.
	Combination of \code{clust.method} and \code{method.type} give a names for elements listed in the first list. Each element of this list is also a 
	list type where each element name correspond to one of similarity index type chosen thanks to \code{sim.ind.type} argument. 
	The order of the names exactly match to the order given in those arguments description. It is easy to understand after considering the 
	following example. \cr
	Let say we are running \code{cls.stab.sim.ind} with default arguments then the results will be given in the following order:
	\code{$agnes.single$dot.pr}, \code{$agnes.single$sim.ind}, \code{$agnes.average$dot.pr}, \code{$agnes.average$sim.ind}, \code{$pam$dot.pr}, 
	\code{$pam$sim.ind}. \cr\cr

	\code{cls.stab.opt.assign} returns a list of vectors. Each vector consists of the set of cluster stability indices described in 
	\emph{Detecting stable clusters using principal component analysis} (see references). Vector length is equal to \code{cl.num} vector length what 
	means that each position in vector is assigned to proper clusters' number given in \code{cl.num} argument.
	The order of the vectors depends on two input arguments: \code{clust.method}, \code{method.type}. The order of the names exactly match to the order 
	given in arguments description. It is easy to understand after considering the following example. \cr
	Let say we are running \code{cls.stab.opt.assign} with \code{c("pam", "kmeans", "hclust", "agnes")} as \code{clust.method} and \code{c("ward","average")} 
	as \code{method.type} then the results will be given in the following order:
	\code{$hclust.average}, \code{$hclust.ward}, \code{$agnes.average}, \code{$agnes.ward}, \code{$kmeans}, \code{$pam}.\cr
	
}
\seealso{
    Advanced cluster stability functions:
    \code{\link{cls.stab.sim.ind.usr}}, \code{\link{cls.stab.opt.assign.usr}}. 

		Functions that compare two different partitionings:
		\code{\link{clv.Rand}}, \code{\link{dot.product}}, \code{\link{similarity.index}}.
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

# fix arguments for cls.stab.* function
iter = c(2,3,4,5,6,7,9,12,15)
smp.num = 5
ratio = 0.8

res1 = cls.stab.sim.ind( iris.data, iter, rep.num=smp.num, subset.ratio=0.7,
                         sim.ind.type=c("rand","dot.pr","sim.ind") )
res2 = cls.stab.opt.assign( iris.data, iter, clust.method=c("hclust","kmeans"),
                             method.type=c("single","average") )

print(res1)
boxplot(res1$agnes.average$sim.ind)
plot(res2$hclust.single)

}
\author{Lukasz Nieweglowski}
\keyword{cluster}
