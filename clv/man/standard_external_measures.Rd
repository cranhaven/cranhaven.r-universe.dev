\name{std.ext}
\title{Standard External Measures: Rand index, Jaccard coefficient etc. }
\description{
	Group of functions which compute standard external measures such as: 
	Rand statistic and Folkes and Mallows index, Jaccard coefficient etc.
}
\usage{
std.ext(clust1, clust2)
clv.Rand(external.ind)
clv.Jaccard(external.ind)
clv.Folkes.Mallows(external.ind)
clv.Phi(external.ind)
clv.Russel.Rao(external.ind)
}
\alias{std.ext}
\alias{clv.Rand}
\alias{clv.Jaccard}
\alias{clv.Folkes.Mallows}
\alias{clv.Phi}
\alias{clv.Russel.Rao}
\arguments{
  \item{clust1}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning. 
	}
  \item{clust2}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning.
	}
  \item{external.ind}{\code{vector} or \code{list} with four values SS,SD,DS,DD which are result of
	function \code{std.ext}
	}
}
\details{
	Two input \code{vectors} keep information about two different partitionings (let say P and P')
	of the same data set X. We refer to a pair of points (xi, xj) (we assume that i != j) from the 
	data set using the following terms:
	\tabular{ll}{
		\code{SS} \tab - number of pairs where both points belongs to the same cluster in 
		both partitionings, \cr
		\code{SD} \tab - number of pairs where both points belongs to the same cluster in partitioning P but 
		in P' do not, \cr
		\code{DS} \tab - number of pairs where in partitioning P both point belongs to different clusters 
		but in P' do not, \cr
		\code{DD} \tab - number of pairs where both objects belongs to different clusters in 
		both partitionings.
	}
	Those values are used to compute (M = SS + SD + DS +DD):
	\tabular{ll}{
	\emph{Rand statistic} \tab R = (SS + DD)/M \cr
	\emph{Jaccard coefficient} \tab J = SS/(SS + SD + DS) \cr
	\emph{Folkes and Mallows index} \tab FM = sqrt(SS/(SS + SD))*sqrt(SS/(SS + DS)) \cr
	\emph{Russel and Rao index} \tab RR = SS/M \cr
	\emph{Phi index} \tab Ph = (SS*DD - SD*DS)/((SS+SD)(SS+DS)(SD+DD)(DS+DD)).
	}
}
\value{
	\tabular{l}{
	\code{std.ext} returns a \code{list} containing four values: SS, SD, DS, DD. \cr
	\code{clv.Rand} returns R value. \cr
	\code{clv.Jaccard} returns J value. \cr
	\code{clv.Folkes.Mallows} returns FM value. \cr
	\code{clv.Phi} returns Ph value. \cr
	\code{clv.Russel.Rao} returns RR value. 
	}
}
\seealso{
		Other measures created to compare two partitionings: 
		\code{\link{dot.product}}, \code{\link{similarity.index}}
	}
\references{
	G. Saporta and G. Youness \emph{Comparing two partitions: Some Proposals and Experiments}. 
	\url{http://cedric.cnam.fr/PUBLIS/RC405.pdf}
}
\examples{
# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
pam.mod <- pam(iris.data,3) # create three clusters
v.pred <- as.integer(pam.mod$clustering) # get cluster ids associated to given data objects
v.real <- as.integer(iris$Species) # get also real cluster ids

# compare true clustering with those given by the algorithm
# 1. optimal solution:

# use only once std.ext function
std <- std.ext(v.pred, v.real)
# to compute three indicies based on std.ext result
rand1 <- clv.Rand(std)
jaccard1 <- clv.Jaccard(std)
folk.mal1 <- clv.Folkes.Mallows(std)

# 2. functional solution:

# prepare set of functions which compare two clusterizations
Rand <- function(clust1,clust2) clv.Rand(std.ext(clust1,clust2))
Jaccard <- function(clust1,clust2) clv.Jaccard(std.ext(clust1,clust2))
Folkes.Mallows <- function(clust1,clust2) clv.Folkes.Mallows(std.ext(clust1,clust2))

# compute indicies
rand2 <- Rand(v.pred,v.real)
jaccard2 <- Jaccard(v.pred,v.real)
folk.mal2 <- Folkes.Mallows(v.pred,v.real)
}
\author{Lukasz Nieweglowski}
\keyword{cluster}
