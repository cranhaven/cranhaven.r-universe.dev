\name{clv.SD, clv.SDbw}
\title{SD, SDbw - Internal Measures}
\description{
	Function computes \emph{SD} and \emph{\eqn{\textrm{S\_Dbw}}{S_Dbw} validity indices}.
}
\usage{
clv.SD(scatt, dis, alfa)
clv.SDbw(scatt, dens)
}
\alias{clv.SD}
\alias{clv.SDbw}
\arguments{
  \item{scatt}{ 
		\emph{average scattering for cluster} value computed using \code{\link{clv.Scatt}}
		function.
	}
  \item{dis}{ 
		\emph{total separation between clusters} value computed using \code{\link{clv.Dis}}
		function.
	}
  \item{dens}{ 
		\emph{inter-cluster density} value computed using \code{\link{clv.DensBw}}
		function.
	} 
  \item{alfa}{ 
		weighting factor (normally equal to Dis(cmax) where cmax is the maximum number 
		of input clusters).
	}
}
\details{
	\emph{SD validity index} is defined by equation:	

	SD = \emph{scatt}*alfa + \emph{dis}

	where \emph{scatt} means \emph{average scattering for clusters} defined in \code{\link{clv.Scatt}}.
	\emph{\eqn{\textrm{S\_Dbw}}{S_Dbw} validity index} is defined by equation:

	\eqn{\textrm{S\_Dbw}}{S_Dbw} = \emph{scatt} + \emph{dens}

	where \emph{dens} is defined in \code{\link{clv.DensBw}}.

}
\value{
	As result of \code{clv.SD} function \emph{SD validity index} is returned.
	As result of \code{clv.SDbw} function \emph{\eqn{\textrm{S\_Dbw}}{S_Dbw} validity index} is returned.
}
\seealso{ \code{\link{clv.Scatt}}, \code{\link{clv.Dis}} and \code{\link{clv.DensBw}} }
\references{
	M. Haldiki, Y. Batistakis, M. Vazirgiannis \emph{On Clustering Validation Techniques},
	\url{http://citeseer.ist.psu.edu/513619.html}
}
\examples{
# load and prepare
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
agnes.mod <- agnes(iris.data) # create cluster tree 
v.pred <- as.integer(cutree(agnes.mod,5)) # "cut" the tree 

# prepare proper input data for SD and S_Dbw indicies
scatt <- clv.Scatt(iris.data, v.pred)
dis <- clv.Dis(scatt$cluster.center)
dens.bw <- clv.DensBw(iris.data, v.pred, scatt)

# compute  SD and S_Dbw indicies
SD <- clv.SD(scatt$Scatt, dis, alfa=5) # alfa is equal to number of clusters 
SDbw <- clv.SDbw(scatt$Scatt, dens.bw)
}
\author{Lukasz Nieweglowski}
\keyword{cluster}
