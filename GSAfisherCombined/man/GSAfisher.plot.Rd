\name{GSAfisher.plot}
\alias{GSAfisher.plot}
\title{GSAfisher.plot}
\description{
Plot method of GSAfisher function that plots the p-values for a joint test of association between a phenotype and multiple gene-sets of genetic variants (SNPs) by combining multiple marginal p-values data vectors using the Fisher method.
}
\usage{
\method{GSAfisher}{plot}(\dots)
}
\arguments{
  \item{\dots}{Set of multiple marginal p-values data vectors or matrix.}
}
\value{
  \item{plot.p.multiple}{Plot of a set of multiple Fisher method combined p-values for each data vector.}
}
\author{Carlos Garcia Prieto}
\examples{
#Generate 3 vectors with 1000 random values from an Uniform distribution U(0, 0.25). 
data.vector1<-runif(1000, 0, 0.25)
data.vector2<-runif(1000, 0, 0.25)
data.vector3<-runif(1000, 0, 0.25)
#Set data class to "plot".
class(data.vector1)<-"plot"
#Apply the GSAfisher.plot function to these data vectors.
GSAfisher(data.vector1, data.vector2, data.vector3)

#You can also try:
#Generate 5000 vectors with 1000 random values from an Uniform distribution U(0,0.25).
data.vectors<-sapply(1:5000, function (x) runif(1000, 0, 0.25))
#Set data class to "plot".
class(data.vectors)<-"plot"
#Apply the GSAfisher.plot function to these data vectors.
GSAfisher(data.vectors)
}
\keyword{ plot }