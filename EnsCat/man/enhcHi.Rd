\name{enhcHi}
\alias{enhcHi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs ensemble hierarchical clustering for high dimensional categorical data
}
\description{
This function performs an ensemble hierarchical clustering of high dimensional categorical
data (p >> n).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
enhcHi(data, En=100, len=c(2,10), type=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A nxp data matrix of data frame; n is the number of observations and p is the number of features or dimensions.}
%%     ~~Describe \code{x} here~~
%}
  \item{En}{Number of clusterings to include in the ensemble, i.e., cardinality of the ensemble.
%%     ~~Describe \code{B} here~~
}
  \item{len}{Range of sizes of clusterings (i.e., number of clusters) to run and ensemble.}
  \item{type}{Numeric indicator of single bootstrap (type=1) or double bootstrap (type=2)
  for selecting subsets of variables to include in each clustering within the ensemble. The
  default is type=2}
}


\references{Amiri, S., Clarke, B., and Clarke, J. (2015). Clustering categorical data via ensembling dissimilarity matrices. arXiv preprint arXiv:1506.07930.
}


\examples{
#data("rhabdodata")
### The following code generates the dissimilary matrix of sequence data stored in alphadata
### The ensemble has 100 member clusterings, and the number of clusters in each clustering
### is generated randomly from a discrete uniform on (2,10). A double bootstrap procedure is
### used to select a subset of variables for each clustering.
#ens<-enhcHi(rhabdodata$dat,En=100,len=c(2,10), type=2)
### Calculate the hamming distance
#dis0<-hammingD(ens)
### Save as distance format
#REDIST<-as.dist(dis0)
#hc0 <- hclust(REDIST,method = "average")
#plot(hc0,label=rhabdodata$lab,hang =-1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\keyword{highdimension}
