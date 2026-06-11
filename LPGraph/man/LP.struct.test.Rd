\name{LP.struct.test}
\alias{LP.struct.test}
\alias{Laplacian}

\title{
  Detection of structures in an ordered-network.
}
\description{
  Given adjacency matrix \code{W}, this function perform a graph based test to determine whether there are different communities present in a graph of ordered vertices.
}
\usage{
LP.struct.test(W, m = NULL, n.iter = 50)
}
\arguments{
  \item{W}{ A \eqn{n}-by-\eqn{n} weighted-adjacency matrix. }
  \item{m}{ Number of LP-nonparametric basis used for generating the test statistic, set to \code{NULL} to use original Laplacian. }
  \item{n.iter}{ Iterations used for small sample correction, default is \eqn{50}. }
}

\value{
A list containing the following items:
   \item{stat}{The test statistic, which asymptotically follows a normal distribution with mean and variance mentioned in the reference.}
   \item{pval}{P-value for the test, small p-value means different communities may be present.}
}
\references{
   Mukhopadhyay, S. and Wang, K. (2018), "Graph Spectral Compression via Smoothing".
}

\author{ Mukhopadhyay, S. and Wang, K.}


\examples{
   ##1.example: null case
   ##simulate a normal data with mean 0 and variance 1:
    X  <-matrix(rnorm(500,mean=0,sd=1),20,25)
   ## Generate adjacency matrix:
    dmat<-dist(X)
    W   <-exp(-as.matrix(dmat)^2/(2*quantile(dmat,.5)^2))
   ## test of structure:
    h0.test<-LP.struct.test(W, m = 4 , n.iter = 50)
   ###extract p-value:
    h0.test$pval

   ##2.example: two sample location alternative
   ##simulate a two sample locational difference normal data:
    X1<-matrix(rnorm(250,mean=0,sd=1),10,25)
    X2<-matrix(rnorm(250,mean=0.5,sd=1),10,25)
    X<-rbind(X1,X2)
   ## Generate adjacency matrix:
    dmat<-dist(X)
    W   <-exp(-as.matrix(dmat)^2/(2*quantile(dmat,.5)^2))
   ## test of structure:
    h1.test<-LP.struct.test(W, m = 4 , n.iter = 50)
   ###extract p-value:
    h1.test$pval
}

\keyword{ Main functions }
