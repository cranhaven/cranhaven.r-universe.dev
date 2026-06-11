\name{LPSpectral}
\alias{LPSpectral}

\title{
Nonparametric smooth approximation of the Laplacian graph spectra
}
\description{
This function provides nonparametric smooth approximation of the Laplacian graph spectra given a weighted-adjacency matrix \eqn{W}.
}
\usage{
LPSpectral(W, k, m=8,sparse=TRUE)
}
\arguments{
  \item{W}{ A \eqn{n}-by-\eqn{n} weighted-adjacency matrix. }
  \item{k}{ Number of approximated singular vectors and singular values to return, where \eqn{k \leq m}.}
  \item{m}{ Number of LP-nonparametric basis used for approximation, where \eqn{m \leq n}. By default, \eqn{m=8}. }
  \item{sparse}{ Set to \code{TRUE} to make coefficients for LP basis sparse, thus allowing for further smoothing.}
}

\value{
 A list containing the following items:
   \item{LP}{\eqn{m}-by-\eqn{m} LP Spectral graph matrix.}
   \item{Phi}{A \eqn{n}-by-\eqn{k} matrix of LP-approximated singular vectors.}
   \item{sval}{A vector of length \eqn{k} containing top k approximated singular values.}
}
\references{
   Mukhopadhyay, S. and Wang, K. (2018), "Graph Spectral Compression via Smoothing".
}

\author{ Mukhopadhyay, S. and Wang, K.}

\examples{

   ##1.toy example:
   ##simulate a two sample locational difference normal data:
    X1<-matrix(rnorm(250,mean=0,sd=1),10,25)
    X2<-matrix(rnorm(250,mean=0.5,sd=1),10,25)
    X<-rbind(X1,X2)
   ## Adjacency matrix:
    dmat<-dist(X)
    W <-exp(-as.matrix(dmat)^2/(2*quantile(dmat,.5)^2))
   ## Obtain top 10 approximated nontrivial singular values:
    data_sval<-LPSpectral(W, k=10)$sval
   ## Obtain approximated singular vector corresponding to the top nontrivial singular value:   
    data_phi1<-LPSpectral(W, k=1)$Phi
   ## plot the results:
    par(mfrow=c(1,2))
    plot(data_sval,type='b')
    plot(data_phi1)

   ##2.Senate Data
  \dontrun{
    data(senate)
    attach(senate)
   ##creating W (long computation)
    require(psych)
    W <- matrix(0,nrow(X),nrow(X))
    for(i in 1:(nrow(X)-1)){
	for(j in (i+1):nrow(X)) { 
		W[i,j] <- psych::phi(table(X[i,],X[j,])) 
	}
    }
    W = W + t(W)
    diag(W)<-0
   ## Obtain top 10 approximated nontrivial singular values:
    senate_sval<-LPSpectral(W, k=10, m=15)$sval
   ## Obtain approximated singular vector corresponding to the top nontrivial singular value:   
    senate_phi1<-LPSpectral(W, k=1, m=15)$Phi
   ## plot the results:
    par(mfrow=c(1,2))
    plot(senate_sval,type='b')
    plot(senate_phi1)
  }
}


\keyword{ Main functions }
