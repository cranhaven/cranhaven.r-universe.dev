\name{LP.basis}
\alias{LP.basis}
\alias{wt.mean}

\title{
  Computes LP basis function of a discrete distribution
}
\description{
  This function computes \code{m} LP basis functions for the given discrete distribution \code{p.dist}
}
\usage{
LP.basis(p.dist, m)
}
\arguments{
  \item{p.dist}{ A vector of \eqn{n} probabilities of a discrete distribution }
  \item{m}{ An integer denoting the number of required LP basis functions}
}

\value{
  A matrix of dimension \eqn{n\times m}.
}
\references{
Mukhopadhyay, S. and Parzen, E. (2014), "LP Approach to Statistical Modeling", arXiv:1405.2601.
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
    W   <-exp(-as.matrix(dmat)^2/(2*quantile(dmat,.5)^2))
   ## getting the basis
    pp<- rowSums(W)/sum(W)
    T<-LP.basis(pp,m=4)
   #plot the j-th LP basis for the two sample data (here we use j=1).
    j=1
    plot(cumsum(pp),T[,j],type='s',xlab='',ylab='')

   ##2.Senate data
   \dontrun{
    data(senate)
    attach(senate)
   #create W matrix: (long computation)
    require(psych)
    W <- matrix(0,nrow(X),nrow(X))
    for(i in 1:(nrow(X)-1)){
	for(j in (i+1):nrow(X)) { 
		W[i,j] <- psych::phi(table(X[i,],X[j,])) 
	}
    }
    W = W + t(W)
    diag(W)<-0
   #getting the basis:
    pp<- rowSums(W)/sum(W)
    T<-LP.basis(pp,m=4)
   #plot the j-th LP basis for senate data (here we use j=1).
    j=1
    plot(cumsum(pp),T[,j],type='s',xlab='',ylab='')
   }
}

\keyword{ Main functions }
