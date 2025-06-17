\name{sparseBC}
\alias{sparseBC}
\title{
Sparse biclustering
}
\description{
This function performs sparse biclustering on an n by p matrix.  Details are given in Tan and Witten (2014).  
}
\usage{
sparseBC(x, k, r, lambda, nstart = 20, Cs.init = NULL, Ds.init = NULL,
 max.iter = 1000,threshold=1e-10,center=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data matrix; samples are rows and columns are features.  Cannot contain missing values.
}
  \item{k}{
The number of row clusters, i.e., the number of clusters for the observations.
}
  \item{r}{
The number of column clusters, i.e., the number of clusters for the features.
}
  \item{lambda}{
Non-negative regularization parameter for lasso on the mean of each bicluster.  lambda=0 means no regularization.    
}
  \item{nstart}{
The number of random initialization sets used in the kmeans function.  The default is 20.
}
  \item{Cs.init}{
Starting values for the row labels.  The default value is NULL -- kmeans clustering is performed to estimate the row labels.  
}
  \item{Ds.init}{
Starting values for the column labels.  The default value is NULL -- kmeans clustering is performed to estimate the column labels. 
}
  \item{max.iter}{
Maximum number of iterations.  The default value is 1000 iterations.  
}
  \item{threshold}{
Threshold value for convergence.  The default is 1e-10.     
}
  \item{center}{
Mean center the data matrix before performing sparse biclustering.  The default is TRUE.
}

}
\details{
 This implements sparse biclustering using Algorithm (1) described in Tan and Witten (2014) 'Sparse biclustering of transposable data', which estimates the  row labels for the observations and column labels for the features.  The mean of each bicluster is encouraged to be sparse using the lasso penalty.  Details are given in Algorithm (1) in Tan and Witten (2014). 
 
If center=TRUE, the data matrix x is mean centered before performing sparse biclustering.  The reported mean matrix mus is the addition of the substracted mean, mean(x), and the estimated mean matrix from sparse biclustering on the mean centered data. 

Note that center=TRUE will not give any estimated mean to be zero, unless the data is initially centered to have mean(x)=0. Instead, when center=TRUE, elements of the mean matrix are shrunken towards mean(x). 

 }
\value{
an object of class sparseBC.  

Among some internal variables, this object includes the elements

\item{Cs}{Cs is the output for the row labels.}
\item{Ds}{Ds is the output for the column labels.}
\item{objs}{objs is the minimized objective value of the l1 penalized log-likelihood.}
\item{mus}{mus is the estimated mean matrix for the entire matrix.}
\item{Mus}{Mus is the estimated mean matrix for each bicluster.}
\item{iteration}{The number of iterations until convergence.}
}

\references{
KM Tan and D Witten (2014) Sparse biclustering of transposable data.  \emph{Journal of Computational and Graphical Statistics} 23(4):985-1008.
}
\author{
Kean Ming Tan and Daniela Witten
}

\seealso{
\code{\link{sparseBC.BIC}}
\code{\link{sparseBC.choosekr}}
\code{\link{summary.sparseBC}}
\code{\link{image.sparseBC}}
}

\examples{
##############################################
# Example from Figure 1 in the manuscript
# A toy example to illustrate the results from k-means and sparse biclustering
##############################################

# Generate the data matrix x
set.seed(1)
n<-100
p<-200
k<-5
r<-5
truthCs<-rep(1:k, each=(n/k))
truthDs<-rep(1:r, each=(p/r))
mus<-runif(k*r,-3,3)
mus<-matrix(c(mus),nrow=k,ncol=r,byrow=FALSE)
x<-matrix(rnorm(n*p,mean=0,sd=5),nrow=n,ncol=p)

# Generate the mean matrix 
musmatrix<-matrix(NA,nrow=n,ncol=p)
for(i in 1:max(truthCs)){
  for(j in 1:max(truthDs)){ 
  x[truthCs==i,truthDs==j]<-x[truthCs==i,truthDs==j]+mus[i,j]
  musmatrix[truthCs==i,truthDs==j]<-mus[i,j]
  } 
}	

# Perform kmeans on the row and columns and calculate its mean
km.Cs<-kmeans(x,k,nstart=20)$cluster
km.Ds<-kmeans(t(x),r,nstart=20)$cluster
km.mus<-matrix(NA,nrow=n,ncol=p)
for(i in 1:n){
  for(j in 1:p){
  km.mus[i,j]<-mean(x[km.Cs==km.Cs[i],km.Ds==km.Ds[j]])		
  }
}

# Perform sparse biclustering with 5 row clusters and 5 column clusters and lambda=0
bicluster<-sparseBC(x,5,5,0)


# Display some information on the object sparseBC
summary(bicluster)


# Image plots to illustrate the estimated mean matrix
par(mfrow=c(2,2))
image(t(x),main="x")
image(t(musmatrix),main="Mean Matrix")
image(t(km.mus),main="Kmeans")
image(t(bicluster$mus),main="sparseBC")

# Built-in image plot for object sparseBC
image(bicluster)
}