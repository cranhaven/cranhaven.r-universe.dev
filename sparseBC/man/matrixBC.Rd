\name{matrixBC}
\alias{matrixBC}
\title{
MVN biclustering
}
\description{
This function performs MVN biclustering on a n by p matrix.  Details are given in Tan and Witten (2014).  
}
\usage{
matrixBC(x, k, r, lambda, alpha, beta, nstart = 20, Cs.init = NULL,
 Ds.init = NULL, max.iter = 50, threshold = 1e-04, Sigma.init = NULL, 
 Delta.init = NULL, center=TRUE)
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
  \item{alpha}{
Non-negative regularization parameter for the graphical lasso to estimate the covariance matrix of the samples.  alpha=0 means no regularization.  alpha>0 is recommended.
}
  \item{beta}{
Non-negative regularization parameter for the graphical lasso to estimate the covariance matrix of the features.  beta=0 means no regularization.  beta>0 is recommended.
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
Maximum number of iterations.  The default value is 50 iterations.  
}
  \item{threshold}{
Threshold value for convergence.  The default is 1e-4.     
}
  \item{Sigma.init}{
Starting values for the covariance matrix of the observations.  The default value is NULL -- the graphical lasso as described in Friedman, Hastie, and Tibshirani (2008) is performed to estimate the covariance matrix of the observations. 
}
  \item{Delta.init}{
Starting values for the covariance matrix of the features.  The default value is NULL -- the graphical lasso as described in  Friedman, Hastie, and Tibshirani (2008) is performed to estimate the covariance matrix of the features. 
}
  \item{center}{
Mean center the data matrix before performing sparse biclustering.  The default is TRUE.
}
}
\details{
 This implements MVN biclustering using Algorithm (3) described in Tan and Witten (2014) 'Sparse biclustering of transposable data'.  This approach takes into account the correlation among the features  within the same cluster and also takes into account the correlation among the observations within the same cluster.  The row labels for the observations and column labels for the features are estimated and the mean of each bicluster is encouraged to be sparse using the lasso penalty.  Details are given in Algorithm (3) in Tan and Witten (2014).  
 
If Sigma.init and Delta.init are NULL, the graphical lasso in Friedman, Hastie, and Tibshirani (2008) is used to estimated the covariance matrix of the observations and features.  If Sigma.init is provided, then the covariance matrices would not be updated in the algorithm. Note that when Sigma and Delta equal the identity matrix up to a scaling factor, this approach is exactly that of sparse biclustering and the function sparseBC should be used.  

Note that most of the computation time comes from the graphical lasso algorithm.  We recommend setting the tuning parameters alpha and beta to be large so that the graphical lasso can be implemented efficiently (see the glasso package).  When n > p, alpha=0 will return an error.  Similarly, when p > n, beta=0 will return an error.

If center=TRUE, the data matrix x is mean centered before performing sparse biclustering.  The reported mean matrix mus is the addition of the substracted mean, mean(x), and the estimated mean matrix from sparse biclustering on the mean centered data.   
     
}
\value{
an object of class matrixBC.  

Among some internal variables, this object includes the elements

\item{Cs}{Cs is the output for the row labels.}
\item{Ds}{Ds is the output for the column labels.}
\item{mus}{mus is the estimated mean matrix for the entire matrix.}
\item{Mus}{Mus is the estimated mean matrix for each bicluster.}
\item{Sigma}{Sigma is the estimated covariance matrix of the observations.}
\item{Delta}{Delta is the estimated covariance matrix of the features.}
\item{objs}{objs is the maximized objective value of the negative l1 penalized log-likelihood of the matrix-variate normal distribution.}
\item{iteration}{The number of iterations until convergence.}
}
\references{
KM Tan and D Witten (2014) Sparse biclustering of transposable data.  \emph{Journal of Computational and Graphical Statistics} 23(4):985-1008.

J Friedman, T Hastie, and R Tibshirani (2008). Sparse inverse covariance estimation with the lasso.  \emph{Biostatistics} 9, 432--441.
}
\author{
Kean Ming Tan and Daniela Witten
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sparseBC}}
\code{\link{matrixBC.BIC}}
\code{\link{summary.matrixBC}}
\code{\link{image.matrixBC}}
}
\examples{
# Lung cancer data
# Not run to save time
#data(lung)
#truecluster<-as.numeric(as.factor(rownames(lung)))
#cancersd<-apply(lung,2,sd)
# Pick the top 400 genes that have the largest standard deviation
#lung<-lung[,rank(cancersd)>=length(cancersd)-399]

# Example of MVN Biclustering
#set.seed(5)
#res<-matrixBC(lung,k=4,r=10,lambda=60,alpha=0.4,beta=0.4) 
# one misclassification
#res$Cs

# lambda chosen such that the estimated mean matirx ofsparseBC has a
# similar number of nonzero as matrixBC
#res2<-sparseBC(lung,k=4,r=10,lambda=230)
# a few observations are being misclassified
#res2$Cs

# print information from the object matrixBC
#summary(res)

# Plot the estimated mean matris for the object matrixBC
#image(res)

}