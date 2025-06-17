\name{sparseBC.BIC}
\alias{sparseBC.BIC}
\title{
Do tuning parameter (lambda) selection for sparse biclustering via BIC criterion
}
\description{
We assume that both K and R are known.  A range of values of lambda is usually considered - value that results in the lowest BIC is selected.
}
\usage{
sparseBC.BIC(x, k, r, lambda)
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
A range of values of tuning parameters to be considered.  All values must be non-negative.
}
}
\details{
This implements the tuning parameter selection using BIC criterion for sparse biclustering as described in Section 5.2 in Tan and Witten (2014) 'Sparse biclustering of transposable data'.  The BIC criterion is BIC = np x log(RSS) + np log(q) where RSS is the usual residual sum of squares, and q is the number of non-zero bicluster mean in the output of sparseBC.  We select the value of lambda that leads to the smallest BIC.  

The data is centered to have mean 0 in this function.

}
\value{
\item{lambda}{Value of lambda that results in lowest BIC.}
\item{BIC}{BIC values for a range of tuning parameters considered.}
\item{nonzeromus}{Number of nonzero bicluster means for a range of tuning parameters considered.}
}
\references{
KM Tan and D Witten (2014) Sparse biclustering of transposable data.  \emph{Journal of Computational and Graphical Statistics} 23(4):985-1008.
}
\author{
Kean Ming Tan and Daniela Witten
}


\seealso{
\code{\link{sparseBC}}
\code{\link{sparseBC.choosekr}}
}
\examples{
########### Create data matrix
x <- matrix(rnorm(20*30),nrow=20,ncol=30)

########### Perform sparseBC.BIC to select lambda
lambda<-sparseBC.BIC(x,k=2,r=2,lambda=c(0,10,20,30,40))$lambda 
}
