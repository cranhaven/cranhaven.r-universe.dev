\name{sparseBC.choosekr}
\alias{sparseBC.choosekr}
\title{
Do tuning parameter K and R selection for sparse biclustering via cross-validation.
}
\description{
Perform cross-validation to select K (number of row clusters) and R (number of column clusters) for sparse biclustering.  We assume that lambda is known.   
}

\usage{
sparseBC.choosekr(x, k, r, lambda, percent = 0.1, trace=FALSE)
}
\arguments{
  \item{x}{
Data matrix; samples are rows and columns are features.  Cannot contain missing values.
}
  \item{k}{
A range of values of K to be considered.  Values considered must be an increasing sequence.
}
  \item{r}{
A range of values of R to be considered.  Values considered must be an increasing sequence.
}
  \item{lambda}{
Non-negative regularization parameter for lasso.  lambda=0 means no regularization.    
}
  \item{percent}{
Percentage of elements of x to be left out for cross-validation.  1 must be divisible by the specified percentage.  The default value is 0.1.
 }
  \item{trace}{
  	Print out progress as iterations are performed.  Default is FALSE.
  } 
}
\details{
The function performs cross-validation as described in Algorithm (2) in Tan and Witten (2014) 'Sparse biclustering of transposable data'.  Briefly, it works as follows: (1) some percent of the elements of x is removed at random from the data matrix - call those elements missing elements, (2) the missing elements are imputed using the mean of the other elements of the matrix, (3) sparse biclustering is performed with various values of K and R and the mean matrix is estimated, (4) calculate the sum of squared error of the missing values between x and the estimated mean matrix.  This procedure is repeated 1/percent times.  Finally, we select K and R based on the criterion described in Algorithm (2) in Tan and Witten (2014).  A similar procedure is used in Witten et al (2009) 'A penalized matrix decomposition, with applications to sparse principal components and canonical correlation analysis'.     

Note that sparseBC is run with center=TRUE in this function.  
}
\value{
\item{estimated_kr}{The chosen values of K and R based on cross-validation.}
\item{results.mean}{Mean squared error for all values of K and R considered.}
\item{results.se}{Standard error of the sum of squared error for all values of K and R considered.}
}
\references{
KM Tan and D Witten (2014) Sparse biclustering of transposable data.  \emph{Journal of Computational and Graphical Statistics} 23(4):985-1008.

D Witten, R Tibshirani, and T Hastie (2009) A penalized matrix decomposition, with applications to sparse principal components and canonical correlation analysis, \emph{Biostatistics} {10(3)}, 515--534.
}
\author{
Kean Ming Tan and Daniela Witten
}



\seealso{
\code{\link{sparseBC}}
\code{\link{sparseBC.BIC}}
}
\examples{

########### Create data matrix with K=2 R=4 row and column clusters 
#k <- 2
#r <- 4
#n <- 200
#p <- 200
#  mus<-runif(k*r,-3,3)
#  mus<-matrix(c(mus),nrow=k,ncol=r,byrow=FALSE)
#  truthCs<-sample(1:k,n,rep=TRUE)
#  truthDs<-sample(1:r,p,rep=TRUE)
#  x<-matrix(rnorm(n*p,mean=0,sd=2),nrow=n,ncol=p)
#  for(i in 1:max(truthCs)){
#     for(j in 1:max(truthDs)){ 
#         x[truthCs==i, truthDs==j] <- x[truthCs==i, truthDs==j] + mus[i,j]
#    }
#  }
#  x<-x-mean(x)

# Example is commented out for short run-time	
############ Perform sparseBC.choosekr to choose the number of row and column clusters
#sparseBC.choosekr(x,1:5,1:5,0,0.2)$estimated_kr

}


