\name{iRafNet_network}
\alias{iRafNet_network}
\title{Compute permutation-based FDR of importance scores and return estimated regulations.}
\usage{
iRafNet_network(out.iRafNet,out.perm,TH)
}
\arguments{
\item{out.iRafNet}{Output object from function \code{iRafNet}.}

\item{out.perm}{Output object from function \code{Run_permutation.}}

\item{TH}{Threshold for FDR.}
}

\value{
List of estimated regulations. 
}
\description{
This function computes permutation-based FDR of importance scores and returns gene-gene regulations.  
}
\examples{

  # --- Generate data sets
  n<-20           # sample size 
  p<-5            # number of genes
  genes.name<-paste("G",seq(1,p),sep="")   # genes name
  M=5;            # number of permutations
  data<-matrix(rnorm(p*n),n,p)       # generate gene expression matrix
  data[,1]<-data[,2]                 # var 1 and var 2 interact
  W<-abs(matrix(rnorm(p*p),p,p))     # generate weights for regulatory relationships
  
  # --- Standardize variables to mean 0 and variance 1
  data <- (apply(data, 2, function(x) { (x - mean(x)) / sd(x) } ))

  # --- Run iRafNet and obtain importance score of regulatory relationships
  out.iRafNet<-iRafNet(data,W,mtry=round(sqrt(p-1)),ntree=1000,genes.name)

  # --- Run iRafNet for M permuted data sets
  out.perm<-Run_permutation(data,W,mtry=round(sqrt(p-1)),ntree=1000,genes.name,M)

  # --- Derive final networks
  final.net<-iRafNet_network(out.iRafNet,out.perm,0.001)

}
\references{
Petralia, F., Song, W.M., Tu, Z. and Wang, P. (2016). New method for joint network analysis reveals common and different coexpression patterns among genes and proteins in breast cancer. \emph{Journal of proteome research}, \bold{15}(3), pp.743-754.

A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. \emph{R News} \bold{2}, 18--22.

Xie, Y., Pan, W. and Khodursky, A.B., 2005. A note on using permutation-based false discovery rate estimates to compare different analysis methods for microarray data. \emph{Bioinformatics}, \bold{21}(23), pp.4280-4288.
}
