\name{iRafNet_permutation}
\alias{iRafNet_permutation}
\title{Derive importance scores for one permuted data.}
\usage{
iRafNet_permutation(X, W, ntree, mtry,genes.name,perm)
}
\arguments{
\item{X}{\code{(n x p)} Matrix containing expression levels for \code{n} samples and \code{p} genes.}

\item{W}{\code{(p x p)} Matrix containing iRafNet sampling scores. Element \code{(i,j)} contains score for regulatory relationship \code{(i -> j)}. Scores must be non-negative. Larger value of sampling score corresponds to higher likelihood of gene \code{i} regulating gene \code{j}. Columns and rows of \code{W} must be in the same order as the columns of \code{X}. Sampling scores \code{W} are computed considering one prior data such as protein-protein interactions or gene expression from knock-out experiments.}

\item{ntree}{Numeric value: number of trees.}

\item{mtry}{Numeric value: number of predictors to be sampled at each node.}

\item{genes.name}{Vector containing genes name. The order needs to match the rows of \code{x_j}.}

\item{perm}{Integer: seed for permutation.}

}
\value{
A vector containing importance score for permuted data. 
}
\description{
This function computes importance score for one permuted data set. Sample labels of target genes are randomly permuted and iRafNet is implemented. Resulting importance scores can be used to derive an estimate of FDR.  
}
\examples{

  # --- Generate data sets
  n<-20                  # sample size
  p<-5                   # number of genes
  genes.name<-paste("G",seq(1,p),sep="")   # genes name
  data<-matrix(rnorm(p*n),n,p)       # generate expression matrix
  W<-abs(matrix(rnorm(p*p),p,p))     # generate weights for regulatory relationships
 
  # --- Standardize variables to mean 0 and variance 1
  data <- (apply(data, 2, function(x) { (x - mean(x)) / sd(x) } ))

  # --- Run iRafNet and obtain importance score of regulatory relationships
  out.iRafNet<-iRafNet(data,W,mtry=round(sqrt(p-1)),ntree=1000,genes.name)

  # --- Run iRafNet for one permuted data set and obtain importance scores
  out.perm<-iRafNet_permutation(data,W,mtry=round(sqrt(p-1)),ntree=1000,genes.name,perm=1)

}
\references{
Petralia, F., Wang, P., Yang, J., Tu, Z. (2015) Integrative random forest for gene regulatory network inference, \emph{Bioinformatics}, \bold{31}, i197-i205.

Petralia, F., Song, W.M., Tu, Z. and Wang, P. (2016). New method for joint network analysis reveals common and different coexpression patterns among genes and proteins in breast cancer. \emph{Journal of proteome research}, \bold{15}(3), pp.743-754.

A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. \emph{R News} \bold{2}, 18--22.
}
