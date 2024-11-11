\name{Run_permutation}
\alias{Run_permutation}
\title{Derive importance scores for M permuted data sets.}
\usage{
Run_permutation(X, W, ntree, mtry,genes.name,M)
}
\arguments{
\item{X}{\code{(n x p)} Matrix containing expression levels for \code{n} samples and \code{p} genes.}

\item{W}{\code{(p x p)} Matrix containing iRafNet sampling scores. Element \code{(i,j)} contains score for regulatory relationship \code{(i -> j)}. Scores must be non-negative. Larger value of sampling score corresponds to higher likelihood of gene \code{i} regulating gene \code{j}. Columns and rows of \code{W} must be in the same order as the columns of \code{X}. Sampling scores \code{W} are computed considering one prior data such as protein-protein interactions or gene expression from knock-out experiments.}

\item{ntree}{Numeric value: number of trees.}

\item{mtry}{Numeric value: number of predictors to be sampled at each node.}

\item{genes.name}{Vector containing genes name. The order needs to match the rows of \code{x_j}.}

\item{M}{Integer: total number of permutations.}

}
\value{
A matrix with \code{I} rows and \code{M} columns with \code{I} being the total number of regulations and \code{M} the number of permutations. Element \code{(i,j)} corresponds to the importance score of interaction \code{i} for permuted data \code{j}. 
}
\description{
This function computes importance score for \code{M} permuted data sets. Sample labels of target genes are randomly permuted and iRafNet is implemented. Resulting importance scores can be used to derive an estimate of FDR.  
}
\examples{

  # --- Generate data sets
  n<-20                  # sample size 
  p<-5                   # number of genes
  genes.name<-paste("G",seq(1,p),sep="")   # genes name
  M=5;            # number of permutations
 
  data<-matrix(rnorm(p*n),n,p)       # generate expression matrix
  W<-abs(matrix(rnorm(p*p),p,p))          # generate score for regulatory relationships
 
  # --- Standardize variables to mean 0 and variance 1
  data <- (apply(data, 2, function(x) { (x - mean(x)) / sd(x) } ))

  # --- Run iRafNet and obtain importance score of regulatory relationships
  out.iRafNet<-iRafNet(data,W,mtry=round(sqrt(p-1)),ntree=1000,genes.name)

  # --- Run iRafNet for M permuted data sets
  out.perm<-Run_permutation(data,W,mtry=round(sqrt(p-1)),ntree=1000,genes.name,M)

}
\references{
Petralia, F., Wang, P., Yang, J., Tu, Z. (2015) Integrative random forest for gene regulatory network inference, \emph{Bioinformatics}, \bold{31}, i197-i205.

Petralia, F., Song, W.M., Tu, Z. and Wang, P. (2016). New method for joint network analysis reveals common and different coexpression patterns among genes and proteins in breast cancer. \emph{Journal of proteome research}, \bold{15}(3), pp.743-754.

A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. \emph{R News} \bold{2}, 18--22.
}
