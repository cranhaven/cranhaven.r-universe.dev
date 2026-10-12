\name{estMemSNPs.oneSetHyperPara}
\alias{estMemSNPs.oneSetHyperPara}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate SNP cluster membership
}
\description{
Estimate SNP cluster membership. Only update cluster mixture proportions.
Assume all 3 clusters have the same set of hyperparameters.
}
\usage{
estMemSNPs.oneSetHyperPara(es, 
           var.memSubjs = "memSubjs", 
           eps = 1.0e-3,
           MaxIter = 50, 
           bVec = rep(3, 3), 
           pvalAdjMethod = "none", 
           method = "FDR",
           fdr = 0.05,
           verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{
An ExpressionSet object storing SNP genotype data.
It contains 3 matrices. The first matrix, which can be extracted by \code{exprs} method (e.g., \code{exprs(es)}), stores genotype data, with rows are SNPs and columns are subjects.

The second matrix, which can be extracted by \code{pData} method (e.g., \code{pData(es)}), stores phenotype data describing subjects. Rows are subjects, and columns are phenotype variables.

The third matrix, which can be extracted by \code{fData} method (e.g., \code{fData(es)}), stores feature data describing SNPs. Rows are SNPs and columns are feature variables.
}
  \item{var.memSubjs}{
character. The name of the phenotype variable indicating subject's case-control status. It must take only two values: 1 indicating case and 0 indicating control.
}
  \item{eps}{
numeric. A small positive number as threshold for convergence of EM algorithm.
}
  \item{MaxIter}{
integer. A positive integer indicating maximum iteration in EM algorithm.
}
  \item{bVec}{
numeric. A vector of 2 elements. Indicates the parameters of the symmetric Dirichlet prior for proportion mixtures.
}
  \item{pvalAdjMethod}{
character. Indicating p-value adjustment method. c.f. \code{\link{p.adjust}}.
}
  \item{method}{method to obtain SNP cluster membership based on the responsibility matrix. The default value is \dQuote{FDR}. The other possible 
value is \dQuote{max}. see details.}
  \item{fdr}{numeric. A small positive FDR threshold used to call SNP cluster membership}
  \item{verbose}{
logical. Indicating if intermediate and final results should be output.
}
}
\details{
We characterize the distribution of genotypes of SNPs by a mixture of 3 Bayesian hierarchical models.  The 3 Bayeisan hierarchical models correspond to 3
clusters of SNPs.

In cluster \eqn{+}, the minor allele frequency
(MAF) \eqn{\theta_{x+}} of cases is greater than the MAF \eqn{\theta_{y+}} of
controls.

In cluster \eqn{0}, the MAF \eqn{\theta_{0}} of cases is equal to 
the MAF of controls.

In cluster \eqn{-}, the MAF \eqn{\theta_{x-}} of cases is smaller than 
the MAF \eqn{\theta_{y-}} of controls.

The proportions of the 3 clusters of SNPs are \eqn{\pi_{+}},
\eqn{\pi_{0}}, and \eqn{\pi_{-}}, respectively.


We assume a \dQuote{half-flat shape} bivariate prior for the MAF in
cluster \eqn{+}
\deqn{2h\left(\theta_{x+}\right)h\left(\theta_{y+}\right)
I\left(\theta_{x+}>\theta_{y+}\right),
}
where \eqn{I(a)} is hte indicator function taking value \eqn{1} if the event 
\eqn{a} is true, and value \eqn{0} otherwise.
The function \eqn{h} is the probability density function of the 
beta distribution \eqn{Beta\left(\alpha, \beta\right)}.

We assume \eqn{\theta_{0}} has the beta prior \eqn{Beta(\alpha, \beta)}. 

We also assume a \dQuote{half-flat shape} bivariate prior for the MAF in
cluster \eqn{-}
\deqn{2h\left(\theta_{x-}\right)h\left(\theta_{y-}\right)
I\left(\theta_{x-}>\theta_{y-}\right).
}

Given a SNP, we assume Hardy-Weinberg equilibrium holds for its genotypes. 
That is, given MAF \eqn{\theta}, the probabilities of genotypes are
\deqn{Pr(geno=2) = \theta^2}
\deqn{Pr(geno=1) = 2\theta\left(1-\theta\right)}
\deqn{Pr(geno=0) = \left(1-\theta\right)^2}

We also assume the genotypes \eqn{0} (wild-type), \eqn{1} (heterozygote), and 
\eqn{2} (mutation) follows a multinomial distribution
\eqn{Multinomial\left\{1, \left[
\theta^2, 2\theta\left(1-\theta\right),
\left(1-\theta\right)^2
\right]\right\}}

For each SNP, we calculat its posterior probabilities that it belongs to cluster \eqn{k}. This forms a matrix with 3 columns. Rows are SNPs. 
The 1st column is the posterior probability that the SNP belongs to cluster \eqn{-}. 
The 2nd column is the posterior probability that the SNP belongs to cluster \eqn{0}.
The 3rd column is the posterior probability that the SNP belongs to cluster \eqn{+}.
We call this posterior probability matrix as responsibility matrix.
To determine which cluster a SNP eventually belongs to, we can use 2 methods.
The first method (the default method) is \dQuote{FDR} method, which will
use FDR criterion to determine SNP cluster membership.
The 2nd method is use the maximum posterior probability to decide which
cluster a SNP belongs to.


}
\value{
A list of 10 elements
  \item{wMat }{matrix of posterior probabilities. The rows are SNPs. There are 3 columns. 
The first column is the posterior probability that a SNP belongs to cluster - given genotypes of subjects. 
The second column is the posterior probability that a SNP belongs to cluster 0 given genotypes of subjects. 
The third column is the posterior probability that a SNP belongs to cluster + given genotypes of subjects. 
}
  \item{memSNPs }{a vector of SNP cluster membership for the 3-cluster partitionfrom the mixture of 3 Bayesian hierarchical models.}
  \item{memSNPs2 }{a vector of binary SNP cluster membership. 1 indicates the SNP has different MAFs between cases and controls. 0 indicates the SNP has the same MAF in cases as that in controls.}
  \item{piVec}{a vector of cluster mixture proportions.}
  \item{alpha}{the first shape parameter of the beta prior for MAF obtaind from initial 3-cluster partitions based on GWAS.}
  \item{beta}{the second shape parameter of the beta prior for MAF obtaind from initial 3-cluster partitions based on GWAS.}
  \item{loop}{number of iteration in EM algorithm}
  \item{diff}{sum of the squared difference of cluster mixture proportions between current iteration and previous iteration in EM algorithm. if \code{eps < eps}, we claim the EM algorithm converges.}
  \item{res.limma}{object returned by limma}
}
\references{
  Yan X, Xing L, Su J, Zhang X, Qiu W. 
  Model-based clustering for identifying disease-associated SNPs in case-control genome-wide association studies. 
  Scientific Reports 9, Article number: 13686 (2019) https://www.nature.com/articles/s41598-019-50229-6.
}
\author{
Yan Xu <yanxu@uvic.ca>, 
Li Xing <sfulxing@gmail.com>, 
Jessica Su <rejas@channing.harvard.edu>, 
Xuekui Zhang <xuekui@uvic.ca>, 
Weiliang Qiu <Weiliang.Qiu@gmail.com>
}
%\note{
%%  ~~further notes~~
%}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

data(esSimDiffPriors)
print(esSimDiffPriors)
fDat = fData(esSimDiffPriors)
print(fDat[1:2,])
print(table(fDat$memGenes))

res = estMemSNPs.oneSetHyperPara(
  es = esSimDiffPriors, 
  var.memSubjs = "memSubjs")

print(table(fDat$memGenes, res$memSNPs))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
