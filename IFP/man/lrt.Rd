\name{lrt}
\alias{lrt}
\title{Likelihood Ratio Tests for Identifying Number of Functional Polymorphisms}
\description{
 Compute p-values and likelihoods of all possible models for a given number of functional SNP(s). 
}
\usage{
 lrt(n.fp, n, x, geno, no.con=nrow(geno))
}
\arguments{
 \item{n.fp}{number of functional SNPs for tests.}
 \item{n}{array of each total number of case sample chromosomes for SNPs}
 \item{x}{array of each total allele number in case samples}
 \item{geno}{
  matrix of alleles, such that each locus has a pair of adjacent columns of alleles, and the order of columns corresponds to the order of loci on a chromosome.  If there are K loci, then ncol(geno) = 2*K. Rows represent the alleles for each subject. Each allele shoud be represented as numbers (A=1,C=2,G=3,T=4).
 }
 \item{no.con}{number of control chromosomes.}
}
\value{
 matrix of likelihood ratio test results. First n.fp rows indicate the model for each set of disease polymorphisms, and followed by p-values, -2 log(likelihood ratio) with corrections for variances, maximum likelihood ratio estimates, and likelihood.
}
\references{
L. Park, Identifying disease polymorphisms from case-control genetic association data, Genetica, 2010 138 (11-12), 1147-1159.
}
\seealso{
 allele.freq
 hap.freq
}
\examples{
## LRT tests when SNP1 & SNP6 are the functional polymorphisms.

data(apoe)

n<-c(2000, 2000, 2000, 2000, 2000, 2000, 2000) #case sample size = 1000
x<-c(1707, 281,1341, 435, 772, 416, 1797) #allele numbers in case samples 


Z<-2 	#number of functional SNPs for tests
n.poly<-ncol(apoe7)/2 	#total number of SNPs

#control sample generation( sample size = 1000 )
con.samp<-sample(nrow(apoe7),1000,replace=TRUE)
con.data<-array()
for (i in con.samp){
con.data<-rbind(con.data,apoe7[i,])
}
con.data<-con.data[2:1001,]

lrt(1,n,x,con.data)
lrt(2,n,x,con.data)
}
