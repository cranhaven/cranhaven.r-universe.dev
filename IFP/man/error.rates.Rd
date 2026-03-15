\name{error.rates}
\alias{error.rates}
\title{Error Rates Estimation for Likelihood Ratio Tests Designed for Identifying Number of Functional Polymorphisms}
\description{
 Compute error rates for a given model. 
}
\usage{
 error.rates(H0,Z, pMc, geno, no.ca, no.con=nrow(geno), sim.no = 1000)
}
\arguments{
 \item{H0}{the index number for a given model for functional SNPs}
 \item{Z}{number of functional SNPs for the given model}
 \item{pMc}{array of allele frequencies of case samples}
 \item{geno}{
  matrix of alleles, such that each locus has a pair of adjacent columns of alleles, and the order of columns corresponds to the order of loci on a chromosome.  If there are K loci, then ncol(geno) = 2*K. Rows represent the alleles for each subject. Each allele shoud be represented as numbers (A=1,C=2,G=3,T=4).
}
 \item{no.ca}{number of case chromosomes}
 \item{no.con}{number of control chromosomes}
 \item{sim.no}{number of simulations for error rates estimation}
}
\value{
 array of results consisted of Type I error rate (alpha=0.05), Type I error rate (alpha=0.01), Type II error rate (beta=0.05), Type II error rate (beta=0.01), percent when the target model has the lowest corrected -2 log likelihood ratio. 
}
\seealso{
 allele.freq
 hap.freq
 lrtB
}
\examples{
## LRT tests when SNP1 & SNP6 are the functional polymorphisms.


data(apoe)

n<-c(2000, 2000, 2000, 2000, 2000, 2000, 2000) #case sample size = 1000
x<-c(1707, 281,1341, 435, 772, 416, 1797) #allele numbers in case samples 

Z<-2 	#number of functional SNPs for tests
n.poly<-ncol(apoe7)/2 	#total number of SNPs

#index number for the model in this case is 5 for SNP1 and 6. 
#apoe7 is considered to represent the true control allele and haplotype frequencies.
#Control sample size = 1000.

error.rates(5, 2, x/n, apoe7, 2000, 2000, sim.no=2)

# to obtain valid rates, use sim.no=1000.

}
