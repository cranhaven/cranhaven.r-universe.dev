\name{lrtG}
\alias{lrtG}
\title{Likelihood Ratio Tests for Identifying Disease Polymorphisms with Same Effects}
\description{
 Compute p-values and likelihoods of all possible models for a given number of disease SNP(s). 
}
\usage{
 lrtG(n.fp, genoT, genoC)
}
\arguments{
 \item{n.fp}{number of disease SNPs for tests.}
 \item{genoT}{
  matrix of control genotypes. Each row indicates a variant, and each column indicates a haplotype of an individual. Two alleles of 0 and 1 are allowed.
 }
 \item{genoC}{
  matrix of case genotypes. Each row indicates a variant, and each column indicates a haplotype of an individual. Two alleles of 0 and 1 are allowed.
 }
}
\value{
 matrix of likelihood ratio test results. First row indicates the index, and following n.fp rows indicate the model for each set of disease polymorphisms, and followed by p-values, -2 log(likelihood ratio) with corrections for variances, and the degree of freedom.
}
\references{
L. Park, J. Kim, Rare high-impact disease variants: properties and identification, Genetics Research, 2016 Mar; 98, e6.
}
\seealso{
 allele.freq.G
}
\examples{
## LRT tests for a dominant variant (15th variant)
## the odds ratio: 3, control: 100, case: 100.

data(apoeG)
lrtG(1,genoT[,1:20],genoC[,1:20])

# use "lrtG(1,genoT,genoC)" for the actual test.

}
