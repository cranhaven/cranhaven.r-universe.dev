\name{hap.freq}
\alias{hap.freq}
\title{Estimation of Haplotype Frequencies with Two SNPs}
\description{
 EM computation of haplotype frequencies with two SNPs. The computation is relied on the package"haplo.stats". 
}
\usage{
 hap.freq(geno)
}
\arguments{
 \item{geno}{
  matrix of alleles, such that each locus has a pair of adjacent columns of alleles, and the order of columns corresponds to the order of loci on a chromosome.  If there are K loci, then ncol(geno) = 2*K. Rows represent the alleles for each subject. Each allele shoud be represented as numbers (A=1,C=2,G=3,T=4).
 }
}
\value{
 matrix of haplotype frequencies consisted of two alleles from each SNP. These alleles are the same ones computed for frequency using the function "allele.freq".
}
\seealso{
 allele.freq
}
\examples{
 data(apoe)
 hap.freq(apoe7)
 hap.freq(apoe)
}
