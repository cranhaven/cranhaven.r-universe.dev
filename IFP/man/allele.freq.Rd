\name{allele.freq}
\alias{allele.freq}
\title{Allele Frequency Computation from Genotype Data}
\description{
 Computes allele frequencies from genotype data. 
}
\usage{
 allele.freq(geno)
}
\arguments{
 \item{geno}{
  matrix of alleles, such that each locus has a pair of adjacent columns of alleles, and the order of columns corresponds to the order of loci on a chromosome.  If there are K loci, then ncol(geno) = 2*K. Rows represent the alleles for each subject. Each allele shoud be represented as numbers (A=1,C=2,G=3,T=4).
 }
}
\value{
 array of allele frequencies of each SNP. The computed allele is targeted as an order of alleles, "A", "C", "G", and "T".
}
\examples{
 data(apoe)
 allele.freq(apoe7)
 allele.freq(apoe)
}
