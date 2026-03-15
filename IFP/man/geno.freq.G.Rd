\name{geno.freq}
\alias{geno.freq}
\title{Genotype Frequency Computation from the sequencing data with a vcf type of the 1000 Genomes Project}
\description{
 Computes genotype frequencies from the sequencing data with a vcf type of the 1000 Genomes Project. 
}
\usage{
 geno.freq(genoG)
}
\arguments{
 \item{genoG}{
  matrix of haplotypes. Each row indicates a variant, and each column ind
icates a haplotype of an individual. Two alleles of 0 and 1 are available.
}
}
\value{
 matrix of genotype frequencies of each variant.
}
\examples{
 data(apoeG)
 geno.freq(apoeG)
}
