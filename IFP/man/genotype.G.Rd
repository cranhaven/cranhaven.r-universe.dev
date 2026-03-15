\name{genotype}
\alias{genotype}
\title{Conversion to Genotypes from Alleles using the sequencing data with a vcf type of the 1000 Genomes Project}
\description{
 Convert sequencing data to genotypes. 
}
\usage{
 genotype(genoG)
}
\arguments{
 \item{genoG}{
  matrix of haplotypes. Each row indicates a variant, and each column ind
icates a haplotype of an individual. Two alleles of 0 and 1 are available.
}
}
\value{
 matrix of genotypes with rows of variants and with columns of individuals.
}
\examples{
 data(apoeG)
 genotype(apoeG)
}
