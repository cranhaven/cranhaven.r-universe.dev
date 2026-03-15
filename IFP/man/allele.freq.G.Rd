\name{allele.freq.G}
\alias{allele.freq.G}
\title{Allele Frequency Computation from the sequencing data with a vcf type of the 1000 Genomes Project}
\description{
 Computes allele frequencies from the sequencing data with a vcf type of the 1000 Genomes Project. 
}
\usage{
 allele.freq.G(genoG)
}
\arguments{
 \item{genoG}{
  matrix of haplotypes. Each row indicates a variant, and each column ind
icates a haplotype of an individual. Two alleles of 0 and 1 are available.
}
}
\value{
 array of allele frequencies of each variant.
}
\examples{
 data(apoeG)
 allele.freq.G(apoeG)
}
