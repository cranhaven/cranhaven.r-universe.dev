\name{CviCol}
\alias{CviCol}
\docType{data}
\title{Arabidopsis thaliana genotype data }
\description{ 
  The genotype data of the Cvi-0 \eqn{\times} Col-0 Recombinant Inbred Line (RIL) population.
}

\usage{data(CviCol)}
\format{
  The format is a matrix containing 90 single-nucleotide polymorphism (SNP) markers for 367 individuals.
}  

\details{
The \emph{Arabidopsis thaliana} genotype data is derived from a RIL cross between Columbia-0 (Col-0) and the Cape Verde Island (Cvi-0), where 367 individuals were genotyped for 90 genetic markers. This is a diploid population with three possible genotpe states \code{(k = 3)}, where the genotypes coded as \code{0, 1, 2}, where 0 and 2 represent the homozygous genotypes and 1 defines the heterozygous genotype. \cr
This data set can be used to detect epistatic selection, short- and long- range linkage disequilibrium between 90 SNP markers. 
}

\source{
Simon, M., et al. "QTL mapping in five new large RIL populations of Arabidopsis thaliana genotyped with consensus SNP markers." Genetics 178 (2008): 2253-2264.
It is publicly available at http://publiclines.versailles.inra.fr/page/8
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}


\examples{
data(CviCol)
dim(CviCol)
head(CviCol, n=3)
}

\keyword{datasets}
