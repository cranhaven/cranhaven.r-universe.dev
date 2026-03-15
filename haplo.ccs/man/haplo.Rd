\name{haplo}
\alias{haplo}
\title{Specify an Allele Matrix, Inheritance Mode, and Grouping for Rare Haplotypes}
\description{
  
  'haplo' specifies an allele matrix and an inheritance mode for the
  'haplo.ccs' model formula. 'haplo' also specifies preferences for
  grouping rare haplotypes.

}
\usage{

haplo(..., mode, group.rare=TRUE, rare.freq=0.02)

}
\arguments{

  \item{\dots}{a matrix of alleles or list of columns of an allele matrix.
    Each locus on the chromosome has a pair of adjacent columns in the
    allele matrix, so that the number of columns of the allele matrix is
    twice the number of loci on the chromosome. The order of the columns
    corresponds to the order of the loci on the chromosome, and each row
    corresponds to the alleles for each subject.  The alleles should be
    numerically coded, i.e., 1, 2, 3, or 4 for 'A', 'C', 'G', or 'T'.}

  \item{mode}{the inheritance mode, either 'additive', 'dominant', or
    'recessive'. Note that the default inheritance mode is 'additive'.}

  \item{group.rare}{a logical value indicating whether rare haplotypes
    should be grouped in the 'haplo.ccs' model. Note that the default is
    to group rare haplotypes.}

  \item{rare.freq}{the population haplotype frequency to define rare
    haplotypes. If 'group.rare=TRUE', then haplotypes with an estimated
    population frequency less than or equal to 'rare.freq' are grouped
    in the 'haplo.ccs' model. Note that the default haplotype frequency
    is 0.02. 'rare.freq' is automatically set to 0 if
    'group.rare=FALSE'.}
  
}
\value{
  
  A matrix of alleles with mode, group.rare, and rare.freq assigned as
  attributes.

}
\author{

  Benjamin French and Thomas Lumley, University of Washington

}
\references{

  French B, Lumley T, Monks SA, Rice KM, Hindorff LA, Reiner AP, Psaty
  BM. Simple estimates of haplotype relative risks in case-control
  data. Genetic Epidemiology 2006; 30(6):485-494.

}
\seealso{

  \code{\link{haplo.ccs}}

}
\examples{

data(renin)

## Specify an allele matrix in a model fit by 'haplo.ccs'.

haplo.ccs(case ~ haplo(geno))

## Specify dominant inheritance and define rare haplotypes.

haplo.ccs(case ~ haplo(geno, mode="dominant", rare.freq=0.01))

## Specify the allele matrix without grouping rare haplotypes.

haplo.ccs(case ~ haplo(geno, group.rare=FALSE))

}
\keyword{models}
\keyword{regression}
