\name{renin}
\alias{renin}
\alias{age}
\alias{case}
\alias{gender}
\alias{geno}
\alias{race}
\docType{data}
\title{Example Dataset for 'haplo.ccs'}
\description{
  
  This dataset serves as the example dataset for 'haplo.ccs'. The
  genotypes in this dataset were generated from haplotype frequency data
  for renin, one of the genes in the renin-angiotensin system.
  
}
\usage{

data(renin)

}
\format{

  All variables are in numeric format.
  
}
\details{

  'case': case-control status (1=Case, 0=Control)

  'geno': a matrix of alleles indicating genotype where each locus
    has a pair of adjacent columns of alleles, and the order of columns
    corresponds to the order of the loci on the chromosome (1=A, 2=C,
    3=T, 4=G)

  'gender': gender (1=Male, 2=Female)

  'age': age in years

  'race': race (1=White, 2=Black, 3=Asian, 4=Other)
  
}
\note{

  Other covariates, such as gender, age, and race were randomly
  generated. Therefore no scientific inference should be made from these
  data.
  
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

}
\keyword{datasets}
