\name{dat}
\alias{dat}
\docType{data}
\title{
Simulated allele counts
}
\description{
\code{dat} is a simulated data set meant to mimic GBS data. It is a list with three components. The first two components are N (number of individuals) by P (number of SNPs) matrixes with allele counts for the first and second allele at each locus, respectively. The third component is a numeric vector that gives the true ploidy for each individual (2 = diploid, 4 = tetraploid).
}
\usage{data("dat")}
\format{
  The format is:
List of 3
 $ : int [1:200, 1:10000] NA NA 7 4 NA NA NA NA NA NA ...
 $ : int [1:200, 1:10000] NA NA 2 5 NA NA NA NA NA NA ...
 $ : num [1:200] 4 2 4 2 2 2 2 2 2 2 ...
}
\examples{
data(dat)
str(dat)
}
\keyword{datasets}
