\name{DecodeMap}
\alias{DecodeMap}
\docType{data}
\title{
Decode recombination map
}
\description{
A recombination map of the human genome, adapted from the dataset published in (Kong et al., 2010).
}
\usage{DecodeMap}
\format{
List of 23 components (one for each chromosome pair). Each is a list of 2 matrices, containing the male and female recombination maps of the given chromosome respectively. The 23rd component has NULL as its first (male) element, as we assume no recombination between sex chromosomes in males.
The recombination map matrices have 2 columns, named "Mb" and "cM". The "Mb" column has the base positions of the markers used by (Kong et. al, 2010), and "cM" the genetic distance from the left telomere.
}
\source{
Kong, A. et al. (October 2010) Fine scale recombination rate differences between sexes, populations and individuals. \emph{Nature}, 467, 1099--1103. doi:10.1038/nature09525.
}
\examples{
#the first entries of the male map of chromosome 1:
head(DecodeMap[[1]]$male)
}
\keyword{datasets}
