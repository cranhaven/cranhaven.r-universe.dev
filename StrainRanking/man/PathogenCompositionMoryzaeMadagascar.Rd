\docType{data}
\name{PathogenCompositionMoryzaeMadagascar}
\alias{Madagascar}
\alias{MoryzaeMadagascar}
\alias{PathogenCompositionMoryzaeMadagascar}
\title{Compositions of Magnaporthe oryzae collected in Madagascar}
\format{A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in February 2005; 2nd row: PC collected in April 2005).}
\usage{
data(PathogenCompositionMoryzaeMadagascar)
}
\description{
Compositions of Magnaporthe oryzae formed from samples
collected in Andranomanelatra, Madagascar, in February and
April 2005 (Saleh et al., 2014).
}
\examples{
## Load Pathogen Compositions of M. oryzae collected in Madagascar
data(PathogenCompositionMoryzaeMadagascar)

## Size of the first sample
sum(PathogenCompositionMoryzaeMadagascar[1,])

## Size of the second sample
sum(PathogenCompositionMoryzaeMadagascar[2,])

## Total number of different variants
ncol(PathogenCompositionMoryzaeMadagascar)

## Display pathogen compositions
x=PathogenCompositionMoryzaeMadagascar
barplot(t(x), col=rainbow(ncol(x)), main="M. oryzae - Madagascar")
}
\references{
Saleh D, Milazzo J, Adreit H, Fournier E, Tharreau D
(2014). South-East Asia is the center of origin, diversity
and dispersion of the rice blast fungus, Magnaporthe
oryzae. New Phytologist 201: 1440-1456.

Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H,
Berder J, Moinard J, Fournier E, Tharreau D,
Morris C, Sache I (2017). Testing differences between
pathogen compositions with small samples and sparse data.
Phytopathology 107: 1199-1208. http://doi.org/10.1094/PHYTO-02-17-0070-FI
}
\seealso{
\code{\link{PathogenCompositionMoryzaeChina}}
}
\keyword{datasets}

