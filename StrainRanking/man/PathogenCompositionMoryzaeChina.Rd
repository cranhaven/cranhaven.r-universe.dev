\docType{data}
\name{PathogenCompositionMoryzaeChina}
\alias{China}
\alias{MoryzaeChina}
\alias{PathogenCompositionMoryzaeChina}
\title{Compositions of Magnaporthe oryzae collected in China}
\format{A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in August 2008; 2nd row: PC collected in September 2008).}
\usage{
data(PathogenCompositionMoryzaeChina)
}
\description{
Compositions of Magnaporthe oryzae formed from samples
collected in Youle, Yunnan Province, China, in August 2008
and September 2009 (Saleh et al., 2014).
}
\examples{
## Load Pathogen Compositions of M. oryzae collected in China
data(PathogenCompositionMoryzaeChina)

## Size of the first sample
sum(PathogenCompositionMoryzaeChina[1,])

## Size of the second sample
sum(PathogenCompositionMoryzaeChina[2,])

## Total number of different variants
ncol(PathogenCompositionMoryzaeChina)

## Display pathogen compositions
x=PathogenCompositionMoryzaeChina
barplot(t(x), col=rainbow(ncol(x)), main="M. oryzae - China")
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
\code{\link{PathogenCompositionMoryzaeMadagascar}}
}
\keyword{datasets}

