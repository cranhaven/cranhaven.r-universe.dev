\docType{data}
\name{PathogenCompositionPtriticinaGalibier}
\alias{Galibier}
\alias{PathogenCompositionPtriticinaGalibier}
\alias{PtriticinaGalibier}
\title{Compositions of Puccinia triticina in Galibier crops}
\format{A data frame with 28 rows, each row providing the pathogen composition (PC) at a given date in years 2007-2013. The dates are provided in Soubeyrand et al. (2017).}
\usage{
data(PathogenCompositionPtriticinaGalibier)
}
\description{
Compositions of Puccinia triticina formed from samples
collected in Lomagne, South-West France, from 2007 to 2013
(Soubeyrand et al., 2017).
}
\examples{
## Load Pathogen Compositions of P. triticina in Galibier crops
data(PathogenCompositionPtriticinaGalibier)

## Size of the first sample
sum(PathogenCompositionPtriticinaGalibier[1,])

## Total number of different variants
ncol(PathogenCompositionPtriticinaGalibier)

## Display pathogen compositions
x=PathogenCompositionPtriticinaGalibier
barplot(t(x), col=rainbow(ncol(x)), las=2, main="P. triticina - Galibier")
}
\references{
Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H,
Berder J, Moinard J, Fournier E, Tharreau D,
Morris C, Sache I (2017). Testing differences between
pathogen compositions with small samples and sparse data.
Phytopathology 107: 1199-1208. http://doi.org/10.1094/PHYTO-02-17-0070-FI
}
\seealso{
\code{\link{PathogenCompositionPtriticinaKalango}}
}
\keyword{datasets}

