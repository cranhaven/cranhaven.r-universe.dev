\docType{data}
\name{PathogenCompositionPsyringaePhylogroups}
\alias{PathogenCompositionPsyringaePhylogroups}
\alias{Phylogroups}
\alias{PsyringaePhylogroups}
\title{Compositions of Pseudomonas syringae at the phylogroup resolution}
\format{A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in Lower Durance River valley; 2nd row: PC collected in Upper Durance River valley).}
\usage{
data(PathogenCompositionPsyringaePhylogroups)
}
\description{
Compositions of Pseudomonas syringae formed from samples
collected in South-East France, in Lower Durance River
valley and in Upper Durance River valley (Monteil et al.,
2014).
}
\examples{
## Load Pathogen Compositions of P. syringae at the phylogroup resolution
data(PathogenCompositionPsyringaePhylogroups)

## Size of the first sample
sum(PathogenCompositionPsyringaePhylogroups[1,])

## Size of the second sample
sum(PathogenCompositionPsyringaePhylogroups[2,])

## Total number of different variants
ncol(PathogenCompositionPsyringaePhylogroups)

## Display pathogen compositions
x=PathogenCompositionPsyringaePhylogroups
barplot(t(x), col=rainbow(ncol(x)), main="P. syringae - Phylogroups")
}
\references{
Monteil C L, Lafolie F, Laurent J, Clement J C, Simler R, Travi Y,
Morris C E (2014). Soil water flow is a source of the plant pathogen
Pseudomonas syringae in subalpine headwaters. Environ. Microbiol. 16: 203862052.

Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H,
Berder J, Moinard J, Fournier E, Tharreau D,
Morris C, Sache I (2017). Testing differences between
pathogen compositions with small samples and sparse data.
Phytopathology 107: 1199-1208. http://doi.org/10.1094/PHYTO-02-17-0070-FI
}
\seealso{
\code{\link{PathogenCompositionPsyringaeClades}},
\code{\link{PathogenCompositionPsyringaeHaplotypes}}
}
\keyword{datasets}

