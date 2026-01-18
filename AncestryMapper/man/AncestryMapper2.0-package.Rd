\name{AncestryMapper}
\alias{AncestryMapper}
\docType{package}
\title{Ancestry Mapper 2.0}
\description{
Package computes the genetic distance, defined as the euclidean distance
amongst a sample set of individuals and any number of human population
references. The package allows for the visualisation of the relationship
of sample individuals to the reference populations, thus permitting the
user to assess the relationship of individuals to different geographic
groupings.

Package comes with pre-loaded with toy data and toy references from
various sources comprising 158 global populations.

Additional and full  Population References can be downloaded from:

http://bit.ly/1OUstDP

}
\details{
\tabular{ll}{
Package: \tab AncestryMapper\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2016-08-??\cr
License: \tab None\cr
LazyLoad: \tab yes\cr
}

}
\author{
Eoghan T. O'Halloran ,Tiago R. Magalh\~aes, Darren J. Fitzpatrick

Maintainer: Eoghan T. O'Halloran  <eoghan.ohalloran2@mail.dcu.ie>
}
\references{
Magalh\~aes et al, 2012 PLOS One accepted.
}
\keyword{ package }
\keyword{ genetic distance }
\keyword{ ancestry mapping }
\keyword{ population genetics }
\keyword{ human ancestry }
\keyword{ admixture }
\keyword{ PCA }
\seealso{
image dist
}
\examples{
\dontrun{
library(AncestryMapper)
Refs <- system.file ("data", package = "AncestryMapper")

tpeds <- system.file ("extdata", package = "AncestryMapper")

Corpheno <- system.file ("extdata", "CorPheno", package =
"AncestryMapper")

All00Frq <- system.file ("data", "MinMaxFreq.rda", package = "AncestryMapper")

genetic.distance <- calculateAMidsArith(pathTotpeds = tpeds,
                                         NameOut = "Example",
                                         pathToAriMedoids = Refs,
                                         pathAll00 = All00Frq)

plotAMids(AMids = genetic.distance, phenoFile = Corpheno, columnPlot = "I")}
}
