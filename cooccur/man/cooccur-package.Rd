\name{cooccur-package}
\alias{cooccur-package}
%%\alias{cooccur}
\docType{package}
\title{
cooccur: Probabilistic Species Co-occurrence Analysis in R
}
\description{
This R package applies the probabilistic model of species co-occurrence (Veech 2013) to a set of species distributed among a set of survey or sampling sites. The algorithm calculates the observed and expected frequencies of co-occurrence between each pair of species. The expected frequency is based on the distribution of each species being random and independent of the other species. The analysis returns the probabilities that a more extreme (either low or high) value of co-occurrence could have been obtained by chance. The package also includes functions for visualizing species co-occurrence results and preparing data for downstream analyses.
}

\details{
\tabular{ll}{
Package: \tab cooccur\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-06\cr
License: \tab GPL-2\cr
}
}

\author{
Maintainer: Daniel M. Griffith <griffith.dan@gmail.com>
}
\references{
Veech (2013). A probabilistic model for analysing species co-occurrence. Global Ecology and Biogeography, DOI: 10.1111/j.1466-8238.2012.00789.x

Griffith, Veech, and Marsh (2016). cooccur: Probabilistic Species Co-Occurrence Analysis in R. Journal of Statistical Software, 69(2), 1-17. doi:10.18637/jss.v069.c02
}
\keyword{ co-occurrence }

\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#summary(cooccur.finches)
}
