\name{StrainRanking-package}
\alias{StrainRanking-package}
\alias{StrainRanking}
\docType{package}
\title{
Ranking of Pathogen Strains
}
\description{
Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics, using demographic and genetic data sampled in the curse of the epidemics. This package also includes the GMCPIC test.
}
\details{
\tabular{ll}{
Package: \tab StrainRanking\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2017-11-25\cr
License: \tab GPL (>=2.0)\cr
Depends: \tab methods\cr
}
To rank pathogen strains using the method of Soubeyrand et al. (2014), create a \link{DG object} (Demographic and Genetic data set) with one of the three construction functions (\link{DGobj.rawdata}, \link{DGobj.simul.regression} and \link{DGobj.simul.mechanistic}) and apply the \link{ranking.strains} function. Other construction functions returning a DG object might be written to extend the approach proposed by Soubeyrand et al. (2014). 
}
\author{
Soubeyrand, S., Tollenaere, C., Haon-Lasportes, E. and Laine, A.-L. 

Maintainer: \email{samuel.soubeyrand@inra.fr}
}
\references{
Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.

Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2017). Testing differences between pathogen compositions with small samples and sparse data. Phytopathology 107: 1199-1208. http://doi.org/10.1094/PHYTO-02-17-0070-FI
}

\keyword{ package }
