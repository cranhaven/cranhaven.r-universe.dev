\name{GRTo-package}
\alias{GRTo-package}
\alias{GRTo}
\docType{package}
\title{
Gutenberg-Richter Tools
}
\description{
Gutenberg-Richter Tools for the analyzis of the properties of distributions of earthquakes 
in magnitude. Some functions in this package are helpful for the comparison of Gutenberg-Richter 
b-values. The Schuster's function can be used to highlight blast contamination in earthquake
catalogs.
}
\details{
\tabular{ll}{
Package: \tab GRTo\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2015-09-16\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
}
\author{ Daniel Amorese <amorese@ipgp.fr>, Paul A. Rydelek <prydelek@memphis.edu> and Jean-Robert Grasso <grasso@obs.ujf-grenoble.fr>

Maintainer: Daniel Amorese <amorese@ipgp.fr>
 }
\references{
D. Amorese, "Applying a change-point detection method on frequency-magnitude distributions", \emph{Bull. seism. Soc. Am.} (2007) 97, doi:10.1785\/0120060181

D. Amorese, J.-R. Grasso and P. A. Rydelek, "On varying \emph{b}-values with depth: results from computer-intensive tests for Southern California", \emph{Geophys. J. Int.} (2010) 180, 347-360 

Rydelek, P. A. and Hass, L. (1994) On Estimating the Amount of Blasts in Seismic Catalogs with Schuster's Method \emph{Bulletin of the Seismological Society of America}, Vol. 84, No. 4, pp. 1256-1259.

Siegel, A.F., "Robust regression using repeated medians", \emph{Biometrika} (1982) 69, 242-244.

Zurn, W. and Rydelek, P. A. (1996) Revisiting the phasor-walkout method for detailed investigation of harmonic signals in time series \emph{Surveys in Geophysics}, Vol. 15, No. 4, pp. 409-431.
}
\keyword{ package }
%\seealso{}
\note{
Thanks to Paul Friberg (ISTI) for telling us about the bug in the graphical part of bvmed.R. Thanks to Scott Kostyshak (Princeton University) for telling us about the extra bootstrap package dependency. GRTo version 1.2 fixes the issues reported by Paul and Scott. Thanks to Andrew Barbour (USGS) for detecting and fixing minor problems of bvmed.R of version 1.2. In version 1.3, the b-value and an optional staircase line FMD are displayed by BBootComp when a single magnitude file is processed. Version 1.3 also fixes a bug with negative magnitude values. In version 1.3, an option coded by Andrew Barbour allows the processing of data from a list. Willy Aspinall (University of Bristol) highlights bugs in the graphical parts of bvmed and BBootComp: An inappropriate magnitude shift in FMD plots is definitely fixed in version 1.3. Many thanks to Willy! 
}
\examples{
# Comparison of the b-value for the IDYLLdeep data set.
    BBootComp(finame1=system.file("extdata","IDYLLdeep.data.txt",package="GRTo"),
    finame2=NULL, colid1=15, colid2=NULL,nrep=200,alter=NULL,findtm1=TRUE,
    findtm2=NULL,plot=TRUE, title="IDYLLWILD", tm1=NULL,tm2=NULL)
}
