\name{quefts_crop}

\alias{quefts_crop}

\title{
Crop parameters
}

\description{
A number of default crop parameter sets are provided
}

\usage{
quefts_crop(name="")
}

\arguments{
 \item{name}{character. crop name} 
}


\details{
\tabular{lll}{
	\tab \bold{Input Parameters} \tab \bold{Explanation} \cr
	\tab \code{_minVeg, _maxVeg, _minStore, _maxStore} \tab minimum and maximum concentration of "\code{_}" (N, P, or K) in vegetative organs and in storage organs (kg/kg) \cr
	\tab \code{Yzero} \tab the maximum biomass of vegetative organs at zero yield of storage organs (kg/ha) \cr
	\tab \code{Nfix} \tab the fraction of a crop's nitrogen uptake supplied by biological fixation \cr
}
}


\value{
list with crop parameters. See Details
}


\examples{
barley <- quefts_crop("Barley")
str(barley)
}
