\name{VirtualSpecies}
\docType{data}
\alias{VirtualSpecies}
\encoding{latin1}
\title{PROBABILITY OF A VIRTUAL SPECIES}
\usage{data(VirtualSpecies)}
\description{Probability of a virtual species in France.} 
\format{The 3 bioclimatic variables of WorldClim at a resolution of five arc-minutes (Hijmans et al., 2005) BIO8 (Mean Temperature of Wettest Quarter), BIO12 (Annual Precipitation) and BIO15 (Precipitation Seasonality, Coefficient of Variation) were used to generate the distribution of the virtual species by using the virtualspecies R package (Leroy et al. 2016; 2018). The limits were from -5º to 6º in latitude, and from 44º to 49º. Subsequently, 50\% of the data were randomly selected, in order to simulate a random sampling of the species. The final continuous suitability maps obtained in this way were scaled between 0 and 1.
}
\references{
Hijmans, R. J., Cameron, S. E., Parra, J. L., Jones, P. G. & Jarvis, A. (2005) Very high resolution interpolated climate surfaces for global land areas. \emph{International Journal of Climatology}, 25: 1965-1978.

Leroy, B., Meynard, C. N., Bellard, C., Courchamp, F. (2016) Virtualspecies, an R package to generate virtual species distributions. \emph{Ecography}, 39: 599-607.

Leroy, B., Meynard, C.N., Bellard, C., Courchamp, F., Delsol, R. & Gaul, W. (2018) Generation of Virtual Species Distributions. R package version 1.4-4. Available at: \url{https://CRAN.R-project.org/package=virtualspecies}.
}
\keyword{VirtualSpecies}
