\name{GX}
\docType{data}
\alias{GX}

\title{GX 5-1: X-ray source variability}
\description{
This dataset is a time series of the rapid changes in brightness seen in the Galactic X-ray binary star GX 5-1 lying near the Galactic Center.  X-ray binaries produce prodigious high energy radiation as gas from a companion star is accreted onto a compact companion (white dwarf, neutron star or black hold); see \url{http://en.wikipedia.org/wiki/X-ray_binary}. While some systems show periodic variations from stellar rotation and orbits, others show stochastic quasi-periodic variations (QPOs) and red noise (1/f-type noise) from the accretion disk around the compact star; see \url{http://en.wikipedia.org/wiki/Quasi-periodic_oscillations}.  Over a thousand studies of the QPO phenomenon have been published.   

This dataset consists of 65,536 evenly spaced measurements of X-ray counts detected in 1/128 second intervals by the Japanese Ginga satellite (Norris et al. 1990). While the photon counts constitute a Poisson process, the values are sufficiently high (~70 counts/interval) that a Gaussian process assumption is reasonable.  The dataset is described in Appendix C.12 of Feigelson & Babu (2012) and Hertz and Feigelson (1995).  It can be used to study stochastic temporal behaviors in evenly-spaced time series.  
}
\usage{GX}
\format{A stream of 65,536 2-digit integers that can be read into R using `scan(GX)'}
\source{Feigelson and Babu (2012), Hertz & Feigelson (1995), Norris et al. (1990)}
\references{
Feigelson, E. D. and Babu, G. J. (2012) \emph{Modern Statistical Methods for Astronomy with R Applications}, Cambridge UK:Cambridge University Press

Hertz, P. and Feigelson, E. D. (1995) A sample of astronomical time series, in \emph{applications of Time Series Analysis in Astronomy and Meteorology}. T. SubbaRao (ed.), Chapman & Hall, 340-356

Norris, J. P., Hertz, P., Wood, K. S., Vaughan, B.. A., Michelson, P. F., Mitsuda, K. & Dotani, T. (1990) Independence of short time scale fluctuations of quasi-periodic oscillations and low frequency nopise in GX5-1, \emph{Astrophysical Journal}, 361, 514-526. \url{http://adsabs.harvard.edu/abs/1990ApJ...361..514N}
}
\keyword{datasets}
