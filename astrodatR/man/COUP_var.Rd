\name{COUP_var}
\docType{data}
\alias{COUP_var}

\title{COUP: X-ray source variability}
\description{
This dataset consists of three time series representing (in)homogeneous Poisson processes. They are tables of arrival times of individual X-ray photons from magnetically flaring young stars in the Orion Nebula Cluster, obtained from the Chandra Orion Ultradeep Project (COUP; Getman et al. 2005). NASA's Chandra X-ray Observatory is described at \url{http://en.wikipedia.org/wiki/Chandra_X-ray_Observatory}.  COUP detected 1616 X-ray sources, mostly flaring young stars.  The first source, COUP 263, has 209 photon arrival times that appear to represent a constant X-ray intensity.  The second source, COUP 551, has 678 photons and exhibits at least two flares.  The third source, COUP 554, is much stronger with 14,258 photons; it exhibits very high-amplitude, nearly continuous flaring.  The dataset has three columns: the source identifier, the photon arrival time, and the photon X-ray energy. Gaps in the data streams due to the satellite observatory orbit have been removed. Photon energy may also represent a time dependent process as flares typically have higher average energy X-rays than quiescent periods. The dataset is discussed at \url{http://astrostatistics.psu.edu/datasets/Chandra_flares.html}.  This dataset is useful for statistical methods treating event data and inhomogeneous Poisson processes. 
}
\usage{COUP_var}
\format{A table containing 15,145 rows and 3 columns}
\source{Getman et al. (2005)}
\references{
Getman,K. V., Flaccomio, E., Broos, P. S., Grosso, N., Tsujimoto, M., Townsley, L., Garmire, G. P., Kastner, J., Li, J., Harnden, F. R., Jr., Wolk, S., Murray, S. S., Lada, C. J., Muench, A. A., McCaughrean, M. J., Meeus, G., Damiani, F., Micela, G., Sciortino, S., Bally, J., Hillenbrand, L. A., Herbst, W., Preibisch, T., Feigelson, E. D. (2005), Chandra Orion Ultradeep Project: Observations and source lists, \emph{Astrophysical Journal Supplements}, 160, 319-352 \url{http://adsabs.harvard.edu/abs/2005ApJS..160..319G}
}
\keyword{datasets}
