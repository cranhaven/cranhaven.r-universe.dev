\name{DME}
\alias{DME}
\title{
Developmental Mass Extrapolation
}

\description{
This function estimates the body mass of an immature specimen based on the mass of a presumed adult and a standard measurement (e.g., femur length or circumference), as described by Erickson and Tumanova (2000).
}

\usage{
DME(juv_proxy, adu_proxy, adu_mass, scale_fac = 3)
}

\arguments{
  \item{juv_proxy}{numeric value or vector corresponding to the measurement taken on the immature specimen(s) of interest}
  \item{adu_proxy}{numeric value corresponding to the measurement taken on the presumed adult representative}
  \item{adu_mass}{numeric value corresponding to the body mass of the adult representative}
  \item{scale_fac}{numeric value corresponding to the growth allometric scaling factor applied to the immature and adult specimens, defaults to 3 (i.e., isometry)}
}

\details{
It cannot be assumed that growth-related allometric scaling coefficients will be consistent with those derived from interspecific relationships of adults (e.g.,\link{QE}). \code{DME} was developed by Erickson and Tumanova (2000) in order to extrapolate the body mass of an adult to that of an immature specimen through the use of a growth-related scaling factor. Although the assumed scaling factor is that of isometry between a linear and volumetric measurement (\code{scale_fac = 3}), if an alternate scaling factor is known, then it can be incorporated by adjusting the \code{scale_fac} value. 
}

\value{
A numeric value or vector representing the mass estimate(s) in grams of the immature specimen(s).
}

\references{
  Brassey C. A., Maidment, S. C. R. & Barrett, P. M. (2015). Body mass estimates of an exceptionally complete Stegosaurus (Ornithischia: Thyreophora): comparing volumetric and linear bivariate mass estimation methods. Biology Letters, 11, 20140984.

  Erickson G. M. and Tumanova, T. A. (2000). Growth curve of Psittacosaurus mongoliensis Orborn (Ceratopsia; Psittacosauridae) inferred from long bone histology. Zoological Journal of the Linnean Society, 130, 551-566.
}

\author{
Nicolas E. Campione
}

\seealso{
  \code{\link{QE}}
  \code{\link{cQE}}
}
\examples{
##Based on the immature Stegosaurus from Brassey et al. (2015)

## Immature (NHMUK R36730: HC = 282, FC = 339, FL = 863
## Adult (YPM1853): HC = 352, FC = 425, FL = 1348

##DME estimate, adult extracted from dinos dataset
DME(juv_proxy = 863, adu_proxy = 1348, adu_mass = QE(352+425)[2])
}

