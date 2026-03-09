\name{featureRep}
\alias{featureRep}
\title{
A function to assess how well landscape features are represented in descrete samples
}
\description{
This function establishes graphical representation of the landscape feature in the sample points. An approximation of Kolmogorov-Smirnov similarity test (D-statistic) between the sampled feature distribution and the population feature distribution is also given.
}
\usage{
featureRep(fgrid,df )
}
\arguments{
  \item{fgrid}{
raster grid of the landscape feature
}
  \item{df}{
dataframe of sampled locations with similar coordinate reference system (CRS) as the input raster map
}
}
\details{
The sampled points should have the same coordinate system as the landscape feature (raster map). The function extracts the raster map values, attaches them to the sample points, and creates histogram distributions: one for the feature map as contained in the sample points and another as contained in the raster map.
}
\value{
Histograms on back-to-back showing distribution of the landscape feature in the sampled points and on the map for similarity comparison
}
\references{
Kolmogorov, A. N. 1933. Sulla determinazione empirica di una legge di distribuzione. Giornale dell’ Istituto Italiano degli Attuari 4: 83–91

Simard R, L'Ecuyer P. 2011. Computing the Two-Sided Kolmogorov–Smirnov Distribution. Journal of Statistical Software. 39 (11): 1–18. doi:10.18637/jss.v039.i11
}
\author{
Christian Thine Omuto
}
\note{
The input points dataframe and raster map must have similar coordinate reference system.
}

\seealso{
\code{\link{imageIndices}}
}
\examples{
library(Hmisc)
data(soil)
dem=suitabinput["dem"]
featureRep(dem,soil)
}
