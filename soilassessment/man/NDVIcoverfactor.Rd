\name{NDVIcoverfactor}
\alias{NDVIcoverfactor}
\title{
Estimating Cover Factor for Erosion Models Using NDVI Remote Sensing Index for Vegetation
}
\description{
This function estimates the C factor of erosion models using NDVI derived from remote sensing images
}
\usage{
NDVIcoverfactor(ndvi, model="kniff")
}

\arguments{
  \item{ndvi}{
remote sensing index for vegetation
}
 \item{model}{
model for relating ndvi with C factor
}
}
\details{
The function uses empirical models from the literature. It currently has 15 models: kniff, patil, almagro, jamshidi, dejong, toumi, gitas, joshi, durigon, wickama, suriyaprasit, lin, bahrawi, kulikov, power, modis.
}
\value{
a numeric value between 0 and 1 .
}
\references{
Ayalew, D.A., Deumlich, D., Šarapatka, B., Doktor, D., 2020. Quantifying the Sensitivity of NDVI-Based C Factor Estimation and Potential Soil Erosion Prediction using Spaceborne Earth Observation Data. Remote Sensing 12, 1136. https://doi.org/10.3390/rs12071136


Mahgoub, M., Elalfy, E., Soussa, H., Abdelmonem, Y., 2024. Relation between the soil erosion cover management factor and vegetation index in semi-arid basins. Environ Earth Sci 83, 337. https://doi.org/10.1007/s12665-024-11593-3

}
\author{
Christian Thine Omuto
}
\note{
Some models may return negative values, which are masked out in the function or may return warning
}


\seealso{
\code{\link{VegCOV}}
}
\examples{
NDVIcoverfactor(0.27,"modis")

library(raster)
library(sp)
r <- raster(xmn= 35.5, ymn= -1.5, xmx = 37.5,ymx = 1.5, res = c(0.01,0.01),
            crs = '+proj=latlon +datum=WGS84 +no_defs')
r <- setValues(r, sample(x=0:1, size=ncell(r), replace=TRUE))
r$s2=NDVIcoverfactor(values(r),"modis")
plot(r$s2)
}
