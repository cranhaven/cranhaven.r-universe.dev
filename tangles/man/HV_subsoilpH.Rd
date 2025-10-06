\name{Hunter Valley subsoil pH points}
\docType{data}
\alias{HV_subsoilpH}
\title{Hunter Valley subsoil pH data with environmental covariates}
\description{A \code{dataframe} carrying point coordinates for 506 observations of soil pH from the Lower Hunter Valley, NSW, Australia. The depth interval of the observation is 60-100cm. Together with the soil pH information is environmental covariate data that have been intersected with the point data. The environmental covariates have been sourced from a digital elevation model and Landsat 7 spectral band reflectance.}
\usage{data(HV_subsoilpH)}

\format{
\code{HV_subsoilpH} is 506 row \code{dataframe} with the first 2 columns indicating the projected coordinate locations. The coordinate reference system is WGS 84 Zone 56. Soil pH for the 60-100cm depth interval is recorded in the following column. A suite of intersected environmental covariate data fills the remaining columns. This environmental covariate information refers to: 
\itemize{
\item \code{Terrain_Ruggedness_Index}: A quantitative measure of topographic heterogeneity. High values indicate terrain is rugged or heterogeneous.  
\item \code{AACN}: Difference between elevation and an interpolation of a channel network base level elevation. Knowledge of the spatial distribution of channel networks (lines) is therefore necessary for this parameter.
\item \code{Landsat_Band1}: Earth surface reflectance information derived from Landsat 7 ETM+ satellite. Band 1 spectral range is .45 to .52 microns (visible blue).
\item \code{Elevation}: Meters above sea level; derived from a digital elevation model.
\item \code{Hillshading}: Analytic hill shading derived from digital elevation model and a fixed sun degree angle.
\item \code{Light_insolation}: Measure of potential incoming solar radiation, and used as a parameter for evaluating the positional aspect effect. Derived from digital elevation model, this parameter was evaluated over the duration of a single calendar year with a 5 day time step.
\item \code{Mid_Slope_Positon}: A relative slope position parameter which gives a classification of the slope position in both valley and crest positions.
\item \code{MRVBF}: Multi-resolution valley bottom flatness is derived using slope and elevation to classify valley bottoms as flat, low areas (Gallant and Dowling 2003). This is accomplished through a series of neighborhood operations at progressively coarser resolutions with the goal of identifying both small and large valleys. MRVBF has been used extensively for the delineation and grading of valley floor units corresponding to areas of alluvial and colluvial deposits. High values of MRVBF indicate relatively low, flat areas of the landscape.
\item \code{NDVI}: Normalized difference vegetation index. Derived from Landsat 7 data (B4-B3)/(B4+B3). High values indicate actively growing vegetation.
\item \code{TWI}: A secondary land form parameter which estimates for each pixel, its tendency to accumulate water.
\item \code{Slope}: Measured in degrees, is the first derivative of elevation in the direction of greatest slope.}}

\details{The area in question is the Hunter Wine Country Private Irrigation District (HWCPID), situated in the Lower Hunter Valley, NSW (32.83S 151.35E), and covers an area of approximately 220 km2. The HWCPID is approximately 140 km north of Sydney, NSW, Australia. Climatically, the HWCPID is situated in a temperate climatic zone, and experiences warm humid summers, and relatively cooler yet also humid winters. Rainfall is mostly uniformly distributed throughout the year. On average the HWCPID receives just over 750 mm of rainfall annually. In terms of land use, an expansive viticultural industry is situated in the area and is possibly most widespread
of rural industries, followed by dry land agricultural grazing systems.}

\note{This data set is used in the Use R for Digital Soil Mapping manual in the section about quantification of uncertainties.}

\references{
  \itemize{
    \item Gallant, J.C., Dowling, T.I. (2003). A multiresolution index of valley bottom flatness for mapping depositional areas. \emph{Water Resources Research}, 39(12), 1347. \doi{10.1029/2002WR001426}
    \item Malone, B.P., Hughes, P., McBratney, A.B., Minasny, B. (2014). A model for the identification of terrons in the Lower Hunter Valley, Australia. \emph{Geoderma Regional}, 1, 31–47. \doi{10.1016/j.geodrs.2014.08.001}
  }
}

\examples{
data(HV_subsoilpH)
summary(HV_subsoilpH)
}
\keyword{datasets}
