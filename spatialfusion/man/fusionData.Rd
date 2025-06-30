\name{fusionData}
\alias{fusionData}
\alias{print.dstan}
\alias{print.dinla}
\title{Prepare data structure for spatial fusion modelling}
\usage{
fusionData(geo.data, geo.formula,
           lattice.data, lattice.formula,
           pp.data, distributions, domain = NULL,
           method = c("Stan", "INLA"),
           proj4string = CRS(as.character(NA)),
           stan.control = NULL)
}
\arguments{
\item{geo.data}{an object of class \code{\link[base]{data.frame}} or \code{sf}. If \code{data.frame}, it must have column names "x" and "y" as coordinates of observations.}

\item{geo.formula}{an object of class \code{\link[stats]{formula}}. A symbolic description of the model to be fitted for geostatistical data. For multivariate geostatistical data, use syntax \code{cbind(y1, y2)} followed by \code{~}.}

\item{lattice.data}{an object of class \code{sf}. Contains lattice data.}

\item{lattice.formula}{an object of class \code{\link[stats]{formula}}. A symbolic description of the model to be fitted for lattice data. For multivariate lattice data, use syntax \code{cbind(y1, y2)} followed by \code{~}.}

\item{pp.data}{an object of class \code{\link[base]{data.frame}} or \code{sf}, or a list of them. If \code{data.frame}, it must have column names "x" and "y" as coordinates.}

\item{distributions}{a vector of strings. Specifying the distributions of each geostatistical and lattice response variable, currently \dQuote{Gaussian} or \dQuote{normal}, \dQuote{Poisson} (count) and \dQuote{Bernoulli} (binary) are supported. Note: no distribution is required to be specified for point pattern data.}

\item{domain}{an object of class \code{sf}. The spatial domain considered for computing gridded point pattern data. If \code{NULL}, a bounding box that contains all spatial units is used.}

\item{method}{character. Either 'Stan' or 'INLA', the method to be used for fitting the spatial fusion model later.}

\item{proj4string}{projection string of class \code{\link[sp]{CRS-class}}.}

\item{stan.control}{a named list of parameters to control the Stan implementation of spatial fusion models. Default to NULL such that all the default values are used.
      \itemize{
      \item \code{n.neighbor} (positive integer) Number of nearest neighbors to consider. Default to 5.

\item \code{n.sampling} (positive integer) Number of sampling points for each area. Default to 5.

\item \code{n.grid} (positive integer) Number of grid used to divide the spatial domain in each of x- and y-direction to count the number of cases/events in each grid. Default to 10.}
}}
\value{
The returned value is an object of either class \code{dstan} or \code{dinla}, depending on the chosen \code{method}. They are both lists that contain:

\item{distributions}{distribution specified each response variable.}

\item{n_point}{sample size for geostatistical data.}

\item{n_area}{sample size for lattice data.}

\item{n_grid}{Set to 1 for INLA, set to the number of grids for Stan.}

\item{p_point}{number of coefficients for geostatistical model component (only if there is geostatistical data).}

\item{n_point_var, n_area_var, n_pp_var}{number of response variables for each data type.}

\item{Y_point}{response variable for geostatistical data (only if there is geostatistical data).}

\item{X_point}{covariates for geostatistical data (only if there is geostatistical data).}

\item{p_area}{number of coefficients for lattice model component (only if there is lattice data).}

\item{Y_area}{response variable for lattice data (only if there is lattice data).}

\item{X_area}{covariates for lattice data (only if there is lattice data).}

\item{geo.formula, lattice.formula}{formulas used for geostatistical and lattice data.}

\code{dstan} additionally contains:

\item{n_neighbor}{number of nearest neighbors to consider for NNGP modelling.}

\item{n_sample}{total number of sampling points.}

\item{nearid, nearind_sample}{vectors containing neighborhood indices}

\item{C_nei, C_site_nei, sC_nei, sC_site_nei}{various distance matrices}

\item{A1}{aggregation matrix that maps sampling points to areal averages (only if there is lattice data).}

\item{Y_pp}{the number of cases/events in each grid for point pattern data (only if there is point pattern data).}

\item{area}{the area of each grid (only if there is point pattern data).}

\item{grd_lrg}{the grid generated for point pattern data modeling (only if there is point pattern data).}

\item{locs}{all the locations where the latent components are modelled.}

\code{dinla} additionally contains:

\item{domain}{spatial domain as a \link[sp]{SpatialPolygons-class}}

\item{locs_point}{locations of geostatistical data.}

\item{locs_pp}{locations of point pattern data.}

\item{poly}{lattice data as a \link[sp]{SpatialPolygonsDataFrame-class}.}
}
\description{
Takes various datasets and formulas from different spatial data types and process them to prepare for spatial fusion modeling using either Stan or INLA.
}
\details{
It is not possible to add covariate for point pattern data in the spatial fusion framework. However, an offset term can be supplied to \code{pp.offset} in the modelling stage with \code{\link{fusion}}. Any covariate information can be taken into account by firstly fit a fixed effect model and enter the fitted values into the offset term.
}

\author{
Craig Wang
}

\seealso{\code{\link{fusion.dinla}}, \code{\link{fusion.dstan}}}

\examples{
## example based on simulated built-in data

dat <- fusionData(dataGeo, lungfunction ~ covariate,
           dataLattice, mortality ~ covariate,
           dataPP, distribution = c("normal","poisson"),
           domain = dataDomain,
           method = "INLA")

\donttest{
if (requireNamespace("INLA", quietly = TRUE)) {
## fit a spatial fusion model on the prepared data
## pp.offset = 400 was chosen based on simulation parameters
mod <- fusion(data = dat, n.latent = 1, bans = 0, pp.offset = 400,
           prior.range = c(0.1, 0.5), prior.sigma = c(1, 0.5),
           mesh.locs = dat$locs_point, mesh.max.edge = c(0.5, 1))

## parameter estimates
summary(mod)
}
}
}
\keyword{methods}

