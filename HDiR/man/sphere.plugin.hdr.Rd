\encoding{utf8}
\name{sphere.plugin.hdr}
\alias{sphere.plugin.hdr}
 \title{
Spherical plug-in estimation of HDRs
}
\description{
This function computes the spherical plug-in estimator of HDRs.
}
\usage{
sphere.plugin.hdr(sample,bw="none",ngrid=500,
                  tau=NULL,level=NULL,nborder=1000,tol=0.01,
                  mesh=40,deg=3,plot.hdr=TRUE, col=NULL)
}
 \arguments{
  \item{sample}{
A matrix whose rows represent points on the unit sphere in Cartesian coordinates. If a row norm is different from one, a message appears indicating that they must be standardized.}
  \item{bw}{Smoothing parameter to be used. It can be a numeric value directly selected by the user. According to \code{Directional} package, this could be also either \code{"none"} for cross validation or \code{"rot"} for the rule of thumb suggested by García-Portugués (2013).  Moreover, function \code{sphere.boot.bw} allows to select the new smoothing parameter proposed in Saavedra-Nieves and Crujeiras (2021). Default \code{bw="none"}.}
  \item{ngrid}{
Sets the resolution of the density calculation. Default \code{ngrid=500}.}
  \item{tau}{
Numeric probability. According to Saavedra-Nieves and Crujeiras (2021), \code{1-tau} represents the probability coverage required for HDR. If \code{tau=NULL}, a value for the threshold \code{level} of the HDR must be provided.}
  \item{level}{Numeric threshold of the HDR provided by the user. When \code{level} is larger than the maximum value of the density, the HDR is equal to the emptyset. If \code{level} is smaller than the minimum of the density, the HDR coincides with the support distribution. If \code{level=NULL}, a value for the probability coverage \code{1-tau} of the HDR must be provided.}

  \item{nborder}{
Maximum number of HDRs boundary points to be represented. Default \code{nborder=} \code{1000}.}

  \item{tol}{Tolerance parameter to determinate the boundary of HDRs. Default \code{tol=0.01}.
}

  \item{mesh}{
A numeric value 10, 20 or 40 indicating the 3D cartesian mesh used for numerical integration on the unit shere. Default \code{mesh=40} considering a total of 32000 triangular cells on the sphere. If \code{mesh=20} or \code{mesh=10}, 8000 triangular cells or 2000 are considered, respectively.
}
  \item{deg}{Integer string indicating the degree (from 0 to 6) of the quadrature rules for triangles on the sphere. Default \code{deg=6}.
}
  \item{plot.hdr}{
Logical string. If \code{TRUE}, the boundary of the level set or the HDR are represented graphically. Default \code{plot.hdr=TRUE}. }
  \item{col}{
Color number for plotting the boundary of the HDR. Default \code{"darkgray"}.}

}
\details{
A detailed definition of plug-in estimators for directional HDRs is given in Saavedra-Nieves and Crujeiras (2021). Moreover, the density quantile algorithm proposed in Hyndman (1996) is used to compute the threshold of HDR.  }

\value{
If \code{tau} is provided, a list with the next components:
\item{hdr}{A matrix of rows of points on the HDR boundary.}
\item{prob.content}{Probability coverage \code{1-tau}.}
\item{level}{Threshold associated to the probability content \code{1-tau}.}
\item{bw}{Value of the smoothing parameter used for kernel density estimation.}
If \code{level} is provided, a list with the next components:
\item{levelset}{A matrix of rows of points on the level set boundary or a character indicating if the level set is equal to the emptyset or the support distribution.}
\item{prob.content}{Probability coverage \code{1-tau} associated to the \code{level} value.}
\item{level}{Threshold of the level set.}
\item{bw}{Value of the smoothing parameter used for kernel density estimation.}


}
\references{
García-Portugués, E. (2013). Exact risk improvement of bandwidth selectors for kernel density
estimation with directional data. Electronic Journal of Statistics, 7, 1655-1685.\cr
Hyndman, R.J. (1996). Computing and graphing highest density regions, The American Statistician, 50, 120-126.\cr
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}

\examples{
\donttest{
# Plug-in HDR estimator for spherical model 9 in HDiR package
set.seed(1)
sample=rspheremix(1000, model =9)
sphere.plugin.hdr(sample,tau=0.8,col="red")

#Plug-in HDR estimator for data on earthquakes on Earth
if (requireNamespace("ggplot2", quietly = TRUE)) {
library(ggplot2)
}
if (requireNamespace("maps", quietly = TRUE)) {
library(maps)
}
if (requireNamespace("mapproj", quietly = TRUE)) {
library(mapproj)
}
data(earthquakes)
library(Directional)
hdr08<-as.data.frame(euclid.inv(sphere.plugin.hdr(euclid(earthquakes),tau=0.8,
plot.hdr=FALSE)$hdr))
world <- map_data("world")
g.earthquakes <- ggplot() +
  geom_map(data = world, map = world,
           mapping = aes(map_id = region),
           color = "grey90", fill = "grey80") +
  geom_point(data = earthquakes,
            mapping = aes(x = Longitude, y = Latitude),
           color = "red",alpha=.2,size=.75,stroke=0) +
  geom_point(data = hdr08,
            mapping = aes(x = Long, y = Lat),
            color = "darkblue", size = 1) +
  scale_y_continuous(breaks = NULL, limits = c(-90, 90)) +
 scale_x_continuous(breaks = NULL, limits = c(-180, 180)) +
  coord_map("mercator")
g.earthquakes
}
}
