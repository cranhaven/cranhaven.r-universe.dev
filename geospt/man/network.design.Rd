\name{network.design}
\alias{network.design}

\title{
Generating \code{AKSE} associated with a conditioned network design
}
\description{
Generates a sampling network for a given sampling distance or type (configuration), and calculates the average kriging standard error (\code{AKSE}) associated with the spatial configuration for a given predefined variogram
}
\usage{
network.design(formula, vgm.model, xmin, xmax, ymin, ymax, npoint.x, npoint.y,
npoints, boundary=NULL, type, ...)
}

\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of independent variables; suppose the dependent variable has name z, for ordinary and simple kriging use the formula \code{z~1}; for simple kriging also define beta (see below); for universal kriging, suppose \code{z} is linearly dependent on \code{x} and \code{y}, use the formula \code{z~x+y}}
\item{vgm.model}{variogram model of dependent variable (or its residuals), defined by a call to vgm or fit.variogram}
\item{npoint.x}{number of points to generate on the \code{x}-axis}
\item{npoint.y}{number of points to generate on the \code{y}-axis}
\item{npoints}{(approximate) sample size inside (shapefile) border}
\item{xmin}{minimum \code{x}-coordinate of the study area.}
\item{ymin}{minimum \code{y}-coordinate of the study area.}
\item{xmax}{maximum \code{x}-coordinate of the study area.}
\item{ymax}{maximum \code{y}-coordinate of the study area.}
\item{boundary}{SpatialPolygons or SpatialPolygonsDataFrame object}
\item{type}{character; "random" for completely spatial random; "regular" for regular (systematically aligned) sampling; "stratified" for stratified random (one single random location in each "cell"); "nonaligned" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates); "hexagonal" for sampling on a hexagonal lattice; "clustered" for clustered sampling; "Fibonacci" for Fibonacci sampling on the sphere (see references). By default type ="regular".}
\item{...}{further arguments will be passed of the \code{\link[gstat]{krige}} and \code{\link[sp]{spsample}} functions.}
}
\references{
Fibonacci sampling: Alvaro Gonzalez, 2010. \emph{Measurement of Areas on a Sphere Using Fibonacci and Latitude-Longitude Lattices}. Mathematical Geosciences 42(1), p. 49-64
}
\value{
returns the \code{AKSE} value associated with the spatial distribution of points and the kriging method used.
}
\seealso{
\code{\link[gstat]{krige}},  \code{\link[gstat]{krige.cv}}, \code{\link[sp]{spsample}}, \code{\link[sp]{point.in.polygon}}, \code{\link{sample}}
}
\examples{
\dontrun{
### regular grid 10x10
vgmok <- vgm(112.33, "Sph", 4.3441,0)
vgmsk <- vgm(74.703, "Sph", 3.573,0)
vgmuk <- vgm(53.064, "Sph", 2.8858,0)
vgmuk2 <- vgm(19.201, "Sph", 1.5823,0)
# network: ordinary kriging (without boundary)
net1.ok <- network.design(z~1,vgmok, xmin=0,xmax=10, ymin=0, ymax=10, npoint.x=10,
                          npoint.y=10, nmax=6)
net2.ok <- network.design(z~1,vgmok, xmin=0,xmax=10, ymin=0, ymax=10, npoint.x=20,
                          npoint.y=20, nmax=6)
# it's worth noting that the variograms are different in each kriging
# network: simple kriging (without boundary)
net1.sk <- network.design(z~1,vgmsk, xmin=0,xmax=10, ymin=0, ymax=10, npoint.x=10,
                          npoint.y=10, nmax=6, beta=2)
net2.sk <- network.design(z~1,vgmsk, xmin=0,xmax=10, ymin=0, ymax=10, npoint.x=20,
                          npoint.y=20, nmax=6, beta=2)
# network: universal kriging, first order trend (without boundary)
net1.uk <- network.design(z~x + y,vgmuk, xmin=0,xmax=10, ymin=0, ymax=10,
                          npoint.x=10, npoint.y=10, nmax=8)
# network: universal kriging, second order trend (without boundary)
net2.uk <- network.design(z~x + y + x*y + I(x^2)+I(y^2),vgmuk2, xmin=0,xmax=10, ymin=0,
                          ymax=10, npoint.x=20, npoint.y=20, nmax=8)
# Creating the grid with the prediction and plotting points
library(geoR)
data(ca20)
Sr1 <- Polygon(ca20$borders)
Srs1 = Polygons(list(Sr1), "s1")
Polygon = SpatialPolygons(list(Srs1))
vgmok.ca <- vgm(112.33, "Sph", 244.9,0)
vgmsk.ca <- vgm(100, "Sph", 150.2,0)
vgmuk.ca <- vgm(85.57, "Sph", 110.5,0)
vgmuk2.ca <- vgm(62.14, "Sph", 89.7,0)
# network: ordinary kriging (with boundary)
netb1.ok<- network.design(z~1, vgmok.ca, npoints=50, boundary=Polygon, nmax=6)
netb2.ok<- network.design(z~1, vgmok.ca, npoints=100, boundary=Polygon, nmax=6)
# network: simple kriging (with boundary)
netb1.sk <- network.design(z~1, vgmsk.ca, npoints=50, boundary=Polygon, nmax=6, beta=2)
netb2.sk <- network.design(z~1, vgmsk.ca, npoints=100, boundary=Polygon, nmax=6, beta=2)
# network: universal kriging, first order trend (with boundary)
netb1.uk <- network.design(z~x + y, vgmuk.ca, npoints=50, boundary=Polygon, nmax=8)
# network: universal kriging, second order trend (with boundary)
netb2.uk <- network.design(z~x + y + x*y + I(x^2)+I(y^2), vgmuk2.ca, npoints=100,
                           boundary=Polygon, nmax=8)
}
}

\keyword{ spatial }
