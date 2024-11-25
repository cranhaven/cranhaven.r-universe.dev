\name{depthharm}
\alias{depthharm}
\title{
A function for harmonizing soil property between uniform depth intervals in observation pits
}
\description{
A function to harmonize soil property between uniform depth intervals in a set of observation pits
}
\usage{
depthharm(soildata, var.name, lam, d)
}
\arguments{
  \item{soildata}{
soil data containing soil property to be harmonized and observed depth intervals
}
  \item{var.name}{
name of target variable or soil property to be harmonized
}
  \item{lam}{
a factor to improve prediction of target soil property between sampled depths
}
  \item{d}{
target uniform depth intervals for harmonizing the target soil propert
}
}
\details{
Input soil data must be a dataframe or class of ProfileCollection. The smoothing factor improves prediction of the target soil propert. Its default value is 0.1. Desired depth intervals are seperated by comma and should be choosen between minimum and maximum depths in the soil data.
}
\value{
The output is a list of two dataframes:
harmonized.d is a dataframe of harmonized soil property at target depth intervals.
obs_n_pred is ugmented dataframe of observed and harmonized soil properties
}
\references{
Bishop, T.F.A., McBratney, A.B., Laslett, G.M., 1999. Modelling soil attribute depth functions with equal-area quadratic smoothing splines. Geoderma 91, 27–45. https://doi.org/10.1016/S0016-7061(99)00003-8

Malone, B.P., McBratney, A.B., Minasny, B., Laslett, G.M., 2009. Mapping continuous depth functions of soil carbon storage and available water capacity. Geoderma 154, 138–152. https://doi.org/10.1016/j.geoderma.2009.10.007

Ponce-Hernandez, R., Marriott, F.H.C., Beckett, P.H.T., 1986. An improved method for reconstructing a soil profile from analyses of a small number of samples. Journal of Soil Science 37, 455–467. https://doi.org/10.1111/j.1365-2389.1986.tb00377.x
}
\examples{
library(aqp)
library(plyr)
library(sp)
x=c(rep(2.12,4),rep(2.05,4))
y=c(rep(9.34,4),rep(8.17,4))
pit=c(rep(1,4),rep(2,4))
depthcode=c(1,2,3,4,1,2,3,4)
upper=c(0,18,25,35,0,12,33,50)
lower=c(10,25,35,67,12,33,50,100)
pH=c(6.7,5.5,5.1,6.7,6.4,5.8,5.3,5.0)
df=data.frame(pit,x,y,upper,lower,depthcode,pH)
lat=df$x;lon=df$y;id=df$pit;top=df$upper;
bottom=df$lower;horizon=df$depthcode;Varn=df$pH

soildata <- join(data.frame(id, top, bottom, Varn, horizon),
              data.frame(id, lat, lon), type='inner')
depths(soildata) <- id ~ top + bottom
site(soildata) <- ~ lat + lon
coordinates(soildata) <- ~ lat + lon
proj4string(soildata) <- CRS("+proj=longlat +datum=WGS84")
depth.s = depthharm(soildata, var.name= "Varn",
lam=0.01,d = t(c(0,10,40,80,100,150)))
plot(soildata, color= "Varn", name="horizon")
}

