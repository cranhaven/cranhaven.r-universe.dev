\name{pendensity-package}
\alias{pendensity-package}
\docType{package}
\title{
The package 'pendensity' offers routines for estimating penalized unconditional and conditional (on factor groups) densities.
}
\description{
The package 'pendensity' offers routines for estimating penalized
unconditional and conditional (on factor groups) densities. For details
see the description of the function pendensity.
}
\details{
\tabular{ll}{
Package: \tab pendensity\cr
Type: \tab Package\cr
Version: \tab 0.2.12\cr
Date: \tab 2019-04-07\cr
License: GPL (>= 2)
LazyLoad: \tab yes\cr
}
The packages contributes the function 'pendensity()' for estimating densities using penalized splines techniques. 
}
\author{Christian Schellhase <christian.schellhase@gmx.net>}

\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\keyword{nonparametric}
\examples{
y <- rnorm(100)
test <- pendensity(y~1)
plot(test)

#################

#second simple example
#with covariate

x <- rep(c(0,1),200)
y <- rnorm(400,x*0.2,1)
test <- pendensity(y~as.factor(x),lambda0=2e+07)
plot(test)

#################

#calculate the value at some (maybe not observed) value yi=c(0,1) of the estimated density

plot(test,val=c(0,1))

#################

#density-example of the stock exchange Allianz in 2006

data(Allianz)
time.Allianz <- strptime(Allianz[,1],form="\%d.\%m.\%y")

#looking for all dates in 2006
data.Allianz <- Allianz[which(time.Allianz$year==106),2]

#building differences of first order
d.Allianz <- diff(data.Allianz)

#estimating the density, choosing a special start value for lambda
density.Allianz <- pendensity(d.Allianz~1,lambda0=90000)
plot(density.Allianz)

}
