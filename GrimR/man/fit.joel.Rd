\name{fit.joel}
\alias{fit.joel}
\title{Function fit.joel}
\usage{
fit.joel(Data, MR = NULL, cw = c("ccw", "cw"),optimMR=FALSE)
}
\arguments{
\item{Data}{(data frame) containing the spindle angles S and the extinction angles ES}

\item{MR}{(numeric) The reference azimuth; If numeric and optimMR==TRUE, this value will be used as a starting value for further optimization. If NULL, a starting value will be guessed. }

\item{cw}{(character) string  "cw" for a clockwise graduated table, "ccw" for a counter-clockwise graduated table (default)}
\item{optimMR}{(logical) If FALSE, the provided MR will be used without further refinement, if TRUE, the MR will be refined so as to minimize the deviance}
}
\value{
(list) with elements: \cr
\item{coeffs}{list of the fitted parameters}
\item{covmat}{matrix of covariances of the parameters}
\item{delta2V}{list of estimate of 2V, its standard deviation and upper and lower confidence limits}
\item{kart}{data frame with cartesian coordinates of the axes, sd, and confidence intervals}
\item{sphaer}{data frame with S and ES values of the axes, sd, and confidence intervals}
\item{principal}{data frame with S and MS angles to bring axes into extinction}
\item{Extinctions}{data frame with S, MS, ES, calculated ES and ES-ES calculated}
\item{Wulffdat}{data necessary to create a plot on the Wulff stereonet}
}
\description{
Calculate the angle between the optical axes 2V, the optical axes in
cartesian and polar coordinates and the principal axes of the dielectric tensor
in cartesian and polar coordinates.
}
\examples{
# With 360 deg. data:
res<-fit.joel(Carman,MR=NULL,cw="ccw",optimMR=TRUE)
Wulffplot(res) #Plot data on a Wulff net
#with 180 degree data:
res<-fit.joel(Gunter,MR=-0.89,cw="cw",optimMR=FALSE)
Wulffplot(res) #Plot data on a Wulff net
}
\author{Florian Dufey <GrimR@gmx.de> }
