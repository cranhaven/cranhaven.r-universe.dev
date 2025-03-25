\name{P1_20111216}
\alias{P1_20111216}
\docType{data}
\title{
Dataset: An example of input time series for the EmiStatR package
}
\description{
This dataset is a data.frame with four columns: time [y-m-d h:m:s] and precipitation depth, P [mm] for Dahl, Eshc-sur-Sure and Eschdorf rain gauge stations. 
The dataset correspond to measurements for December 16th 2011 for a 10-hour event recorded with time steps of 10 minutes. 
The three rain gauge stations are located close to the sub-catchment of the combined sewer overflow chamber at Goesdorf, Grand-Duchy of Luxembourg. 
}
\usage{data("P1_20111216")}
\format{
  A data frame with 61 observations on the following 4 variables.
  \describe{
    \item{\code{time}}{a POSIXct vector}
    \item{\code{Dahl}}{a numeric vector for rainfall depth in millimeters at Dahl rain gauge station}
    \item{\code{Esch-Sure}}{a numeric vector for rainfall depth in millimeters at Esch-sur-Sure rain gauge station}
    \item{\code{Eschdorf}}{a numeric vector for rainfall depth in millimeters at Eschdorf rain gauge station}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
\url{https://www.agrimeteo.lu}
}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data("P1_20111216")

plot(P1_20111216[,1], P1_20111216[,2], col="blue", typ="l", 
     xlab = "time", ylab = "Precipitation at Dahl [mm]") 
     
plot(P1_20111216[,1], P1_20111216[,3], col="blue", typ="l", 
     xlab = "time", ylab = "Precipitation at Esch-sur-Sure [mm]") 
     
plot(P1_20111216[,1], P1_20111216[,4], col="blue", typ="l", 
    xlab = "time", ylab = "Precipitation at Eschdorf [mm]") 

}
\keyword{datasets}
