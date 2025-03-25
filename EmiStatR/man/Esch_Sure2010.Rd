\name{Esch_Sure2010}
\alias{Esch_Sure2010}
\docType{data}
\title{
Dataset: An example time series for the EmiStatR package
}
\description{
This dataset is a list that contains a data.frame with two columns: time [y-m-d h:m:s] and precipitation depth, P [mm]. 
The dataset correspond to measurements for 2010 with time steps of 10 minutes at rain gauge station, Esch-sur-Sure, located close to the sub-catchment of the combined sewer overflow chamber at Goesdorf, Grand-Duchy of Luxembourg.
}
\usage{data("Esch_Sure2010")}
\format{
  A data frame with 52560 observations on the following 2 variables.
  \describe{
    \item{\code{time}}{a POSIXct vector}
    \item{\samp{P [mm]}}{a numeric vector}
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
data(Esch_Sure2010)

plot(Esch_Sure2010[,2], col="blue", typ="l", xlab = "time", ylab = "Precipitation [mm]")  

}

\keyword{datasets}
