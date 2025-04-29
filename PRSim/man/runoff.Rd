\name{runoff}
\alias{runoff}
\docType{data}
\title{
Sample runoff of a catchment
}
\description{
Artifical runoff data based on actual and simulated observations.
}
\usage{data("runoff")}
\format{
  A data frame with 15695 observations of the following 4 variables.
  \describe{
    \item{\code{YYYY}}{a numeric vector, year}
    \item{\code{MM}}{a numeric vector, month}
    \item{\code{DD}}{a numeric vector, day}
    \item{\code{Qobs}}{a numeric vector, synthetic observed runoff}
  }
}
\details{
The data mimiks the runoff of the river Plessur at the gauging station Chur, Switzerland. The the flow regime of the river is melt dominated. More information is given in the reference below.
}
\source{
The provided data is a weighted average of the acutually observed values and a particular simulated runoff. 
The actual discharge data can be ordered from
\url{http://www.bafu.admin.ch/wasser/13462/13494/15076/index}. 
}
\references{
Brunner, M. I., A. Bárdossy, and R. Furrer (2019). Technical note: Stochastic simulation of streamflow time series using phase randomization. Hydrology and Earth System Sciences, 23, 3175-3187, https://doi.org/10.5194/hess-23-3175-2019.
}
\examples{
data(runoff)
str(runoff) 
runoff$timestamp <- paste(runoff$YYYY, runoff$MM, runoff$DD, sep=" ")
runoff$timestamp <- as.POSIXct(strptime(runoff$timestamp,
                                 format="\%Y \%m \%d", tz="GMT"))
plot(runoff$timestamp[1:1000], runoff$Qobs[1:1000], type="l", 
  xlab="Time [d]", ylab=expression(paste("Discharge [m"^3,"/s]")))
}
\keyword{datasets}
