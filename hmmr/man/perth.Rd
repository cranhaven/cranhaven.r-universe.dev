\name{perth}

\docType{data}

\alias{perth}

\title{Perth dams water levels.}

\description{
	
	Data from the Water Corporation of Western Australia. They state the following 
	about these data on their website:
	"Streamflow is the amount of water entering our dams from our catchments 
	and is measured by changing water storage levels."
	This dataset has the annual averages of these storage levels. 
}

\usage{
data(perth)
}

\format{

A data.frame consisting of the following variable:
  \describe{
	\item{\code{water}}{water level (in GL)}
  	\item{\code{year}}{year}
  	\item{\code{wtmin1}}{water level in the previous year (GL)}

  }
  
}

\source{
	
	These data are provided by the Water Corporation of Western Australia and can be found here: 
	\url{https://www.watercorporation.com.au/water-supply/rainfall-and-dams/streamflow/streamflowhistorical}
	
}

\examples{

# the data is first changed to a timeseries object and then plotted
data(perth)
wts <- ts(perth$water,start=1911)
plot(wts,ylab="GL", main="Perth dams water inflow", xlab="year", frame=FALSE, xaxp=c(1910,2020,10))

}

\keyword{datasets}
