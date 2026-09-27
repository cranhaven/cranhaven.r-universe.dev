\name{infilt}
\alias{infilt}
\docType{data}
\title{
Water infiltration characteristics data
}
\description{
This is part of a dataset from a PhD study  which measured water infiltration characteristics from the Upper Athi River basin in Eastern Kenya. It contains rate of infiltration (y) at different levels of cummulative Time intervals (x) 
}
\usage{data(infilt)}
\format{
   A data frame with 1105 observations on the following 6 variables.
  \describe{
    \item{\code{Sample}}{ which is a numeric vector}
    \item{\code{PlotNo}}{which is a factor with levels such as: \code{10lP3} \code{11lP3} \code{12lP3} \code{13lP3} \code{14lP3} \code{15lP3} \code{16lP3} \code{17lP3} \code{18lP3} \code{19lP3} \code{1lP3} \code{20lP3} \code{21lP3} \code{22lP3} \code{23lP3} \code{24lP3} \code{25lP3} \code{26lP3} \code{27lP3} \code{28lP3} \code{29lP3} \code{2lP3} \code{30lP3} \code{3lP3} \code{4lP3} \code{5lP3} \code{6lP3} \code{7lP3} \code{8lP3} \code{9lP3}}
    \item{\code{Erosion}}{which is a factor with levels such as: \code{E0} \code{E1} \code{E2}}
    \item{\code{Time}}{which is a numeric vector of cumulative infiltration time}
    \item{\code{Rate}}{which is a numeric vector of instantaneous infiltration rate}
    \item{\code{Cumrate}}{which is a numeric vector of cumulative infiltration rate}
  }
}
\details{
The data is grouped according to plots (given the name PlotNo) from where the data were collected
}
\source{
Omuto CT. 2006. Large-area soil physical degradation assessment using gis, remote sensing, and infrared spectroscopy in arid and semi-arid Kenya. PhD Dissertation, University of Nairobi, Kenya 
}
\references{
Omuto CT. 2006. Large-area soil physical degradation assessment using gis, remote sensing, and infrared spectroscopy in arid and semi-arid Kenya.PhD Dissertation, University of Nairobi, Kenya
}
\examples{
data(infilt)
str(infilt) 
}
\keyword{datasets}
