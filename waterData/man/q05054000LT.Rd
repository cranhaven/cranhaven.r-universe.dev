\name{q05054000LT}
\docType{data}
\alias{q05054000LT}
\title{Long-term hydrologic dataset.}
\description{
  Daily mean streamflow for the Red River of the North
  at Fargo, North Dakota, streamgage 0504000, from October 1, 1949, to 
  September 30, 2010.
}
\details{
  Streamflow data for U.S. Geological Survey streamgage 05054000, Red 
  River of the North at Fargo, North Dakota, are provided for the 
  vignette. The vignette provides the code (commented out) to import 
  the data from the USGS, but such a large retrieval can timeout, so 
  the data are provided with the package.
}
\usage{q05054000LT}
\format{A dataframe containing 22,280 observations of daily mean 
  streamflow.  There are  four variables shown in the following table.
  \tabular{lll}{
   Name \tab Type \tab Description \cr 
   staid \tab factor \tab USGS Station identification number \cr
   val \tab numeric \tab Daily mean streamflow \cr
   dates \tab Date \tab Date of daily mean \cr
   qualcode \tab factor \tab Qualification code
  }
}

\source{Imported from the USGS Daily Values Site Web Service 
  \url{http://waterservices.usgs.gov/rest/DV-Service.html} (U.S. 
  Geological Survey, 2014) using the function \link{importDVs}.
}
\references{
  U.S. Geological Survey, 2014, National Water Information System (NWISWeb): 
  U.S. Geological Survey database, accessed November 18, 2014, at 
  \url{http://waterservices.usgs.gov/}.
}
\keyword{datasets}


