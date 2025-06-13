\name{misQ05054000}
\docType{data}
\alias{misQ05054000}
\title{Daily mean streamflow with missing values}
\description{
  Daily mean streamflow data with randomly generated missing values.
  \if{html}{\figure{misQ05054000.png}{Red River of the North, North Dakota, 
  streamflow, in cubic feet per second, on a logarithmic scale, with missing 
  values.}}
  \if{latex}{\figure{misQ05054000.pdf}{Red River of the North, North Dakota, 
  streamflow, in cubic feet per second, on a logarithmic scale, with missing 
  values.}}  
}
\details{
  Streamflow data for USGS streamgage 05054000, Red River of the North
  at Fargo, North Dakota, from January 1, 2000, to December 31, 2010,
  were downloaded and then 2,000 of the 4,018 daily values were randomly
  replaced with NAs.  This provides a dataset with slightly less than
  50 percent of the values missing; however, the blocks of missing values
  are less than 30 days long. Definitions of the daily data qualification codes can be 
  found at 
  \url{http://help.waterdata.usgs.gov/codes-and-parameters/daily-value-qualification-code-dv_rmk_cd} 
  (U.S. Geological Survey, 2011).
}
\usage{misQ05054000}
\format{A dataframe containing 4,018 observations of daily mean 
  streamflow.  There are  four variables, staid, the USGS streamgage station 
  number; val, the daily mean streamflow value in cubic feet per second; 
  dates, the dates of the observations; and qualcode, the USGS data 
  qualification code.
  \tabular{lll}{
   Name \tab Type \tab Description \cr 
   staid \tab factor \tab USGS station identification number \cr
   val \tab numberic \tab Daily mean streamflow \cr
   dates \tab Date \tab Date of daily mean \cr
   qualcode \tab factor \tab Qualification code
  }
}
\examples{
data(exampleWaterData)
par(las=1, tck=0.02, mar=c(5,5,4,2), mex=1.2, cex.axis=.9)
plot(misQ05054000$dates, misQ05054000$val, type="l", log="y", 
     ylab="Streamflow, in cubic feet per second", 
     xlab="", yaxs='i', xaxs='i', 
     xlim=c(as.Date("2000-01-01"), as.Date("2012-01-01")), ylim=c(10,50000), 
     yaxt="n")
axis(2, axTicks(2), format(axTicks(2), big.mark=","))
title("Red River of the North at Fargo, North Dakota")
summary(misQ05054000$val)
}
\source{Imported from the USGS Daily Values Site Web Service 
  \url{http://waterservices.usgs.gov/rest/DV-Service.html} (U.S. 
  Geological Survey, 2012) using the function \link{importDVs}, with
  random replacements of data with NAs.
}
\references{
  U.S. Geological Survey, 2011, Daily value qualification code (dv_rmk_cd): 
  National Water Information System: Help System, accessed September 7, 
  2012, at 
  \url{http://help.waterdata.usgs.gov/codes-and-parameters/daily-value-qualification-code-dv_rmk_cd}.\cr\cr
  U.S. Geological Survey, 2012, National Water Information System (NWISWeb): 
  U.S. Geological Survey database, accessed May 2, 2012, at 
  \url{http://waterservices.usgs.gov/}.
}
\keyword{datasets}
\keyword{NA}
\keyword{ts}

