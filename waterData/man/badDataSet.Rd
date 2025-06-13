\name{badDataSet}
\docType{data}
\alias{badDataSet}
\title{Hydrologic dataset with provisional and negative data.}
\description{
  Daily mean streamflow for the Red River of the North
  at Fargo, North Dakota, streamgage 0504000, from January 1, 2000, to 
  December 31, 2010.  At the time these data were downloaded, some of the data 
  were provisional and subject to revision, including a negative value.  
  \if{html}{\figure{badDataSet.png}{Red River of the North, North Dakota, 
  streamflow, in cubic feet per second, with a anomalous negative value.}}
  \if{latex}{\figure{badDataSet.pdf}{Red River of the North, North Dakota, 
  streamflow, in cubic feet per second, with a anomalous negative value.}}
}
\details{
  Streamflow data for U.S. Geological Survey streamgage 05054000, Red River of 
  the North at Fargo, North Dakota, from January 1, 2000, to December 31, 
  2010, were downloaded, then 2,000 of the 4,018 daily values were randomly
  changed to missing values.  At the time these data were downloaded, some of the data 
  were provisional and subject to revision, including a negative value.  For
  example purposes, the three smallest nonnegative values were changed to
  zero.  Data for this site and specific dates have since been revised and 
  approved.  Definitions of the daily data qualification codes can be 
  found at 
  \url{http://help.waterdata.usgs.gov/codes-and-parameters/daily-value-qualification-code-dv_rmk_cd} 
  (U.S. Geological Survey, 2011a).
}
\usage{badDataSet}
\format{A dataframe containing 4,018 observations of daily mean 
  streamflow.  There are  four variables shown in the following table.
  \tabular{lll}{
   Name \tab Type \tab Description \cr 
   staid \tab factor \tab USGS Station identification number \cr
   val \tab numeric \tab Daily mean streamflow \cr
   dates \tab Date \tab Date of daily mean \cr
   qualcode \tab factor \tab Qualification code
  }
}
\examples{
data(exampleWaterData)
par(las=1, tck=0.02, mar=c(5,5,4,2), mex=1.2, cex.axis=0.75)
plot(badDataSet$dates, badDataSet$val, type="l",  
     ylab="Streamflow, in cubic feet per second", 
     xlab="", yaxs='i', xaxs='i', 
     xlim=c(as.Date("2000-01-01"), as.Date("2012-01-01")), 
     yaxt="n", col="blue")
axis(2, axTicks(2), format(axTicks(2), big.mark=",", scientific=FALSE))
title("Red River of the North at Fargo, North Dakota")
summary(badDataSet$val)
}
\source{Imported from the USGS Daily Values Site Web Service 
  \url{http://waterservices.usgs.gov/rest/DV-Service.html} (U.S. 
  Geological Survey, 2011b) using the function \link{importDVs}, 
  modified by replacing the three smallest values with zero.  
  Data for this site and specific dates have since been revised and 
  approved.  The user would not be able to retrieve the same dataset from the USGS 
  Daily Values Web Site Service.
}
\references{
  U.S. Geological Survey, 2011a, Daily value qualification code (dv_rmk_cd): 
  National Water Information System: Help System, accessed September 7, 
  2012, at 
  \url{http://help.waterdata.usgs.gov/codes-and-parameters/daily-value-qualification-code-dv_rmk_cd}.\cr\cr
  U.S. Geological Survey, 2011b, National Water Information System (NWISWeb): 
  U.S. Geological Survey database, accessed November 2, 2011, at 
  \url{http://waterservices.usgs.gov/}.
}
\keyword{datasets}
\keyword{NA}
\keyword{ts}

