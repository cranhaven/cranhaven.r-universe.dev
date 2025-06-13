\name{pH05082500}
\docType{data}
\alias{pH05082500}
\title{Hydrologic dataset with provisional and negative data.}
\description{
  Daily median pH for the Red River of the North
  at Grand Forks, North Dakota, streamgage 05082500, from March 31, 2001, to 
  December 31, 2011.  At the time these data were downloaded, some of it was 
  provisional data subject to revision.  
  \if{html}{\figure{pH05082500.png}{Red River of the North, at Grand Forks, 
  North Dakota, daily median, pH, water unfiltered,field, standard units.}}
  \if{latex}{\figure{pH05082500.pdf}{Red River of the North, at Grand Forks, 
  North Dakota, daily median, pH, water unfiltered,field, standard units.}}
}
\details{
  pH data for USGS streamgage 05082500, Red River of the North
  at Grand Forks, North Dakota, from March 31, 2001, to December 31, 2011.  
  At the time these data were downloaded, some of the data 
  were provisional and subject to revision, indicated by "P" in the qualcode
  column.  Definitions of the daily data qualification codes can be 
  found at 
  \url{http://help.waterdata.usgs.gov/codes-and-parameters/daily-value-qualification-code-dv_rmk_cd} 
  (U.S. Geological Survey, 2011).
}
\usage{pH05082500}
\format{A dataframe containing 1,737 rows.  There are  four variables, staid, 
  the USGS streamgage station number; val, the daily meadian pH value in 
  standard unites;  dates, the dates of the observations; and qualcode, the 
  USGS data qualification code.
  \tabular{lll}{
   Name \tab Type \tab Description \cr 
   staid \tab factor \tab USGS station identification number \cr
   val \tab numeric \tab Daily median pH \cr
   dates \tab Date \tab Date of daily value \cr
   qualcode \tab factor \tab Qualification code
  }
}
\examples{
data(exampleWaterData)
par(las=1, tck=0.02, mar=c(3,5,2,2), cex.axis=0.75)
plot(pH05082500$dates, pH05082500$val, type="l",  
     ylab="Daily median pH, water, \nunfiltered, field, standard units", 
     xlab="", yaxs='i', xaxs='i', ylim=c(7.4, 9), 
     xlim=c(as.Date("2007-01-01"), as.Date("2012-01-01")), col="red")
title("Red River of the North at Grand Forks, North Dakota",cex.main=0.95)
summary(pH05082500$val)
}
\source{Imported from the USGS Daily Values Site Web Service 
  \url{http://waterservices.usgs.gov/rest/DV-Service.html} (U.S. 
  Geological Survey, 2012) using the function \link{importDVs}, July 23, 2012.
}
\references{
  U.S. Geological Survey, 2011, Daily value qualification code (dv_rmk_cd): 
  National Water Information System: Help System, accessed September 7, 
  2012, at 
  \url{http://help.waterdata.usgs.gov/codes-and-parameters/daily-value-qualification-code-dv_rmk_cd}.\cr\cr
  U.S. Geological Survey, 2012, National Water Information System (NWISWeb): 
  U.S. Geological Survey database, accessed July 23, 2012, at 
  \url{http://waterservices.usgs.gov/}.
}
\keyword{datasets}
\keyword{NA}
\keyword{ts}

