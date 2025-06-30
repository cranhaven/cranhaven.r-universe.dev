\name{FeedbackTS-package}
\alias{FeedbackTS-package}
\alias{FeedbackTS}
\docType{package}
\title{
Analysis of Feedback in Time Series
}
\description{
Analysis of fragmented time directionality to investigate feedbacks in time series. Tools provided by the package allow the analysis of feedback for a single time series and the analysis of feedback for a set of time series collected across a spatial domain.
}
\details{
\tabular{ll}{
Package: \tab FeedbackTS\cr
Type: \tab Package\cr
Version: \tab 1.5\cr
Date: \tab 2020-01-22\cr
License: \tab GPL (>=2.0)\cr
Depends: \tab methods, maps, mapdata, proj4, sp, gstat, automap, date\cr
}
To analyze feedback in a single time series create a \link{KDD object} (Key Day Dataset) with the construction function \link{kdd.from.raw.data} and test fragmented time directionality with the function \link{feedback.test}. 
 
To analyze the spatial pattern of feedback from a set of time series collected across a spatial domain, create indices of feedback with the function \link{feedback.stats}, map the index with \link{map.statistic}, krige the index with \link{krige} and test spatial variation in feedback with \link{krige.test}. 
}
\author{
Samuel Soubeyrand, Cindy E. Morris, E. Keith Bigg

Maintainer: Samuel Soubeyrand \email{samuel.soubeyrand@inrae.fr}
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of
fragmented time directionality in time series to elucidate feedbacks in
climate data. Environmental Modelling and Software 61: 78-86.
}

\keyword{ package }

\examples{
#### load library
\dontrun{library(FeedbackTS)}

#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### load data of feedback and change-in-feedback indices in 88 sites across Australia
data(rain.feedback.stats)

#### spatial coordinates of the 88 sites
coord=rain.feedback.stats[,3:4]


########  ANALYSIS OF FEEDBACK WITH A SINGLE TIME SERIES

#### build KDD objects from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008,keyday.threshold=25,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,filter=NULL)

#### test feedback and change in feedback with a single data series
## using the thresholded data series
## using difference of means of positive indicator values (i.e. rainfall occurrence)
## computer intensive stage
\dontrun{
par(mfrow=c(1,2), mar=c(5.1,4.1,4.1,2.1))
feedback.test(object=KDD, test="feedback", operator="dmpiv", nb.rand=10^3, plots=TRUE)}

########  ANALYSIS OF FEEDBACK WITH A SET OF TIME SERIES COLLECTED ACROSS SPACE

#### map of feedback index computed from the whole data series
par(mfrow=c(1,1), mar=c(0,0,0,0))
stat1=rain.feedback.stats[["Feedback.whole.period"]]
map.statistic(coord,stat1,cex.circles=c(3,0.2),
   region=list(border="Australia",xlim=c(110,155)),
   legend=list(x=c(rep(114,3),rep(123,2)),y=-c(37,39.5,42,37,39.5),
      xtext=c(rep(114,3),rep(123,2))+1,ytext=-c(37,39.5,42,37,39.5),digits=2),
  main="Feedback")

#### variogram analysis and kriging of feedback index
## computer intensive stage
\dontrun{
par(mfrow=c(2,2), mar=c(5.1,4.1,4.1,2.1))
kr1=krige(coordinates=coord, statistic=stat1,
   grid=list(x=seq(110,155,0.25),y=seq(-45,-11,0.25),border="Australia", 
      proj="+proj=lcc +lat_1=-18 +lat_2=-36 +lat0=-25 +lon_0=140",degrees=TRUE),
   plots=TRUE)
}

#### test spatial variation in feedback index and plot test output
## computer intensive stage
\dontrun{
kt1=krige.test(krige.output=kr1,subregion=list(x=c(138,152,152,138),y=-c(40,40,33,33)),
  alternative="greater", nb.rand=2000)
par(mfrow=c(1,2), mar=c(5.1,4.1,4.1,2.1))	
plot(kt1,digits=list(predict=3,pvalue=3),breaks=12)
}
}
