\name{rain.feedback.stats}
\alias{rain.feedback.stats}
\docType{data}
\title{
	Statistics of rain feedback in Australia
}
\description{
	Feedback and change-in-feedback statistics based on 88 rainfall data series colllected in 88 sites across Australia. 

}
\usage{data(rain.feedback.stats)}
\format{
  A data frame with 88 observations on the following 8 variables.
  \describe{
    \item{\code{Station.number}:}{a numeric vector providing the identifiers of the meteorological stations.}
    \item{\code{Keyday.threshold}:}{a numeric vector providing for each meteorological station the threshold value above which a day is considered as a key day.}
    \item{\code{Longitude}:}{a numeric vector with longitudes of the meteorological stations.}
    \item{\code{Latitude}:}{a numeric vector with latitudes of the meteorological stations.}
    \item{\code{Feedback.whole.period}:}{a numeric vector providing for each meteorological station the temporal average of after-before differences around key days calculated from the whole time series.}
    \item{\code{Feedback.before.1960}:}{a numeric vector providing for each meteorological station the temporal average of after-before differences around key days calculated from the time series right-truncated in 1960 (data from year 1960 were excluded).}
    \item{\code{Feedback.after.or.in.1960}:}{a numeric vector providing for each meteorological station the temporal average of after-before differences around key days calculated from the time series left-truncated in 1960 (data from year 1960 were kept).}
    \item{\code{Change.in.feedback}:}{a numeric vector providing the difference between \code{Feedback.after.or.in.1960} and \code{Feedback.before.1960}.}
  }
}
\details{
The statistics in this data set were computed using the \link{feedback.stats} function.
}
\source{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\examples{
#### load data of feedback and change-in-feedback indices in 88 sites across Australia
data(rain.feedback.stats)

#### spatial coordinates of the 88 sites and corresponding feedback index 
#### computed from the whole data series
coord=rain.feedback.stats[,3:4]
stat1=rain.feedback.stats[["Feedback.whole.period"]]

#### map of feedback index 
map.statistic(coord,stat1,cex.circles=c(3,0.2),
   region=list(border="Australia",xlim=c(110,155)),
   legend=list(x=c(rep(114,3),rep(123,2)),y=-c(37,39.5,42,37,39.5),
      xtext=c(rep(114,3),rep(123,2))+1,ytext=-c(37,39.5,42,37,39.5),digits=2),
   main="Feedback")
}

\keyword{datasets}
