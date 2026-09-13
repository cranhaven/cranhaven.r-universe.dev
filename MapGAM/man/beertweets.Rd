\name{beertweets}
\alias{beertweets}
\docType{data}
\title{
Geocoded Tweets with Beer Indicator}
\description{
Geocoded tweets from Twitter, with an indicator variable for any mention of beer, time stamp, and state of origin.
}
\usage{data(MAmap)}
\format{
  A data frame with 10000 observations on 5 variables:
  \describe{
    \item{\code{beer}}{1 for tweets about beer, 0 for other tweets.}
    \item{\code{longitude}}{geocoded longitude.}
    \item{\code{latitude}}{geocoded latitude.}
    \item{\code{state}}{a factor with the name of the state.}
    \item{\code{time}}{a list of POSIXlt format dates and times for the tweets.}
  }
}
\details{
A sample of geocoded tweets from within in the contiguous US, from June to October of 2012.  Tweets mentioning beer (cases) are oversampled by a factor of 100.  Geocoding is typically at the level of city or town; tweets that could not be geocoded were excluded from this data set.          
% What time zone(s) for time variable?
}
\source{
Dr. Matthew Zook, University of Kentucky, floatingsheep.org
}
\examples{
data(beertweets)
attach(beertweets)
plot(longitude,latitude,col=beer+1)  # beer in red, non-beer in black
}
\keyword{datasets}
