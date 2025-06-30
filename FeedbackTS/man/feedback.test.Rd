\name{feedback.test}
\alias{feedback.test}

\title{Test of feedback and change-in-feedback for a single time series
}
\description{
Randomization test to investigate the fragmented time directionality in a single time series and the temporal variation in the fragmented time directionality.
}
\usage{
feedback.test(object, test, operator, nb.rand, plots = TRUE, turning.year = NULL)
}

\arguments{
  \item{object}{
either a \link{KDD object} or a \link{KDD.yearly.average object}.
}
  \item{test}{
a character string specifying the type of test, must be one of \code{"feedback"} or \code{"change.in.feedback"}.
}
  \item{operator}{
a character string specifying the transformation of the raw values, must be one of \code{"dmv"} or \code{"dmpiv"} (see section Details in \link{after.minus.before}).
}
  \item{nb.rand}{
a positive integer specifying the number of randomizations.
}
  \item{plots}{
a logical indicating if plots characterizing the test are produced (if \code{plots = TRUE}) or not (if \code{plots = FALSE}), default is TRUE. 
}
  \item{turning.year}{
an optional numeric value specifying a temporal turning point in the data series, must be provided if \code{operator = "change.in.feedback"}.
}
}
\details{
	If \code{plots = TRUE}, two plots are produced. The first plot gives the cumulative after-before difference (CABD; red curve) and for each CABD value the quantiles of order 0.025 (bottom dotted curve), 0.25 (bottom dashed curve), 0.5 (solid black curve), 0.75 (top dashed curve) and 0.975 (top dotted curve) under the null hypothesis that is tested (either no feedback or no change-in-feedback). The second plot gives the number of exits of the CABD function from the confidence intervals (red line) and the distribution of the number of exits under the null hypothesis that is tested (histogram).
}
\value{
	A numeric value providing the p-value of the test.
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
	Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}

\seealso{
\link{KDD}, \link{KDD.yearly.average}, \link{kdd.from.raw.data}, \link{after.minus.before}, \link{rain.site.6008}
}
\examples{
#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build KDD objects from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008, keyday.threshold=25, nb.days=20,
   col.series=5, col.date=c(2,3,4), na.rm=TRUE, filter=NULL)

#### test feedback and change in feedback with a single data series
## using the thresholded data series
## using difference of means of positive indicator values (i.e. rainfall occurrence)
par(mfrow=c(1,2), mar=c(5.1,4.1,4.1,2.1))
feedback.test(object=KDD, test="feedback", operator="dmpiv", nb.rand=10^3, plots=TRUE)
}
\keyword{ misc }

