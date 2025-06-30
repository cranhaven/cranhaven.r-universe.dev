\name{krige.test}
\alias{krige.test}

\title{
Test of spatial variation in feedback and change-in-feedback
}
\description{
Randomization test to investigate spatial variation in a kriged index.
}
\usage{
krige.test(krige.output, subregion, alternative, nb.rand, subregion.coverage=0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{krige.output}{
		a list of items corresponding to the output of the \link{krige} function.
}
  \item{subregion}{
		a list of two vectors containing the lat/long coordinates of the vertices of a polygon. The polygon defines a subregion where one supposes variation in the predicted index \code{krige.output$krige$predict}.
\describe{
	\item{\code{x}:}{longitudes.}
	\item{\code{y}:}{latitudes.}
}
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of "greater" or "less". 
}
  \item{nb.rand}{
a positive integer specifying the number of randomizations (here, a randomization is a random translation on a torus).
}
  \item{subregion.coverage}{
a numeric value between zero and one specifying a criterion to accept a random translation (see Details).  Default value is 0.8.
}
}
\details{
The criterion to accept a random translation is a minimum coverage of \code{subregion} by the translated grid: the ratio between the number of nodes of the translated grid that are contained in \code{subregion} and the number of nodes of the original grid that are contained in \code{subregion} must be equal to or greater than \code{subregion.coverage}.
}
\value{
	an object from the \link{KT.output class}
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
	Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}

\seealso{
	\link{krige}, \link{KT.output-class}, \link{rain.feedback.stats}
}
\examples{
#### load data of feedback and change-in-feedback indices in 88 sites across Australia
data(rain.feedback.stats)

#### spatial coordinates of the 88 sites
coord=rain.feedback.stats[,3:4]

#### map of feedback index computed from the whole data series
stat1=rain.feedback.stats[["Feedback.whole.period"]]

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
\keyword{misc}
