\name{krige}
\alias{krige}

\title{
	Kriging function
}
\description{
Variogram analysis and kriging prediction used to analyze feedback and
	change-in-feedback across space. This function is
	grounded on the function \code{autoKrige} in the \code{automap}
	package, grounded itself on the \code{gstat} package.
}
\usage{
krige(coordinates, statistic, grid, krige.param=NULL, plots=TRUE,
    variog.param=list(npoints=50,nsim=99,plot.numbers=0.04))
}

\arguments{
  \item{coordinates}{a 2-column matrix with latitudes and longitudes of observation sites.
}
\item{statistic}{a numeric vector specifying the values, at observation sites, of the statistic to be predicted.
}

\item{grid}{a list of arguments defining the grid over which the statistic is predicted:
 	\describe{
		\item{\code{x}:}{a numeric vector of longitudes.}
		\item{\code{y}:}{a numeric vector of latitudes.}
		\item{\code{border}:}{a character string that names the region in the world within which the statistic will be predicted. This is a character string in the database \code{world}; see the function \code{map} in the \code{maps} package.}
		\item{\code{proj}:}{a projection definition; see the function \code{project} in the \code{proj4} package.}
		\item{\code{degrees}:}{a logical indicating  if  the lat/long coordinates are assumed to be in degrees (if \code{degrees = TRUE}), or in radians (if \code{degrees = FALSE}).}
	}
	}

\item{krige.param}{a character string equal either to "x+y", "x" or "y"
  indicating whether the coordinates (and which coordinates) have to be
  accounted for in the trend in universal kriging; default is NULL, indicating that
  ordinary kriging without trend is performed.}

  \item{plots}{
a logical indicating if plots characterizing the variogram analysis and the kriging prediction are produced (if \code{plots = TRUE}) or not (if \code{plots = FALSE}), default is TRUE.
}


\item{variog.param}{a list of arguments used to display the variography,  :
 \describe{
\item{\code{npoints}:}{a numeric value specifying the number of points at
 which the variogram model is calculated, default is 50.}

\item{\code{nsim}:}{a numeric value specifying the number of Monte Carlo
          simulations used to compute the envelope, default
          is 99.}

\item{\code{plot.numbers}:}{a numeric value specifyng the relative
  distance to be used between circles representing the empirical
  semivariogram and texts providing the number of point pairs used for
  computing each value of the empirical semivariogram, default is
  0.04 (if \code{plot.numbers = 0}, these texts are not displayed).}
	
}
}

}
\details{
	If \code{plots = TRUE}, four plots are produced. Plot 1:
	Estimation of the semivariogram of the statistic (dots: sample
	semivariogram; solid curve: theoretical semivariogram; dashed
	curves: Monte-Carlo envelopes. Plot 2: Boxplots of kriging prediction (left) and kriging standard error (right). Plot 3: Kriging prediction. Plot 4: Kriging standard error.
}
\value{
a list of items characterizing the variogram analysis and the kriging prediction:
\item{input}{the list of arguments in the call of the \code{krige} function except the argument \code{plots}.
}
\item{MAP}{a list allowing to draw the border of the study region that can be made of several polygons. This is the output of the function \code{map} in the \code{maps} package.}
\item{grid}{a 2-column matrix providing the coordinates (in degrees) of the nodes of the prediction locations.}
\item{in.region}{a logical vector indicating, for each grid node whose coordinates are given in  the 2-column matrix \code{grid}, if the node is in the study region or not. The study region is defined by \code{MAP}.}
\item{krige}{a list providing the result the variography and the spatial prediction. This is the output of the function \code{autoKrige} in the \code{automap} package.}
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
	Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}
\note{
	The \code{krige} function uses some of the functionalities of
	the \code{map} function in the \code{maps} package, and of a few
	functions in the \code{gstat} and \code{automap} package. To
	fully exploit the functionalities of these external functions
	(in particular \code{autoKrige}), the \code{krige} function should be extended.
}


\seealso{\code{rain.feedback.stats}, \code{map} in the \code{maps} package, \code{autoKrige}  and \code{autofitVariogram} in the \code{automap} package
}
\examples{
#### load data of feedback and change-in-feedback indices in 88 sites across Australia
data(rain.feedback.stats)

#### spatial coordinates of the 88 sites
coord=rain.feedback.stats[,3:4]

#### feedback index
stat1=rain.feedback.stats[["Feedback.whole.period"]]

#### variogram analysis and kriging of feedback index
## computer intensive stage
\dontrun{
par(mfrow=c(2,2), mar=c(5.1,4.1,4.1,4.1))
kr1=krige(coordinates=coord, statistic=stat1,
   grid=list(x=seq(110,155,0.25),y=seq(-45,-11,0.25),border="Australia", 
      proj="+proj=lcc +lat_1=-18 +lat_2=-36 +lat0=-25 +lon_0=140",degrees=TRUE),
   plots=TRUE)

## the plot style from the \code{automap} package can be obtained as follows:
plot(kr1$krige)
}

}

\keyword{ misc }
