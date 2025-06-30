\name{map.statistic}
\alias{map.statistic}

\title{Mapping of a spatial index
}
\description{
	Mapping of a spatial index with circles whose sizes and colors vary with the values of the index
}
\usage{
map.statistic(coordinates, statistic, region, cex.circles = c(3, 0.2), legend, 
	main = NULL, add = FALSE)
}
\arguments{
  \item{coordinates}{
a 2-column matrix with latitudes and longitudes of observation sites.
}
  \item{statistic}{
a numeric vector specifying the values, at observation sites, of the index to be mapped.
}
  \item{region}{
a list of arguments defining the region over which the index is mapped:
	\describe{
		\item{\code{border}:}{a character string that names the region in the world within which the statistic will be predicted. This is a character string in the database \code{world}; see the function \code{map} in the \code{maps} package.}

		\item{\code{xlim}:}{a numeric vector of size 2 specifying the limits in longitude of the plot. May not be provided.}

		\item{\code{ylim}:}{a numeric vector of size 2 specifying the limits in latitude of the plot. May not be provided.}
	}
	}
  \item{cex.circles}{ 
  		a numeric vector specifying the circle expansion.
	}
  \item{legend}{
a list of arguments defining the location of the legend in the plot:
	\describe{
		\item{\code{x}:}{a numeric vector specifying the longitudes where the legend circles are drawn.}
		\item{\code{y}:}{a numeric vector specifying the latitudes where the legend circles are drawn.}
		\item{\code{xtext}:}{a numeric vector specifying the longitudes where the values corresponding to the sizes of the legend circles are drawn.}
		\item{\code{ytext}:}{a numeric vector specifying the latitudes where the values corresponding to the sizes of the legend circles are drawn.}
		\item{\code{digits}:}{integer indicating the number of decimal places to be used.}
	}
	}
  \item{main}{
a character string providing an overall title for the plot.
}
  \item{add}{
  	a logical indicating whteher the plot must be added to a current plot (if \code{add = TRUE}) or not (if \code{add = FALSE}), default is FALSE.
}
}
\value{
a plot.
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
	Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}
\seealso{map.statistic
\link{rain.feedback.stats}.
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
\keyword{misc}

