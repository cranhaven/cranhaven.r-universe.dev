\name{kdd.from.raw.data}
\alias{kdd.from.raw.data}
\title{
Build a KDD object from raw data
}
\description{
Build a KDD (Key Day Dataset) object from a matrix or a data frame containing a time series and other attributes.
}
\usage{
kdd.from.raw.data(raw.data, keyday.threshold, nb.days, col.series, col.date, 
na.rm = TRUE, filter = NULL)
}

\arguments{
  \item{raw.data}{
a data frame or a matrix containing raw data.
}
  \item{keyday.threshold}{
a numeric providing the threshold value above which a day is considered as a key day (i.e. if the value \eqn{y_i} of the times series is greater than or equal to \code{keyday.threshold} at day \eqn{i}, then day \eqn{i} is a key day).
}
  \item{nb.days}{
an integer specifying the number of days considered after and before each key day.
}
  \item{col.series}{
an integer specifying the number of the column containing the time series.
}
  \item{col.date}{
an integer vector of size three specifying the numbers of the columns containing the vector of years, the vector of months and the vector of days in numeric format.
}
  \item{na.rm}{
a logical indicating whether key days \eqn{i} with missing values in the sub-series \eqn{y_{i-K},\ldots,y_{i+K}} have to be discarded. If \code{na.rm = TRUE}, these key days are discarded (default is \code{TRUE}). 
}
  \item{filter}{
a list of lists specifying the filters to carry out over the time series (default is \code{NULL}).
}
}
\details{
The \code{filter} argument is a list of lists, each list having the following arguments:
	
\describe{
\item{\code{apply.over}:}{a character string that must be one of "keyday" or "range", and that indicates whether the filter concerns only  the key days or also the range of days considered around the key days (\eqn{K} days before and \eqn{K} days after each key day).}
	
\item{\code{column}:}{an integer specifying the column of \code{raw.data} which the filter is applied to.}
	
\item{\code{value}:}{a value that must be taken by the variable determined by the argument \code{column}. }
}	

Let \eqn{i} denote a key day. Let \eqn{z_i} denote the value, at day \eqn{i}, of the variable determined by the argument \code{column}. If \code{apply.over = "keyday"} and \eqn{z_i} = \code{value}, then key day \eqn{i} is kept, otherwise it is discarded.  If \code{apply.over = "range"} and \eqn{z_j} = \code{value} for all \eqn{j\in\{i-K,\ldots,i+K\}}, then key day \eqn{i} is kept, otherwise it is discarded.
}

\value{
an object from the \link{KDD class}.
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}


\seealso{
\link{KDD}, \link{kdd}, \link{rain.site.6008}
}
\examples{
#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build KDD objects from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008,keyday.threshold=25,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,filter=NULL)
summary(KDD)

#### build KDD objects from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
## using filters
rain.site.6008b=cbind(rain.site.6008,rain.site.6008[["Year"]]>=1960)
KDD2=kdd.from.raw.data(raw.data=rain.site.6008b,keyday.threshold=25,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,
   filter=list(list(apply.over="range",column=6,value=TRUE)))
summary(KDD2)
}
\keyword{misc}

