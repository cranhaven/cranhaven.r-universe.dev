\name{kdd}
\alias{kdd}
\title{
Build a KDD object from values of slots
}
\description{
Build a KDD (Key Day Dataset) object directly from values of the slots of the KDD class.
}
\usage{
kdd(before.after, date, year, day, keyday.threshold)
}
\arguments{
  \item{before.after}{ 
  	 a matrix with \eqn{2\times K+1} rows 
    	and \eqn{n} columns: Each column gives the raw values \eqn{y_{i-K},\ldots,y_{i+K}} of the time series, where \eqn{i} is the date of the key day, \eqn{K} is the number of days considered after and before the key day, \eqn{n} is the number of key days in the data series (depends on \code{keyday.threshold}).
}
  \item{date}{
	a character vector providing the dates of the key days in format yyyy.mm.dd.
}
  \item{year}{
a numeric vector providing the years during which the key days occurred.
}
  \item{day}{
a numeric vector providing for each key day the number of  days since the beginning of the data series.
}
  \item{keyday.threshold}{
a numeric value providing the threshold value above which a day is considered as a key day (i.e. if \eqn{y_i\ge } \code{keyday.threshold}, then day \eqn{i} is a key day).
}
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
\link{KDD}, \link{kdd.from.raw.data}, \link{rain.site.6008}
}

\examples{
#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build KDD objects from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008,keyday.threshold=25,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,filter=NULL)
	
#### build a new KDD object by modifying one of the slots of KDD
## (e.g. new starting point of the data series)
KDD2=kdd(before.after=KDD@before.after,date=KDD@date,year=KDD@year,
   day=KDD@day-100,keyday.threshold=KDD@keyday.threshold)

#### simplest alternative
KDD2=KDD
KDD2["day"]=KDD2["day"]-100
}
\keyword{misc}

