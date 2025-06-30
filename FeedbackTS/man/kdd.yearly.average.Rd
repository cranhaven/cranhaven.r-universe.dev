\name{kdd.yearly.average}
\alias{kdd.yearly.average}

\title{
Build a KDD.yearly.average object from a KDD object
}
\description{
Build a KDD.yearly.average (yearly average of a Key Day Dataset) by averaging on a yearly basis a KDD object
}
\usage{
kdd.yearly.average(object)
}

\arguments{
  \item{object}{
an object from the \link{KDD class}.
}
}
\value{
an object from the \link{KDD.yearly.average class}.
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}
\note{
The \code{before.after} matrix of the \link{KDD object} is averaged on a yearly basis: every group of columns corresponding to a single year is averaged into a single column.
}

\seealso{
 \link{KDD.yearly.average}, \link{KDD}, \link{kdd.from.raw.data}, \link{rain.site.6008}
}
\examples{
#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build a KDD object from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008,keyday.threshold=25,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,filter=NULL)

#### build the yearly average of KDD
KDD2=kdd.yearly.average(KDD)

## summary of the object
summary(KDD2)
}

\keyword{ misc }

