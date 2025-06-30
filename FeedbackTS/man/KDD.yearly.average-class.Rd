\name{KDD.yearly.average-class}
\Rdversion{1.1}
\docType{class}
\alias{KDD.yearly.average-class}
\alias{KDD.yearly.average}
\alias{KDD.yearly.average class}
\alias{KDD.yearly.average object}
\alias{KDD.yearly.average-object}
%\alias{[,KDD.yearly.average,ANY,ANY,ANY-method}
\alias{[,KDD.yearly.average-method}
\alias{show,KDD.yearly.average-method}
\alias{summary,KDD.yearly.average-method}
\alias{names,KDD.yearly.average-method}

\title{Class \code{"KDD.yearly.average"}}
\description{Class KDD.yearly.average (yearly average of a Key Day Dataset) used as argument in \code{FeedbackTS} functions for the analysis of fragmented time directionality and feedback.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("KDD.yearly.average", ...)} and \link{kdd.yearly.average}(...).
}
\section{Slots}{
  \describe{
    \item{\code{before.after}:}{Object of class \code{"matrix"}  with \eqn{2\times K+1} rows 
    	and \eqn{n} columns: Each column gives the yearly average of the vectors of raw values \eqn{(y_{i-K},\ldots,y_{i+K})} of the time series for key days \eqn{i} occurring during a single year (\eqn{K} is the number of days considered after and before the key day, \eqn{n} is the number of years with key days in the data series and depends on \code{keyday.threshold}).}
    \item{\code{year}:}{Object of class \code{"numeric"}, vector of size \eqn{n} providing the years during which the key days occurred.}
    \item{\code{keyday.threshold}:}{Object of class \code{"numeric"} providing the threshold value above which a day is considered as a key day (i.e. if \eqn{y_i\ge } \code{keyday.threshold}, then day \eqn{i} is a key day).}
    \item{\code{yearly.nb.keydays}:}{Object of class \code{"numeric"}, vector of size \eqn{n} providing the number of key days at each year of the slot \code{year}.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "KDD.yearly.average")}: Extract one of the slots.}
    \item{names}{\code{signature(x = "KDD.yearly.average")}: Prints slot names.}
    \item{show}{\code{signature(object = "KDD.yearly.average")}: Prints all slots of the KDD object.}
    \item{summary}{\code{signature(object = "KDD.yearly.average")}: Prints summary characteristics of the KDD object.}
	 }
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}



\seealso{
 \link{KDD.yearly.average}, \link{KDD}, \link{kdd.from.raw.data}, \link{rain.site.6008}
}
\examples{
showClass("KDD.yearly.average")

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
## names of the object
names(KDD2)
slotNames(KDD2)

## show attributes of the object
KDD2["before.after"][,1:5]
KDD2["year"]
KDD2["keyday.threshold"]
}
\keyword{classes}
