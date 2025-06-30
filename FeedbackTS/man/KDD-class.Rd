\name{KDD-class}
\Rdversion{1.3}
\docType{class}
\alias{KDD-class}
\alias{KDD}
\alias{KDD object}
\alias{KDD-object}
\alias{KDD class}
\alias{[<-,KDD,ANY,ANY,ANY-method}
%\alias{[<-,KDD-method}
%\alias{[,KDD,ANY,ANY,ANY-method}
\alias{[,KDD-method}
\alias{names,KDD-method}
\alias{show,KDD-method}
\alias{summary,KDD-method}

\title{Class \code{"KDD"}}
\description{
Class KDD (Key Day Dataset) used as argument in \code{FeedbackTS} functions for the analysis of fragmented time directionality and feedback.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("KDD", ...)}, \link{kdd.from.raw.data}(...) and \link{kdd}(...). 
}
\section{Slots}{
  \describe{
    \item{\code{before.after}:}{Object of class \code{"matrix"} with \eqn{2\times K+1} rows 
    	and \eqn{n} columns: Each column gives the raw values \eqn{y_{i-K},\ldots,y_{i+K}} of the time series, where \eqn{i} is the date of the key day, \eqn{K} is the number of days considered after and before the key day, \eqn{n} is the number of key days in the data series (depends on \code{keyday.threshold}).}
    \item{\code{date}:}{Object of class \code{"character"}, vector of size \eqn{n} providing the dates of the key days in format yyyy.mm.dd.}
    \item{\code{year}:}{Object of class \code{"numeric"}, vector of size \eqn{n} providing the years during which the key days occurred.}
    \item{\code{day}:}{Object of class \code{"numeric"}, vector of size \eqn{n} providing for each key day the number of days since the beginning of the data series.}
    \item{\code{keyday.threshold}:}{Object of class \code{"numeric"} providing the threshold value above which a day is considered as a key day (i.e. if \eqn{y_i\ge } \code{keyday.threshold}, then day \eqn{i} is a key day).}
  }
}
\section{Methods}{
  \describe{
 %   \item{[<-}{\code{signature(x = "KDD")}: Change one of the slots.}
    \item{[<-}{\code{signature(x = "KDD", i = "ANY", j = "ANY", value =
	"ANY")}: Change one of the slots.}
    \item{[}{\code{signature(x = "KDD")}: Extract one of the slots.}
%    \item{[}{\code{signature(x = "KDD", i = "ANY", j = "ANY", drop =
%	"ANY")}: Extract one of the slots.}
    \item{names}{\code{signature(x = "KDD")}: Prints slot names. }
    \item{show}{\code{signature(object = "KDD")}: Prints all slots of
  the KDD object.}
    \item{summary}{\code{signature(object = "KDD")}: Prints summary
  characteristics of the KDD object.}
	 }
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}


\seealso{
 \link{kdd}, \link{kdd.from.raw.data}, \link{KDD.yearly.average}, \link{rain.site.6008}
}
\examples{
showClass("KDD")

#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build a KDD object from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008,keyday.threshold=25,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,filter=NULL)

## summary of the object
summary(KDD)
## names of the object
names(KDD)
slotNames(KDD)

## show attributes of the object
KDD["before.after"][,1:5]
KDD["date"]
KDD["keyday.threshold"]

## change keyday threshold
KDD["keyday.threshold"]=50
}
\keyword{classes}
