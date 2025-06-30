\name{KT.output-class}
\Rdversion{1.1}
\docType{class}
\alias{KT.output-class}
\alias{KT.output}
\alias{KT.output class}
\alias{KT.output-object}
\alias{KT.output object}
\alias{[<-,KT.output,ANY,ANY,ANY-method}
%\alias{[<-,KT.output-method}
%\alias{[,KT.output,ANY,ANY,ANY-method}
\alias{[,KT.output-method}
\alias{show,KT.output-method}
\alias{summary,KT.output-method}
\alias{names,KT.output-method}
\alias{plot,KT.output,ANY-method}

\title{Class \code{"KT.output"}}
\description{
Output of the \link{krige.test} function.
}
\section{Objects from the Class}{
Objects are created by calls of the \link{krige.test} function.
}
\section{Slots}{
  \describe{
    \item{\code{krige.output}:}{Object of class \code{"list"}, output of the \link{krige} function.}
    \item{\code{subregion}:}{Object of class \code{"list"}, two vectors \code{x} and \code{y} containing the latitudes and the longitudes, respectively, of the vertices of a polygon. The polygon defines a subregion where one supposes variation in the predicted index.}
    \item{\code{averageKrigingPrediction.rand}:}{Object of class \code{"numeric"} specifying the averages of the kriging predictions in \code{subregion} obtained with randomized data (here, a randomization is a random translation on a torus). }
    \item{\code{averageKrigingPrediction.obs}:}{Object of class \code{"numeric"} specifying the average of the kriging prediction in \code{subregion} obtained with observed data. }
    \item{\code{alternative}:}{Object of class \code{"character"}, "greater" or "less".}
    \item{\code{p.value}:}{Object of class \code{"numeric"}, p-value of the test.}
  }
}
\section{Methods}{
  \describe{
    \item{[<-}{\code{signature(x = "KT.output", i = "ANY", j = "ANY", value = "ANY")}}
    %\item{[<-}{\code{signature(x = "KT.output")}: Change one of the slots.}
    %\item{[}{\code{signature(x = "KT.output", i = "ANY", j = "ANY", drop = "ANY")}}
    \item{[}{\code{signature(x = "KT.output")}: Extract one of the slots.}
    \item{names}{\code{signature(x = "KT.output")}: Prints slot names.}
    \item{show}{\code{signature(object = "KT.output")}: Prints all slots of
  the KDD object.}
    \item{summary}{\code{signature(object = "KT.output")}: Prints summary
  characteristics of the KDD object.}
    \item{plot}{\code{signature(x = "KT.output"), i="ANY"}: Graphically displays contents of the object.}
	 }
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
	Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}
\examples{
showClass("KT.output")

#### load data of feedback and change-in-feedback indices in 88 sites across Australia
data(rain.feedback.stats)

#### spatial coordinates of the 88 sites
coord=rain.feedback.stats[,3:4]

#### map of feedback index computed from the whole data series
stat1=rain.feedback.stats[["Feedback.whole.period"]]

#### variogram analysis and kriging of feedback index
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
plot(kt1,digits=list(predict=3,pvalue=3),breaks=12) }
}
\keyword{classes}
