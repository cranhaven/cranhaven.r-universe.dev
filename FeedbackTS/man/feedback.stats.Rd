\name{feedback.stats}
\alias{feedback.stats}
\title{
Computation of feedback and change-in-feedback statistics
}
\description{
Computation of temporal averages of after-before differences around key days calculated from a time series and computation of the difference in the temporal averages around a turning point in time.
}
\usage{
feedback.stats(object, operator, turning.year = NULL,
    trend.correction = list( apply = FALSE , object2 = NULL ))
}

\arguments{
  \item{object}{
either a \link{KDD object} or a \link{KDD.yearly.average object}.
}
  \item{operator}{
a character string specifying the transformation of the raw values, must be one of \code{"dmv"}, \code{"dmpiv"} or \code{"dmgiv"}.
}
  \item{turning.year}{
an optional numeric vector of years used to specify turning points in the data series.
}
  \item{trend.correction}{
an optional list of two items: the \code{apply} item, which is a logical
  indicating whether the trend correction should be applied or not
  (default to \code{FALSE}); the \code{object2} item, which is a
  \link{KDD object} typically built like \code{object} except that the
  keyday threshold is fixed at zero (see example section below).
}
}
\details{
The function computes the following temporal averages of after-before differences around key days calculated from a time series:
\deqn{\bar D(I)=\frac{1}{n(I)}\sum_{i\in I} D_i}
where \eqn{I} is a set of key days, \eqn{n(I)} is the number of key days in \eqn{I}, and \eqn{D_i} is an after-before difference computed for each key day \eqn{i} (see below and in \link{after.minus.before} function). 

If \code{operator = "dmv"} (difference of mean values), the raw values \eqn{y_{i-K},\ldots,y_{i+K}} of the time series are used to compute the difference: 
	\deqn{
		D_i=\left(\frac{1}{K}\sum_{k=1}^K y_{i+k}\right) - \left(\frac{1}{K}\sum_{k=1}^K  y_{i-k}\right)=\frac{1}{K}\sum_{k=1}^K (y_{i+k}-y_{i-k}),
	}
	where \eqn{i} is the date of the key day, \eqn{K} is the number of days considered around the key day (specified when \code{data} is provided).
	
If \code{operator = "dmpiv"} (difference of means of positive indicator values), the raw values \eqn{y_{i-K},\ldots,y_{i+K}} are used to compute the difference: 
	\deqn{
		D_i=\left(\frac{1}{K}\sum_{k=1}^K 1(y_{i+k}>0)\right) - \left(\frac{1}{K}\sum_{k=1}^K  1(y_{i-k}>0)\right)=\frac{1}{K}\sum_{k=1}^K \{1(y_{i+k}>0)-1(y_{i-k}>0)\},
	}
	where \eqn{1(\cdot)} is the indicator function.

If \code{operator = "dmgiv"} (difference of means of greater indicator values), the raw values \eqn{y_{i-K},\ldots,y_{i+K}} are used to compute the difference: 
	\deqn{
		D_i=\left(\frac{1}{K}\sum_{k=1}^K
	1(y_{i+k}>y_{i-k})\right) - \left(\frac{1}{K}\sum_{k=1}^K
	1(y_{i-k}>y_{i+k})\right).
      }
	
If \code{turning.year = NULL}, the function computes \eqn{\bar D(I)}
where \eqn{I} is the set of all key days in the whole time series.

If \code{turning.year} is a numeric vector, for each value \eqn{t} in
\code{turning.year} the function computes \eqn{\bar D(I)} with \eqn{I}
equal to the set of key days in the whole time series, in the time
series before \eqn{t} and in the time series after \eqn{t}. The function
also computes, for each value \eqn{t}, the difference between the
temporal averages of after-before differences after \eqn{t} and before
\eqn{t}.

If \code{trend.correction$apply = TRUE}, a trend correction is applied
to take into account, for example, seasonal effect in the time series
(see Morris et al., 2016). 

}
\value{
If \code{turning.year = NULL}, a numeric equal to the temporal average of after-before differences around key days calculated from the whole time series.

If \code{turning.year} is a numeric vector, a numeric vector providing: 

\describe{
	\item{--}{the temporal average of after-before differences around key days calculated from the whole time series;}

	\item{--}{for each value \eqn{t} in \code{turning.year},
		\describe{
			\item{--}{the temporal average \eqn{\bar D_{<t}} of after-before differences around key days calculated from the time series right-truncated at time \eqn{t};}
			\item{--}{the temporal average \eqn{\bar D_{\ge t}}  of after-before differences around key days calculated from the time series left-truncated at time \eqn{t};}
			\item{--}{the difference \eqn{\bar D_{\ge t}-\bar D_{<t}}.}
		}
	}
}
}
\references{
Morris, C.E., Soubeyrand, S.; Bigg, E.K., Creamean, J.M., Sands,
		D.C. (2016). Rainfall feedback maps: a tool to depict
		the geography of precipitation's sensitivity to
		aerosols. INRA Research Report.
  
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
	Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}

\seealso{
	\link{KDD}, \link{KDD.yearly.average}, \link{kdd.from.raw.data}, \link{after.minus.before}, \link{rain.site.6008}, \link{rain.feedback.stats}
}
\examples{
#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build KDD objects from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008, keyday.threshold=25, nb.days=20,
   col.series=5, col.date=c(2,3,4), na.rm=TRUE, filter=NULL)
	
#### main feedback statistic
feedback.stats(KDD, "dmv")

#### main and auxiliary feedback statistics
feedback.stats(KDD, "dmv", turning.year=c(1960,1980))

#### apply a trend correction
## define the KDD object used for trend correction (it is defined as
## KDD above except that the threshold value is equal to 0)
KDD2=kdd.from.raw.data(raw.data=rain.site.6008, keyday.threshold=0, nb.days=20,
   col.series=5, col.date=c(2,3,4), na.rm=TRUE, filter=NULL)
## compute the statistic
feedback.stats(KDD, "dmv", trend.correction=list(apply=TRUE, object2=KDD2))
}
\keyword{ misc }