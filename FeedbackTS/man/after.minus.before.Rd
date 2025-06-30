\name{after.minus.before}
\alias{after.minus.before}
\title{
	Computation of after-before differences around key days
}
\description{
	Computation of after-before differences around key days using using data \eqn{K} days before and after each key day.
}
\usage{
	after.minus.before(data, operator)
}

\arguments{
  \item{data}{
	either a \link{KDD object}, a \link{KDD.yearly.average object}, or a matrix with an odd number of rows corresponding built as the \code{before.after} slot of a \link{KDD object} or a \link{KDD.yearly.average object}. 
}
  \item{operator}{
	a character string specifying the transformation of the raw values, must be one of \code{"dmv"}, \code{"dmpiv"} or \code{"dmgiv"}.
}
}
\details{
	If \code{operator = "dmv"} (difference of mean values), the raw values \eqn{y_{i-K},\ldots,y_{i+K}} of the time series are used to compute the difference: 
	\deqn{
		D_i=\left(\frac{1}{K}\sum_{k=1}^K y_{i+k}\right) - \left(\frac{1}{K}\sum_{k=1}^K  y_{i-k}\right)=\frac{1}{K}\sum_{k=1}^K (y_{i+k}-y_{i-k}),
	}
	where \eqn{i} is the date of the key day, \eqn{K} is the number of days considered after and before the key day (specified when \code{data} is provided).
	
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
}
\value{
A numeric vector providing for each key day the value of the after-before difference.
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\author{
Samuel Soubeyrand \email{Samuel.Soubeyrand@avignon.inra.fr}, Cindy E. Morris, E. Keith Bigg.
}

\seealso{
\link{KDD}, \link{KDD.yearly.average}, \link{kdd.from.raw.data}, \link{rain.site.6008}
}
\examples{
#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build KDD objects from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 25
KDD=kdd.from.raw.data(raw.data=rain.site.6008,keyday.threshold=25,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,filter=NULL)

#### compute and plot after-before differences of KDD 
par(mfrow=c(2,2), mar=c(5.1,4.1,4.1,4.1))

## using option dmpiv (difference of means of positive indicator values)
amb1=after.minus.before(KDD,"dmpiv")
plot(KDD["day"],amb1,type="l",xlab="Day",ylab="After-Before")
abline(h=0,lty="dashed",col="grey")
plot(KDD["day"],cumsum(amb1),type="l",xlab="Day",ylab="Cumul After-Before")
abline(h=0,lty="dashed",col="grey")

## using option dmv (difference of means of values)
amb2=after.minus.before(KDD,"dmv")
plot(KDD["day"],amb2,type="l",xlab="Day",ylab="After-Before")
abline(h=0,lty="dashed",col="grey")
plot(KDD["day"],cumsum(amb2),type="l",xlab="Day",ylab="Cumul After-Before")
abline(h=0,lty="dashed",col="grey")
}
\keyword{ misc }

