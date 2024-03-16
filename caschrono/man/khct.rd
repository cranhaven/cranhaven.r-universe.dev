\name{khct}
\alias{khct}
\docType{data}
\title{Monthly electricity comsumption for the period 1970-1984}
\description{
Monthly electricity consumption, heating degree days and cooling degree days in some region for the period 1970-1984.}
\value{\code{csdl} is a multivariate \code{ts} object which contains:
  \item{kwh}{electricity consumption in kilo-watt-hours}
  \item{htdd}{heating degree days, in Fahrenheit degrees}
  \item{cldd}{cooling degree days, in Fahrenheit degrees}
}
\details{
\code{htdd} (heating degree days) is minus the sum over the month of the daily difference between the average daily temperature, if it is lower than 65 F. degrees, and 65 F. degrees, the equilibrium temperature above which a house does not need to be heated. \cr
\code{cldd} (cooling degree days)  is the sum over the month of the daily difference between the average daily temperature, if it is greater than  65 F. degrees, and 65, the equilibrium temperature above which air conditioning is switched on. \cr
The dataset is from the book  by Pankratz (1991).
}
\usage{data(csdl)}
\source{Pankratz A., Forecasting with dynamic regression models, 1991, Wiley.}
\examples{
data(khct)
# The executed code is : 
## Not run: 
khct = read.csv2(file= system.file("/import/conselec.csv",package="caschrono"))
attach(khct)
khc = ts(cbind(kwh, htdd,cldd), frequency = 12, start=c(1970,1))
kwh = khc[,1]
htdd = khc[,2]
cldd = khc[,3]
temps = time(kwh)
## End(Not run)
}
\keyword{datasets}
