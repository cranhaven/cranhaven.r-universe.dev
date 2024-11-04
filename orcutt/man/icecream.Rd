\name{icecream}
\alias{icecream}
\docType{data}
\title{
Ice Cream Consumption
}
\description{
four weekly observations from 1951-03-18 to 1953-07-11 in United States (30 observations)
}
\usage{data("icecream")}
\format{
  A data frame with 30 observations on the following 4 variables.
  
\describe{ 
\item{\code{price}}{price of ice cream (per pint);}
\item{\code{cons}}{consumption of ice cream per head (in pints);} 
\item{\code{income}}{average family income per week (in US Dollars);}
\item{\code{temp}}{average temperature (in Fahrenheit);}
  }
}

\source{
Hildreth, C. and J. Lu (1960) Demand relations with autocorrelated disturbances, Technical Bulletin No 2765, Michigan State University.
}
\references{
Verbeek M. (2004) \emph{A guide to modern econometrics}, John Wiley & Sons Ltd
}
\examples{
data(icecream)
summary(icecream)
}
\keyword{datasets}
\keyword{orcutt}