\name{cars}
\alias{cars}
\docType{data}
\title{real data set in symbolic form - selected car models described by a set of symbolic variables}
\description{symbolic data set: 30 observations on 12 symbolic variables - 9 interval-valued and 3 multinominal variables, third dimension represents the begining and the end of intervals for interval-valued variable's implementation or a set of categories for multinominal variable's implementation
}
\format{symbolic data table  (see (\code{link{symbolic.object}})}
\source{the original data on 30 selected car models and their prices, chasis and engine types were collected from the websites of authorized car dealers. Then the data were converted (aggregated) to symbolic format (second order symbolic objects). 
Each symbolic object - e.g. "Seat Leon”, "Citroen C4" - represents all chasis, engine types and price range of this kind of car model available on the Polish market in 2010. For example the price range [54,900; 96,190] PLN, hatchback and saloon body style, petrol and diesel engine, acceleration 0-100 kph range [10.00; 11.90] seconds are, in general, the characteristics of "Toyota Corolla".
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#sdt<-cars
#r<- HINoV.SDA(sdt, u=5, distance="U_3")
#print(r$stopri)
#plot(r$stopri[,2], xlab="Variable number", ylab="topri",
#xaxt="n", type="b")
#axis(1,at=c(1:max(r$stopri[,1])),labels=r$stopri[,1])
}
\keyword{datasets}
