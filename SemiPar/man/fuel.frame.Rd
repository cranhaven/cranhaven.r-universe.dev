\name{fuel.frame}
\alias{fuel.frame}
\non_function{}
\title{Automobile data from consumer reports}
\usage{data(fuel.frame)}
\description{
The \code{fuel.frame} data frame contains data on 
5 variables (columns) for 117 cars (rows).
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{car.name}{character variable giving the name (make) of the car}
    \item{Weight}{the weight of the car in pounds.}
    \item{Disp.}{the engine displacement in litres.}
    \item{Mileage}{gas mileage in miles/gallon.}    
    \item{Fuel}{a derived variable concerning fuel efficiency.}
    \item{Type}{a factor giving the general type of car. 
                The levels are: Small ,Sporty , Compact , 
                Medium , Large , Van.}
  }
}
\source{
Consumer Reports, April, 1990, pp. 235-288.
}
\references{
    Chambers, J.M. and Hastie, T.J. (eds.) (1992)\cr
     \emph{Statistical Models in S}.\cr
     Wadsworth and Brooks, Pacific Grove, California.
}
\examples{
library(SemiPar)
data(fuel.frame)
pairs(fuel.frame)
par(mfrow=c(2,2))
fuel.fit <- lm(Fuel ~ Weight + Disp.,fuel.frame)
plot(fuel.fit,ask=FALSE)
par(mfrow=c(1,1))
}
\keyword{datasets}
