\name{headneck}
\alias{headneck}
\docType{data}
%# Example 9.4 page 341.
\title{ Head and Neck Cancer data   }
\description{
    Data from a head and neck cancer study where time was discretized by 
    one-month intervals.  
}
\usage{data(headneck)}
\format{
  A data frame with 47 observations on the following 4 variables.
  \describe{
    \item{month}{a numeric vector}
    \item{atrisk}{a numeric vector, number at risk}
    \item{deaths}{a numeric vector}
    \item{withdrawals}{a numeric vector}
  }
}
\source{
     Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(headneck) 
summary(headneck)
with(headneck, {plot(month, atrisk, type="s");
                lines(month, deaths, type="s", col="red");
                lines(month, withdrawals, type="S", col="green")})
}
\keyword{datasets}
