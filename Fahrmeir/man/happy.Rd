\name{happy}
\alias{happy}
\docType{data}
%# Example 3.12 page 107.
\title{ Reported Happiness  }
\description{
    Relationship between sex, years in school, and reported happiness.
}
\usage{data(happy)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{Rep.happiness}{an ordered factor with levels \code{Not to happy} < \
                 code{Pretty happy} <  \code{Very happy}}
    \item{School}{a factor with levels \code{<12} \code{>16} \code{12} 
                       \code{13-16}}
    \item{Sex}{a factor with levels \code{Females} \code{Males}}
    \item{n}{number of persons in group}
  }
}
\source{
     Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(happy)
table(happy)
}
\keyword{datasets}
