\name{Regensburg}
\alias{Regensburg}
\docType{data}
\concept{ordinal response}
\concept{proportional odds}
\concept{cumulative logistic model}
\title{ Job Expectation  }
\description{
  A sample of psychology students was asked if they expected to
   find adecuate employment after graduation.
}
\usage{data(Regensburg)}
\format{
  A data frame with 30 observations on the following 4 variables.
  \describe{
    \item{y}{response categories}
    \item{n}{number of students with this response in group}
    \item{age}{age in years}
    \item{lage}{natural log of age}
  }
}
\details{
  In a study on the perspectives of students, psychology students
  at the university of Regensburg have been asked if they expect to 
  find an adequate employment after getting their degree. 
  The response categories where ordered with respect to their expectation.
  Categories where "don't expect adequate employment" - 1, 
  "not sure" - 2, "immediately after the degree" - 3.
}
\source{
     Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(Regensburg)
summary(Regensburg)
# Example 3.5 page 83 in book:
library(MASS)
Regensburg$y <- ordered(Regensburg$y)
Regensburg.polr <- polr(y~lage, data=Regensburg, weights = n) 
summary(Regensburg.polr)
class(Regensburg.polr)
}
\keyword{datasets}
