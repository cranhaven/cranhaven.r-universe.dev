\name{ohio}
\alias{ohio}
\docType{data}
\concept{repeated measurement}
\concept{short time series}
\title{ Air Pollution and Health  }
\description{
  Air Pollution and Health, annual data on children 7 to ten years old in Ohio.
}
\usage{data(ohio)}
\format{
  A data frame with 32 observations on the following 6 variables.
  \describe{
    \item{a7}{Presence (1) or absence (0) of respiratory infection}
    \item{a8}{Presence (1) or absence (0) of respiratory infection}
    \item{a9}{Presence (1) or absence (0) of respiratory infection}
    \item{a10}{Presence (1) or absence (0) of respiratory infection}
    \item{mother.smoke}{a factor with levels \code{no} \code{yes}}
    \item{n}{number of children}
  }
}
\details{
  Within the harvard Study of Air Pollution and Health, 537 children 
  were examined annually from age 7 to 10, on the presence or absence
  of respiratory infection. So there are four repeated measurements
  on each child, or "short time series".  The only available covariate
  is mothers smoking status at start of study. 
}
\source{
    Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(ohio)
summary(ohio)
}
\keyword{datasets}
