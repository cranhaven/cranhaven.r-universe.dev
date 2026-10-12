\name{tonsil}
\alias{tonsil}
\docType{data}
\title{ Data Set of Tonsil Size in Children  }
\description{
   Children have been classified according to their relative tonsil size and 
   wheater or not they are carriers of Streptococcus pyogenes.
}
\usage{data(tonsil)}
\format{
  A data frame with 6 observations on the following 3 variables.
  \describe{
    \item{Streptococcus.p}{a factor with levels \code{carriers} \code{noncarriers}}
    \item{Size}{numeric, 1, 2 or 3, tonsil size}
    \item{n}{number of children in group}
  }
}
\details{
   It may be assumed that tonsil size always starts in the normal state 
   "present but not enlarged" (category 1). If the tonsils grow abnormally, 
   they may become "enlarged" (category 2), if the process does not stop, 
   they may become "greatly enlarged" (category 3).
}
\source{
     Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(tonsil)
summary(tonsil)
}
\keyword{datasets}
