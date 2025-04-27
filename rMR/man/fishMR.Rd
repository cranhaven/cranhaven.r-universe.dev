\name{fishMR}
\alias{fishMR}
\docType{data}
\title{
Gnathonemus Respirometry Trial Data
}
\description{
This is a dataset acquired duirng a respirometry trial on a mormyrid of the species Gnathonemus victoriae. It went great. There are several "loops" (open/close the respirometer) to establish routine metabolic rate, as well as an extended "closed" period to capture the "P.crit", the point at which the linear relationship between metabolic rate and ambient dissolved oxygen changed.
}
\usage{data(fishMR)}
\format{
  A data frame with 64239 observations on the following 7 variables.
  \describe{
    \item{\code{Date.time}}{a character vector}
    \item{\code{times}}{a numeric vector}
    \item{\code{Bar.Pressure.hpa}}{a numeric vector}
    \item{\code{Phase}}{a numeric vector}
    \item{\code{temp.C}}{a numeric vector}
    \item{\code{DO.mgL}}{a numeric vector}
  }
  }
\references{
Moulton Tyler L., Chapman Lauren J., Krahe Rudiger. Manuscript in Prep.

}
\examples{
data(fishMR)
str(fishMR)
head(fishMR) 
}
\keyword{datasets}
