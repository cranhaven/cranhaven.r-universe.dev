\name{champa.ts}
\alias{champa.ts}
\docType{data}
\title{Monthly shipments of bottles of champagne for the period 2001-2010}
\description{
The file "/import/champagne_2001.txt" contains monthly total shipping of 75 cl bottles of Champagne wine, in the world. 2001-2010. 
}
\format{The series first is imported as a vector of numeric type with function \code{scan}
 and then transformed into a \code{ts} object. Is is then converted in 1,000 of bottles.}
 
\source{SRISE - DRAAF Champagne-Ardenne}
\examples{
data("champa.ts")
# The executed code is : 
## Not run: 
aa <- scan(system.file("/import/champagne_2001.txt", package = "caschrono"))
champa.ts <- ts(aa/1000, start = c(2001,1), frequency = 12)
## End(Not run)
}
\keyword{datasets}
