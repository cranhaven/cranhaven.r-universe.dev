
\name{simulated_hue}
\docType{data}
\alias{simulated_hue}
\encoding{UTF-8}
\title{Hue color simulated data}
\description{
Simulated hue data set based on papaya's maturation over time considering three
methods of measurement.  
}
\usage{data(simulated_hue)}
\format{
  A simulated data frame with 6,000 observations on the mean hue variable.
The format is:
  \tabular{lll}{
\code{Hue} \tab numeric; mean hue of papaya's peel \cr 
\code{Method} \tab a factor with levels labelled from \code{Method 1} to  \code{Method 3}  \cr
\code{Time} \tab integer; time in days from 0 to 19 \cr
\code{Fruit} \tab a factor with 100 levels labelled from \code{1} to \code{100} \cr
\tab where each level is represented by one fruit.
 }
}

\examples{
data(simulated_hue)
summary(simulated_hue)
str(simulated_hue)
}
\details{
A total of 100 fruits were observed over 20 days by three methods to
evaluate the mean hue of fruit's peel. The aim of the agreement study was to
assess how well the methods 2, and 3 agreed with method 1 over time.
}

\seealso{
  \code{\link[lcc]{lcc}}.
}
\keyword{datasets}
