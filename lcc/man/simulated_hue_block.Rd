\name{simulated_hue_block}
\docType{data}
\alias{simulated_hue_block}
\encoding{UTF-8}
\title{Hue color simulated data in a randomized block design}
\description{
Simulated hue data set based on papaya's maturation over time considering four
methods of measurement in a randomized block design.  
}
\usage{data(simulated_hue_block)}
\format{
   A simulated data frame with 24,000 observations on the mean hue variable.
The format is:
  \tabular{lll}{
    \code{Hue} \tab numeric; mean hue of papaya's peel \cr
    \code{Block} \tab factor with levels labelled from \code{1} to \code{3} \cr
    \code{Method} \tab a factor with levels labelled from \code{Method 1},
    to \code{Method 4}  \cr
    \code{Time} \tab integer; time in days from 0 to 19 \cr
    \code{Fruit} \tab a factor with 300 levels labelled from \code{1} to \code{300} \cr
\tab where each level is represented by one fruit.
 }
}

\examples{
data(simulated_hue_block)
summary(simulated_hue_block)
str(simulated_hue_block)
}

\details{
A total of 100 fruits by block were observed over 20 days by four methods to
evaluate the mean hue of fruit's peel. We considered three blocks in
this simulation. The aim of the agreement study was to
assess how well the methods 2, 3, and 4 agreed with method 1 over time.
}

\seealso{
  \code{\link[lcc]{lcc}}.
}
\keyword{datasets}
