\name{ch1.GM03563}
\alias{ch1.GM03563}
\docType{data}
\title{
Fibroblast cell line (GM03563) data
}
\description{
Chromosome 1 of cell line GM03563
}
\usage{data("ch1.GM03563")}
\format{
A single column data frame with 135 observations corresponds to chromosome 1 of cell line GM03563.
\describe{
    \item{\code{log2ratio}}{normalized average of the log base 2 test over reference ratio data}
  }
}
\details{
This data set is extracted from a single experiments on 15 fibroblast cell lines with each array containing over 2000 (mapped) BACs spotted in triplicate discussed in Snijders et al.(2001). Data corresponds to the chromosome 1 of cell line GM03563.
}
\references{
Snijders,A.M. et al. (2001) Assembly of microarrays for genome-wide measurement of DNA copy number. Nature Genetics, 29, 263-26.
}
\examples{
data(ch1.GM03563)
\dontrun{
## CE with four parameter beta distribution ##
obj1 <- CE.Normal.Mean(ch1.GM03563, distyp = 1, parallel =TRUE)
profilePlot(obj1, ch1.GM03563)

## CE with truncated normal distribution ##
obj2 <- CE.Normal.Mean(ch1.GM03563, distyp = 2, parallel =TRUE)
profilePlot(obj2, ch1.GM03563)
}}
\keyword{data}
