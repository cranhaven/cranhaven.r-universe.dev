\name{copper}
\alias{copper}
\non_function{}
\title{Copper data}
\usage{data(copper)}
\description{
The \code{copper} data frame has 442 sets
of observations from a simulation based on a stockpile
of mined material in the former Soviet Union. Boreholes
have been drilled into the dump. The drill core
is cut every 5 metres and assayed for copper and cobalt
content in percentage by weight.
}
\format{
  This data frame contains the following columns:
  \describe{
\item{sample.num}{sample number.}
\item{id}{sample identification number.}
\item{zone}{zone code.}
\item{xcoord}{x co-ordinate.}
\item{ycoord}{y co-ordinate.}    
\item{zcoord}{z co-ordinate.}    
\item{grade}{grade measurement.}    
\item{core.length}{percentage of copper.}
  }
}
\source{Clark, I. and Harper, W.V. (2000). 
\emph{Practical Geostatistics 2000}.
Columbus, Ohio: Ecosse North America Llc.
}
\examples{
library(SemiPar)
data(copper)
pairs(copper[,4:7])
}
\keyword{datasets}
