\name{PAS.binmod.beef}
\alias{PAS.binmod.beef}
\alias{data}
\alias{x}
\alias{y}
\alias{map}
\docType{data}
\title{
beef data
}
\description{
The data are provide for demonstration purpose only.
}

\value{
  \describe{
    \item{\code{x}}{genotypic data. int [1:836, 1:300] 0 0 0 -1 -1 -1 -1 0 0 0 ...}
    \item{\code{y}}{phenotypic data. int [1:836, 1] 768 157 508 614 590 777 505 243 509 351 ...}
    \item{\code{map}}{physical map: 'data.frame':	300 obs. of  2 variables:\cr
                     $ chr: num  1 1 1 1 ...\cr
                     $ pos: int  113641 244698 369418 447277 ..\cr}
  }
}
\examples{
#load PAS library
library (PAS)
#load example data
data(beef)
str(x)
str(y)
str(map)
}
\keyword{datasets}
