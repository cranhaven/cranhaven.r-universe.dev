\name{splett2}
\alias{splett2}
\docType{data}
\title{
Energy data
}
\description{
Energy absorbed by four machines for Charpy V-notch testing.
}
\usage{data(splett2)}
\format{
  A data frame with 99 observations on the following 2 variables.
  \describe{
    \item{\code{Energy}}{Energy absorbed by machine (in foot-pounds)}
    \item{\code{Machine}}{Machine type (1 = Tinius1, 2 = Tinius2, 3 = Satec, 4 = Tokyo)}
  }
}

\source{
Dataplot webpage of the National Institute of Standards and Technology (NIST), \cr USA (https://www.itl.nist.gov/div898/software/dataplot/data/SPLETT2.DAT).
}

\examples{
data(splett2)
head(splett2)
}
\keyword{datasets}
