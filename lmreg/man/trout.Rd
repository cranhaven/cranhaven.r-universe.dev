\name{trout}
\alias{trout}
\docType{data}
\title{
Brown trout hemoglobin data 
}
\description{
The measured hemoglobin content in the blood of brown trout that were randomly allocated to four troughs, where different concentrations of sulfamerazine in food were administered 35 days prior to measurement (Gutsell, 1951).
}
\usage{data(trout)}
\format{
  A data frame with 40 observations on the following 2 variables.
  \describe{
    \item{\code{Sulfamerazine}}{Concentrations of sulfamerazine (in grams per 100 pounds of fish)}
    \item{\code{Hemoglobin}}{Hemoglobin content (in grams per 100 ml of blood)}
  }
}

\source{
Gutsell, James S. (1951) The effect of sulfamerazine on the erythrocyte and hemoglobin content of trout blood, Biometrics 7(2), pp.171-179.
}

\examples{
data(trout)
head(trout)
}
\keyword{datasets}
