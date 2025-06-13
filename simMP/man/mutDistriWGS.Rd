\name{mutDistriWGS}
\alias{mutDistriWGS}
\docType{data}
\title{
Distribution of single base substitutions
}
\description{
Distribution of single base substitutions over all currently available WGS genomes in ICGC data realse 23.
}
\usage{data("mutDistriWGS")}
\format{
  A data frame with 3543 observations on the following variable.
  \describe{
    \item{\code{X0}}{a numeric vector}
  }
}
\source{
  Zhou, Nan, et al. "Pan-cancer scale landscape of simple somatic mutations." bioRxiv (2017): 112367.
}
\examples{
data(mutDistriWGS)
head(mutDistriWGS)

\dontrun{
plot(1:nrow(mutDistriWGS), sort(c(t(mutDistriWGS)), decreasing = TRUE))
}
}
\keyword{datasets}
