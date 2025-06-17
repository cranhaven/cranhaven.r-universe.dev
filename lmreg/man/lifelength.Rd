\name{lifelength}
\alias{lifelength}
\docType{data}
\title{
Age at death
}
\description{
William Guy's nineteenth century data on the age at death of persons belonging to different professions.
}
\usage{data(lifelength)}
\format{
  A data frame with 690 observations on the following 2 variables.
  \describe{
    \item{\code{Category}}{Code for profession: 1 = historian, 2 = poet, 3 = painter, 4 = musician, 5 = mathematician or astronomer, 6 = chemist or natural philosopher, 7 = naturalist, 8 = engineer, architect or surveyor}
    \item{\code{Lifelength}}{Age (in years) of deceased}
  }
}

\source{
Guy, W. (1859) On the duration of life as affected by the pursuits of literature, science and art. J. Statist. Soc. London, 22. 
}

\examples{
data(lifelength)
head(lifelength)
}
\keyword{datasets}
