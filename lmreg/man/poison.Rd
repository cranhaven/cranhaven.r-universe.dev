\name{poison}
\alias{poison}
\docType{data}
\title{
Survival times of poisoned animals
}
\description{
Survival times of animals exposed to poison and treatment (Box and Cox,
1964).
}
\usage{data(poison)}
\format{
  A data frame with 48 observations on the following 3 variables.
  \describe{
    \item{\code{Survtime}}{Survival time (in 10 hour units)}
    \item{\code{Treatment}}{Treatment type: 1 = treatment A, 2 = treatment B, 3 = treatment C, 4 = treatment D}
    \item{\code{Poison}}{Poison type: 1 = Poison I, 2 = Poison II, 3 = Poison III}
  }
}

\source{
Box, G.E.P. and Cox, D.R. (1964) An analysis of transformations. J. Roy. Statist. Soc. Ser. B, 26, pp.211-252.
}

\examples{
data(poison)
head(poison) 
}
\keyword{datasets}
