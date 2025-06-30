\name{turtles}
\alias{turtles}
\docType{data}
\title{
Turtle Humeri
}
\description{
A dataset containing a sample of 40 turtle humeri used in Dickson and Pierce (2019).
}
\usage{data("turtles")}
\format{
  A data frame with 40 observations on the following 4 variables.
  \describe{
    \item{\code{x}}{the first axis of shape variation as determiend by a between-groups principal components analysis}
    \item{\code{y}}{the second axis of shape variation as determiend by a between-groups principal components analysis}
    \item{\code{Group}}{the locomotor ecologies of the turtles}
    \item{\code{Ecology}}{the three ecological groups as determined by a Procrustes ANOVA; "M" (marine), "S" (semiaquatic), and "T" (terrestrial)}
  }
}
\source{
Dickson, B.V. and Pierce, S.E. (2019), Functional performance of turtle humerus shape across an ecological adaptive landscape. Evolution, 73: 1265-1277. \doi{10.1111/evo.13747}
}
\keyword{datasets}
