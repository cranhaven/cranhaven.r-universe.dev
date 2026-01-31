\name{depresion}
\alias{depresion}
\docType{data}
\title{
Datos simulados sobre depresion
}
\description{
Datos simulados sobre depresion en tres poblaciones distintas.
}
\usage{depresion}
\format{
  Una base de datos con 21 observaciones y  2 variables.
  \describe{
    \item{\code{depresion}}{Nivel de depresion medido con una prueba psicometrica}
    \item{\code{poblacion}}{Poblacion de referencia de los sujetos de la muestra}
  }
}

\examples{
  data(depresion)
  with(depresion,by(depresion,poblacion,summary))
}

\keyword{datasets}
