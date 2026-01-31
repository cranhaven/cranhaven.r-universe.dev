\name{memoriaVisual}
\alias{memoriaVisual}
\docType{data}
\title{
Memoria visual en una muestra de pacientes fobicos
}
\description{
En un experimento se desea contrastar algunas caracteristicas sobre la memoria visual y organizacion de los procesos al registrar y recuperar informacion en personas con algun trastorno fobico. En una muestra de 30 sujetos con trastorno fobico se ha registrado la variable numero de imagenes recordadas, medida en dos momentos distintos: antes de un entrenamiento, el cual consiste en el adiestramiento en la organizacion de las imagenes, y despues del entrenamiento. El objetivo es determinar si existen diferencias entre los dos momentos de medida. 
}
\usage{memoriaVisual}
\format{
  Una base de datos con 30 observaciones y  3 variables.
  \describe{
    \item{\code{antes}}{Numero de imagenes recordadas antes de recibir el entrenamiento}
    \item{\code{despues}}{Numero de imagenes recordadas despues de recibir el entrenamiento}
    \item{\code{diferencia}}{Diferencias de los momentos antes y despues en la muestra}  
  }
}
\examples{
  data(memoriaVisual)
  summary(memoriaVisual)
}

\keyword{datasets}
