\name{testBarcelona}
\alias{testBarcelona}
\docType{data}
\title{
Datos del test Barcelona
}
\description{
Datos obtenidos mediante una version reducida del test neuropsicologico Barcelona.
}
\usage{testBarcelona}
\format{
  Una base de datos con 499 observaciones y  25 variables.
  \describe{
    \item{\code{consulta}}{Tipo de servicio donde se realiza la evaluacion}
    \item{\code{estado.civil}}{Estado civil de la persona evaluada}
    \item{\code{sexo}}{Sexo de la persona evaluada}
    \item{\code{derivado}}{Tipo de servicio que cursa la derivacion}
    \item{\code{nivel.escolar}}{Nivel de estudios de la persona evaluada}
    \item{\code{ocupacion}}{Ocupacion de la persona evaluada}
    \item{\code{dominancia}}{Descripcion de la lateralidad de la persona evaluada}
    \item{\code{edad}}{Edad de la persona evaluada}
    \item{\code{anos.escolaridad}}{Anos de escolaridad de la persona evaluada}
    \item{\code{lenguaje.espontaneo}}{Subtest del Test Barcelona que sirve para evaluar el lenguaje espontoneo de la persona}
    \item{\code{descripcion.lamina}}{Subtest del Test Barcelona que consiste en describir una lamina}
    \item{\code{fluencia}}{Subtest del Test Barcelona que sirve para evaluar la fluidez del lenguaje de la persona}
    \item{\code{contenido.informativo}}{Subtest del Test Barcelona}
    \item{\code{orientacion.persona}}{Evaluacion de la orientacion respecto a las personas}
    \item{\code{orientacion.lugar}}{Evaluacion de la orientacion respecto al lugar}
    \item{\code{orientacion.tiempo}}{Evaluacion de la orientacion respecto al tiempo}
    \item{\code{digitos.directos}}{Subtest del Test Barcelona que consiste en recordar en el mismo orden una secuencia de digitos}
    \item{\code{digitos.inversos}}{Subtest del Test Barcelona que consiste en recordar en orden inverso una secuencia de digitos}
    \item{\code{series.directas}}{Subtest del Test Barcelona que consiste en recordar en el mismo orden una secuencia de palabras}
    \item{\code{series.directas.tiempo}}{Subtest del Test Barcelona que consiste en recordar en el mismo orden una secuencia de palabras. La prueba tiene un limite de tiempo}
    \item{\code{series.inversas}}{Subtest del Test Barcelona que consiste en recordar en orden inverso una secuencia de palabras}
    \item{\code{series.inversas.tiempo}}{Subtest del Test Barcelona que consiste en recordar en orden inverso una secuencia de palabras. La prueba tiene un limite de tiempo}
    \item{\code{repeticion.logatomos}}{Subtest del Test Barcelona}
    \item{\code{repeticion.palabras}}{Subtest del Test Barcelona que consiste en repetir palabras}
    \item{\code{diagnostico}}{Diagnostico medico realizado}
  }
}
\examples{
  data(testBarcelona)
  summary(testBarcelona)
}
\keyword{datasets}
