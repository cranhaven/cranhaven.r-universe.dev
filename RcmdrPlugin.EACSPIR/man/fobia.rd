\name{fobia}
\alias{fobia}
\docType{data}
\title{
Eficacia tratamiento para la fobia
}
\description{
Datos simulados sobre la eficacia de un tratamiento psicologico en pacientes fobicos.
}
\usage{fobia}
\format{
  Una base de datos con 160 observaciones y  14 variables.
  \describe{
    \item{\code{identif}}{Identificador numerico}
    \item{\code{sexo}}{Sexo del paciente}
    \item{\code{subtipo}}{Subtipo de fobia}
    \item{\code{edad}}{Edad del paciente}
    \item{\code{educa}}{Nivel educativo}
    \item{\code{detecci}}{Fase en la que se le detecta la enfermedad}
    \item{\code{grupo}}{Grupo al que se asigna al paciente}
    \item{\code{pensini}}{Pensamientos fobicos al inicio de la investigacion}
    \item{\code{pulsoini}}{Frecuencia cardiaca al inicio de la investigacion}
    \item{\code{electini}}{Actividad electrodermica al inicio de la investigacion}
    \item{\code{ansiedad}}{Nivel de ansiedad del paciente}
    \item{\code{pensfin}}{Pensamientos fobicos al finalizar la investigacion}
    \item{\code{penseg}}{Medida de seguimiento respecto a los pensamientos fobicos }
    \item{\code{pulsofin}}{Frecuencia cardiaca al finalizar la investigacion}
    \item{\code{electfin}}{Actividad electrodermica al finalizar la investigacion}
  }
}

\examples{
  data(fobia)
  summary(fobia)
}

\keyword{datasets}
