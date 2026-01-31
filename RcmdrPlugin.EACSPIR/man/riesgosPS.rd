\name{riesgosPS}
\alias{riesgosPS}
\docType{data}
\title{
Valoracion de riesgos psicosociales en la empresa
}
\description{
Evaluacion del riesgo psicosocial en una empresa espanola con el fin de conocer la salud laboral y psicosocial de la empresa. La evaluacion se ha llevado a cabo a partir de la administracion de la Bateria MC-UB, que consta de un checklist a completar por el tecnico de prevencion de riesgos laborales de origen psicosocial, la realizacion de una entrevista a los directivos de la empresa y, finalmente, la administracion de un cuestionario a los trabajadores de la empresa. En la presente base de datos se proporciona la informacion recogida a partir del cuestionario de la Bateria MC-UB. Este cuestionario tiene en cuenta las siguientes areas: organizacion del tiempo de trabajo, comunicacion, formacion y desarrollo, efectos sociales y de grupo, participacion, contenido del trabajo y exigencia de la tarea y del entorno de trabajo.
}
\usage{riesgosPS}
\format{
  Una base de datos con 990 observaciones y  13 variables.
  \describe{
    \item{\code{identificador}}{Identificador de la persona evaluada}
    \item{\code{puesto}}{Categoria profesional de la persona}
    \item{\code{sexo}}{Sexo de la persona encuestada}
    \item{\code{edad}}{Edad en anos cumplidos}
    \item{\code{{antiguedad.anos}}}{Antiguedad en la empresa medida en anos}
    \item{\code{antiguedad.meses}}{Antiguedad en la empresa medida en meses}
    \item{\code{organizacion.trabajo}}{Puntuacion en el factor Organizacion del tiempo de trabajo de la bateria MC-UB}
    \item{\code{comunicacion}}{Puntuacion en el factor Comunicacion de la bateria MC-UB}
    \item{\code{formacion.desarrollo}}{Puntuacion en el factor Formacion y Desarrollo de la bateria MC-UB}
    \item{\code{ef.sociales.grupo}}{Puntuacion en el factor Efectos sociales y de grupo de la bateria MC-UB}
    \item{\code{participacion}}{Puntuacion en el factor Participacion de la bateria MC-UB}
    \item{\code{contenido.trabajo}}{Puntuaciin en el factor Contenido de trabajo de la bateria MC-UB}
    \item{\code{efectos.tarea.entorno}}{Puntuacion en el factor Exigencia de la tarea y del entorno de trabajo de la bateria MC-UB}
  }
}
\examples{
  data(riesgosPS)
  summary(riesgosPS)
}

\keyword{datasets}
