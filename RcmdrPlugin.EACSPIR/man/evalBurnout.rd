\name{evalBurnout}
\alias{evalBurnout}
\docType{data}
\title{
Evaluacion del Burnout en personal de enfermeria
}
\description{
Datos obtenidos mediante un instrumento para la evaluacion del Burnout en el personal de enfermeria y descripcion general de las variables predictoras.
}
\usage{evalBurnout}
\format{
  Una base de datos con 62 observaciones y 55 variables.
  \describe{
    \item{\code{sexo}}{Sexo de la persona encuestada}
    \item{\code{edad}}{Edad de la persona encuestada}
    \item{\code{relaciones}}{Relaciones personales}
    \item{\code{hijos}}{Numero de hijos}
    \item{\code{antig.anos}}{Antiguedad de la persona encuestada medida en anos}
    \item{\code{antig.meses}}{Antiguedad de la persona encuestada medida en meses}
    \item{\code{sit.laboral}}{Situacion laboral de la persona encuestada}
    \item{\code{dedicacion}}{Dedicacion de la persona encuestada}
    \item{\code{turno}}{Turno de trabajo de la persona encuestada}
    \item{\code{horas.semana}}{Horas trabajadas a la semana por la persona encuestada}
    \item{\code{horas.turno}}{Horas trabajadas en el turno por la persona encuestada}
    \item{\code{personal.cargo}}{Personal a cargo de la persona encuestada}
    \item{\code{servicio}}{Servicio(s) en el que trabaja la persona encuestada}
    \item{\code{vuln.BRP.1}}{Item 1 de vulnerabilidad relacionada con la baja realizacion personal}
    \item{\code{vuln.BRP.2}}{Item 2 de vulnerabilidad relacionada con la baja realizacion personal}
    \item{\code{vuln.BRP.3}}{Item 3 de vulnerabilidad relacionada con la baja realizacion personal}
    \item{\code{vuln.BRP.4}}{Item 4 de vulnerabilidad relacionada con la baja realizacion personal}
    \item{\code{vuln.BRP.5}}{Item 5 de vulnerabilidad relacionada con la baja realizacion personal}
    \item{\code{vuln.BRP.6}}{Item 6 de vulnerabilidad relacionada con la baja realizacion personal}
    \item{\code{vuln.D.1}}{Item 1 de vulnerabilidad relacionada con la despersonalizacion}
    \item{\code{vuln.D.2}}{Item 2 de vulnerabilidad relacionada con la despersonalizacion}
    \item{\code{vuln.D.3}}{Item 3 de vulnerabilidad relacionada con la despersonalizacion}
    \item{\code{vuln.D.4}}{Item 4 de vulnerabilidad relacionada con la despersonalizacion}
    \item{\code{vuln.D.5}}{Item 5 de vulnerabilidad relacionada con la despersonalizacion}
    \item{\code{vuln.D.6}}{Item 6 de vulnerabilidad relacionada con la despersonalizacion}
    \item{\code{vuln.AE.1}}{Item 1 de vulnerabilidad relacionada con la autoestima}
    \item{\code{vuln.AE.2}}{Item 2 de vulnerabilidad relacionada con la autoestima}
    \item{\code{vuln.AE.3}}{Item 3 de vulnerabilidad relacionada con la autoestima}
    \item{\code{vuln.AE.4}}{Item 4 de vulnerabilidad relacionada con la autoestima}
    \item{\code{vuln.AE.5}}{Item 5 de vulnerabilidad relacionada con la autoestima}
    \item{\code{vuln.AE.6}}{Item 6 de vulnerabilidad relacionada con la autoestima}
    \item{\code{exp.riesgo.1}}{Item 1 de exposicion al riesgo}
    \item{\code{exp.riesgo.2}}{Item 2 de exposicion al riesgo}
    \item{\code{exp.riesgo.3}}{Item 3 de exposicion al riesgo}
    \item{\code{exp.riesgo.4}}{Item 4 de exposicion al riesgo}
    \item{\code{exp.riesgo.5}}{Item 5 de exposicion al riesgo}
    \item{\code{exp.riesgo.6}}{Item 6 de exposicion al riesgo}
    \item{\code{exp.riesgo.7}}{Item 7 de exposicion al riesgo}
    \item{\code{exp.riesgo.8}}{Item 8 de exposicion al riesgo}
    \item{\code{exp.riesgo.9}}{Item 9 de exposicion al riesgo}
    \item{\code{exp.riesgo.10}}{Item 10 de exposicion al riesgo}
    \item{\code{burn.1}}{Item 1 de Burnout}
    \item{\code{burn.2}}{Item 2 de Burnout}
    \item{\code{burn.3}}{Item 3 de Burnout}
    \item{\code{burn.4}}{Item 4 de Burnout}
    \item{\code{burn.5}}{Item 5 de Burnout}
    \item{\code{burn.6}}{Item 6 de Burnout}
    \item{\code{burn.7}}{Item 7 de Burnout}
    \item{\code{burn.8}}{Item 8 de Burnout}
    \item{\code{burn.9}}{Item 9 de Burnout}
    \item{\code{burn.10}}{Item 10 de Burnout}
    \item{\code{burn.11}}{Item 11 de Burnout}
    \item{\code{burn.12}}{Item 12 de Burnout}
    \item{\code{burn.13}}{Item 13 de Burnout}
    \item{\code{burn.14}}{Item 14 de Burnout}
  }  
}

\examples{
  data(evalBurnout)
  table(evalBurnout$servicio)
}

\keyword{datasets}
