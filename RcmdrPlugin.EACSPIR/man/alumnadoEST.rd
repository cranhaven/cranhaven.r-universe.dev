\name{alumnadoEST}
\alias{alumnadoEST}
\docType{data}
\title{
Valoracion del alumnado de la asignatura de Estadistica
}
\description{
Datos acerca de la valoracion de todo el material docente elaborado para la asignatura 'Analisi de Dades en Psicologia' por parte de una muestra de estudiantes del curso 2008-09. En concreto, se evaluo el grado de satisfaccion respecto a diversos materiales docentes elaborados para esta asignatura. Se pretendia, ademas, relacionar estos resultados con otras variables, como son la calificacion obtenida en la asignatura, el hecho de haber cursado previamente una asignatura niveladora (Fonaments Matematics). 
}
\usage{alumnadoEST}
\format{
  Una base de datos con 250 observaciones y  28 variables.
  \describe{
    \item{\code{identificador}}{Identificador de la persona encuestada}
    \item{\code{nota}}{Nota cualitativa de la persona}
    \item{\code{repetidor}}{Repite o no la asignatura}
    \item{\code{fundamentos.matematicos}}{Ha cursado o no la asignatura niveladora Fonaments Matematics}
    \item{\code{uso.libros.biblio}}{Ha usado o no las referencias propuestas en la bibliografia}
    \item{\code{uso.libros.prof}}{Ha usado o no las referencias elaboradas por el profesorado}
    \item{\code{uso.cuadro.resumen}}{Ha usado o no el cuadro resumen de la asignatura}
    \item{\code{uso.ppt}}{Ha usado o no las transparencias Power Point de la asignatura}
    \item{\code{uso.dosier}}{Ha usado o no el dosier de practicas y sus soluciones para preparar la asignatura}
    \item{\code{uso.formulario}}{Ha usado o no el formulario}
    \item{\code{uso.glosario}}{Ha usado o no el glosario de la asignatura}
    \item{\code{uso.Pythia}}{Ha usado o no el recurso Pythia}
    \item{\code{uso.CDs}}{Ha usado o no los CDs interactivos de la asignatura}
    \item{\code{uso.clases.teor}}{Ha asistido o no a las sesiones teoricas}
    \item{\code{uso.tutorias}}{Ha asistido o no a las tutorias}
    \item{\code{uso.clases.prac}}{Ha asistido o no a las sesiones practicas}
    \item{\code{libros.biblio}}{Grado de satisfaccion respecto a las referencias propuestas en la bibliografia}
    \item{\code{libros.prof}}{Grado de satisfaccion respecto a las referencias elaboradas por el profesorado}
    \item{\code{cuadro.resumen}}{Grado de satisfaccion respecto al cuadro resumen de la asignatura}
    \item{\code{ppt}}{Grado de satisfaccion respecto a las transparencias Power Point de la asignatura}
    \item{\code{dosier}}{Grado de satisfaccion respecto al dosier de practicas}
    \item{\code{formulario}}{Grado de satisfaccion respecto al formulario}
    \item{\code{glosario}}{Grado de satisfaccion respecto al glosario de la asignatura}
    \item{\code{Pythia}}{Grado de satisfaccion respecto al recurso Pythia}
    \item{\code{CDs}}{Grado de satisfaccion respecto a los CDs interactivos de la asignatura}
    \item{\code{teoria}}{Grado de satisfaccion respecto a las sesiones teoricas}
    \item{\code{tutorias}}{Grado de satisfaccion respecto a las tutorias}
    \item{\code{tutorias}}{Grado de satisfaccion respecto a las practicas}
  }
}

\examples{
  data(alumnadoEST)
  summary(alumnadoEST)
}

\keyword{datasets}
