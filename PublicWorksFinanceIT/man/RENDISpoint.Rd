\name{RENDISpoint}
\alias{RENDISpoint}
\docType{data}
\title{
Soil Defense Public Works for the Basilicata Region.
}
\description{
Dataset collecting data about soil defense public works in the Basilicata region retrieved from the ReNDiS repository. Data are georeferenced with point coordinates.
}
\usage{data("RENDISpoint")}
\format{
  A data frame with 210 observations on the following 27 variables.
  \describe{
    \item{\code{CUP}}{a character vector}
    \item{\code{Intervention}}{a character vector}
    \item{\code{Type}}{a character vector}
    \item{\code{Finance}}{a numeric vector}
    \item{\code{DEN_MUNICIPALITY}}{a character vector}
    \item{\code{DEN_REGION}}{a character vector}
    \item{\code{COD_REGION}}{a character vector}
    \item{\code{COD_MUNICIPALITY}}{a character vector}
    \item{\code{COD_PROVINCE}}{a character vector}
    \item{\code{DEN_PROVINCE}}{a character vector}
    \item{\code{FeasibilityStudyStartingDate}}{a character vector}
    \item{\code{FeasibilityStudyEndingDate}}{a character vector}
    \item{\code{PreliminaryDesignStartingDate}}{a character vector}
    \item{\code{PreliminaryDesignEndingDate}}{a character vector}
    \item{\code{DefinitiveDesignStartingDate}}{a character vector}
    \item{\code{DefinitiveDesignEndingDate}}{a character vector}
    \item{\code{ExecutiveDesignStartingDate}}{a character vector}
    \item{\code{ExecutiveDesignEndingDate}}{a character vector}
    \item{\code{EffectiveDesignStartingDate}}{a character vector}
    \item{\code{EffectiveDesignEndingDate}}{a character vector}
    \item{\code{WorksExecutionStartingDate}}{a character vector}
    \item{\code{WorksExecutionEndingDate}}{a character vector}
    \item{\code{ConclusionStartingDate}}{a character vector}
    \item{\code{ConclusionEndingDate}}{a character vector}
    \item{\code{InterventionClosed}}{a character vector}
    \item{\code{Operability}}{a character vector}
    \item{\code{geom}}{a character vector}
  }
}
\details{
Dataset is obtained using the \code{\link{get_data_RENDIS}} function.
}
\source{
\url{http://www.rendis.isprambiente.it/rendisweb/}
}
\examples{
data(RENDISpoint)
}
\keyword{datasets}
