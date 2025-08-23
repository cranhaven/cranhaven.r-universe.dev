\name{OBDAPpoint}
\alias{OBDAPpoint}
\docType{data}
\title{
Soil Defense Public Work for the Molise.}
\description{
Dataset collecting data about soil defense public works in the Molise region retrieved from the Open BDAP repository. Data are georeferenced with point coordinates.
}
\usage{data("OBDAPpoint")}
\format{
  A data frame with 722 observations on the following 22 variables.
  \describe{
    \item{\code{LocalProjectCode}}{a character vector}
    \item{\code{CUP}}{a character vector}
    \item{\code{Intervention}}{a character vector}
    \item{\code{EffectiveDesignStartingDate}}{a character vector}
    \item{\code{EffectiveDesignEndingDate}}{a character vector}
    \item{\code{WorksExecutionStartingDate}}{a character vector}
    \item{\code{WorksExecutionEndingDate}}{a character vector}
    \item{\code{ConclusionStartingDate}}{a character vector}
    \item{\code{ConclusionEndingDate}}{a character vector}
    \item{\code{Operability}}{a character vector}
    \item{\code{StateFunding}}{a numeric vector}
    \item{\code{EuFunding}}{a numeric vector}
    \item{\code{LocalAuthoritiesFunding}}{a numeric vector}
    \item{\code{PrivateFunding}}{a numeric vector}
    \item{\code{OtherFunding}}{a numeric vector}
    \item{\code{COD_MUNICIPALITY}}{a character vector}
    \item{\code{COD_PROVINCE}}{a character vector}
    \item{\code{COD_REGION}}{a character vector}
    \item{\code{DEN_MUNICIPALITY}}{a character vector}
    \item{\code{DEN_PROVINCE}}{a character vector}
    \item{\code{DEN_REGION}}{a character vector}
    \item{\code{geom}}{a character vector}
  }
}
\details{
Dataset is obtained using the \code{\link{get_data_OBDAP}} function.
}
\source{
\url{https://openbdap.rgs.mef.gov.it/}
}
\examples{
data(OBDAPpoint)
}
\keyword{datasets}
