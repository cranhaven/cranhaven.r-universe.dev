\name{OCpoint}
\alias{OCpoint}
\docType{data}
\title{
Soil Defense Public works for the Umbria Region}
\description{
Dataset collecting data about soil defense public works in the Umbria region retrieved from the Open Coesione repository. Data are georeferenced with point coordinates.}
\usage{data("OCpoint")}
\format{
  A data frame with 82 observations on the following 44 variables.
  \describe{
    \item{\code{LocalProjectCode}}{a character vector}
    \item{\code{CUP}}{a character vector}
    \item{\code{Intervention}}{a character vector}
    \item{\code{COD_REGION}}{a character vector}
    \item{\code{DEN_REGION}}{a character vector}
    \item{\code{COD_PROVINCE}}{a character vector}
    \item{\code{DEN_PROVINCE}}{a character vector}
    \item{\code{COD_MUNICIPALITY}}{a character vector}
    \item{\code{DEN_MUNICIPALITY}}{a character vector}
    \item{\code{EuFunding}}{a numeric vector}
    \item{\code{FESR_EuFunding}}{a numeric vector}
    \item{\code{FSE_EuFunding}}{a numeric vector}
    \item{\code{FEASR_EuFunding}}{a numeric vector}
    \item{\code{FEAMP_EuFunding}}{a numeric vector}
    \item{\code{IOG_EuFunding}}{a numeric vector}
    \item{\code{FondoDiRotazioneITA}}{a numeric vector}
    \item{\code{FSC_FundingITA}}{a numeric vector}
    \item{\code{PAC_FundingITA}}{a numeric vector}
    \item{\code{CompletamentiFunding_ITA}}{a numeric vector}
    \item{\code{OtherMeasuresFundingITA}}{a numeric vector}
    \item{\code{RegionFunding}}{a numeric vector}
    \item{\code{ProvinceFunding}}{a numeric vector}
    \item{\code{MunicipalityFunding}}{a numeric vector}
    \item{\code{ReleasedResources}}{a logical vector}
    \item{\code{OtherPublicFunding}}{a numeric vector}
    \item{\code{ForeignStateFunding}}{a logical vector}
    \item{\code{PrivateFunding}}{a numeric vector}
    \item{\code{TotalPublicFunding}}{a numeric vector}
    \item{\code{TotalFunding}}{a numeric vector}
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
    \item{\code{geom}}{a character vector}
  }
}
\details{
Dataset is obtained using the \code{\link{get_data_region_OC}} function.
}
\source{
\url{https://opencoesione.gov.it/it/}
}

\examples{
data(OCpoint)
}
\keyword{datasets}
