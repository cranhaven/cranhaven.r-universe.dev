#' Infectious mononucleosis transcriptome
#'
#' This is compilied numeric matrix of raw microarray intensities data. Each of 100 rows corresponds to a
#' probe (gene, transcript) and each of 89 column correspondes to a specimen (patient). Specimen is total mRNA
#' samples from human peripheral blood leukocytes' taken from healthy children and children with
#' infectious mononucleosis of different ethiology.
#' Specimen features are in \code{IMspecimen} data.
#'
#'
#' \itemize{
#'   \item rownames. Probes IDs. For most transcripts there are several probes.
#'   \item colnames. Specimen IDs.
#'   }
#'
#' @author Nikolai A. Sakharnov, Dmitry I. Knyazev, Oleg V. Utkin
#' @docType data
#' @keywords datasets
#' @name IMexpression
#' @usage data(IMexpression)
#' @format A matrix with 100 rows and 89 variables
"IMexpression"


#' Specimen features
#'
#' A dataset containing information about specimens from \code{IMexpression} data: IDs and diagnoses.
#'
#' \itemize{
#'   \item ID. Specimen ID, factor variable with 89 levels.
#'   \item diagnosis. Specimen diagnosis:
#'   "ebv" (children with acute EBV mononucleosis),
#'   "hhv6" (children with acute HHV6 mononucleosis),
#'   "norm" (healthy children). Factor variable with 3 levels.
#' }
#'
#' @author Nikolai A. Sakharnov, Dmitry I. Knyazev, Oleg V. Utkin
#' @docType data
#' @keywords datasets
#' @name IMspecimen
#' @usage data(IMspecimen)
#' @format A data frame with 89 rows and 2 variables.
"IMspecimen"
