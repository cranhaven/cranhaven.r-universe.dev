#' Data in meta-analysis reported in review CD003366, 'Cochrane library'. 
#'
#' A dataset containing the meta-data of the outcome 'Leukopaenia' (CMP005), 
#' in the review "Texane-containing regimins for metastatic breast cancer" (CD003366)
#' the results were reported by 28 studies, and analysed by Random-Effects meta-analysis.
#'
#' @format A data frame with 28 rows and 12 variables:
#' \describe{
#'   \item{STUDY}{Name of the study.}
#'   \item{STUDY_WEIGHT}{Stydy weight in meta-analysis as reported in th review.}
#'   \item{N_EVENTS1}{Number of events in the first group tested.}
#'   \item{N_EVENTS2}{Number of events in the second group tested.}
#'   \item{N_TOTAL1}{Number of patirnts in the first group tested.}
#'   \item{N_TOTAL2}{Number of patirnts in the second group tested.}
#'   \item{GROUP1}{Names of the first group in each study.}
#'   \item{GROUP2}{Names of the second group in each study.}
#'   \item{N_STUDIES}{ Overall number of studies in the meta-analysis}
#'   \item{CMP_ID}{Cochrane Database review number}
#'   \item{SM}{A character string indicating which summary measure ("RR", "OR", "RD", or "ASD") is to be used for pooling of studies.}
#'   \item{RANDOM}{ "YES" or "NO" indicating whether random-effects meta-analysis was performed.}
#' }
#' @source \url{https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD003366.pub3/full}
"CD003366_CMP005"