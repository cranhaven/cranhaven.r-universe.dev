

#' @title Ki67 Data
#' 
#' @description 
#' Ki67 cell data containing 622 patients.
#' 
#' @details
#' The [groupedHyperframe] `Ki67` is the complete data, to be used in examples of downstream packages
#' \pkg{maxEff} and \pkg{hyper.gam}.
#' 
# The \link[base]{data.frame} `Ki67.` is a \link[base]{subset} of the complete data `Ki67`,
# which consists only the first 6 patients.  This small data is used as examples for function [as.groupedHyperframe.data.frame()]. 
#' 
#' @format
#' 
#' \describe{
#'   \item{`patientID`}{\link[base]{factor}, unique patient identifier}
#'   \item{`tissueID`}{\link[base]{factor}, TMA core identifier}
#'   \item{`recurrence`}{\link[base]{integer}, recurrence indicator, 1 = Recurred, 0 = not Recurred}
#'   \item{`recfreesurv_mon`}{\link[base]{integer}, recurrence-free survival time in months}
#'   \item{`logKi67`}{\link[base]{double}, log-transformed cell signal intensity of the protein immunofloerscence signal}
#   \item{`x`}{\link[base]{numeric}, \eqn{x}-coordinate in the cell centroid in the TMA core}
#   \item{`y`}{\link[base]{numeric}, \eqn{y}-coordinate in the cell centroid in the TMA core}
#'   \item{`age`}{\link[base]{integer}, age at diagnosis}
#'   \item{`Tstage`}{\link[base]{integer}, tumor stage}
#'   \item{`node`}{\link[base]{factor}, node stage}
#'   \item{`HR`}{\link[base]{logical}, hormone positive status}
#'   \item{`histology`}{\link[base]{integer}, histology grade}
#'   \item{`Her2`}{\link[base]{logical}, Her2 status}
#'   \item{`race`}{\link[base]{character}, race, White, Black, Asian, Native Hawaiian or Other Pacific Islander, American Indian or Alaska Native, Unknown}
#'   \item{`adj_chemo`}{\link[base]{logical}, whether completed adjuvant chemo treatment}
#'   \item{`adj_rad`}{\link[base]{integer}, adjuvant radiation treatment, 0 = unknown,  1 = done, 2 = NOT done}
#' }
#' 
#' @keywords internal
#' @name Ki67
'Ki67'
# @rdname Ki67
# 'Ki67.'






#' @title wrobel_lung
#' 
#' @description
#' From Dr. Julia Wrobel's data \url{https://sph.emory.edu/profile/faculty/julia-wrobel}.
# \url{http://juliawrobel.com/MI_tutorial/Data/lung.RDA}. # website hacked
#' 
#' @format 
#' A \link[base]{data.frame}
#' 
#' @keywords internal
'wrobel_lung'






#' @title \link[utils]{bibentry} for Quarto Book
#' 
#' @keywords internal
#' @name bib
'bioinformatics_btaf430'

