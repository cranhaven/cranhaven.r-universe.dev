#' Pt01 seizure 1 around seizure onset
#'
#' This data corresponds to the first seizure of patient from
#' the Fragility Data Set. EcoG recording gathered in collaboration with the National Institute of Health. The data contains only the good channels.
#' It has been notch filtered and common average referenced in RAVE.
#' The time range for full data is (-10:10s). Due to the size limit of the package, The full data has been epoched -1:2s around the seizure onset.
#' The acquisition frequency is 1000 Hz
#' 
#' @docType data
#'
#' @usage
#' ## EEG data
#' data(pt01EcoG)
#'
#' @format
#' pt01EcoG: A Matrix with 84 rows (electrodes) and 3000 columns (time points)
#'
#' pt01Frag: A fragility object results of applying the main function \code{calcAdjFrag} to pt01EcoG with `window` = 250 and `step` = 125
#'
#' @keywords datasets
#'
#' @source Fragility Multi-Center Retrospective Study
#' (\href{https://openneuro.org/datasets/ds003029/versions/1.0.0}{OpenNeuro})
#'
#' @aliases pt01EcoG pt01Frag
"pt01EcoG"
