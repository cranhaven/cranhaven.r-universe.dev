#' MNREAD data collected in subjects with low vision.
#'
#' A dataset containing raw MNREAD data for 12 subjects with low vision. 
#' 6 subjects were treated with treatment A while the other 6 were given treatment B.
#' Each subject was tested twice on the MNREAD:
#' \itemize{
#' \item once on the regular polarity of the test (black print on white background)
#' \item once on the reverse polarity of the test (white print on black background)
#' }
#'
#' @format A data frame with 437 rows and 7 variables, where each line stores data for one sentence:
#' \describe{
#'   \item{subject}{subject ID code}
#'   \item{polarity}{test polarity used (regular or reverse)}
#'   \item{treatment}{treatment given to the subject (A or B)}
#'   \item{vd}{viewing distance in cm}
#'   \item{ps}{print size in logMAR, as written on the chart (print size uncorrected for viewing distance)}
#'   \item{rt}{reading time in seconds}
#'   \item{err}{number of errors}
#'   ...
#' }
#' @source Data collected at the Minnesota Laboratory for Low-Vision Research (UMN)
"data_low_vision"
