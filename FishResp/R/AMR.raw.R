#' @name AMR.raw
#' @aliases AMR.raw
#' @docType data
#' @title Active Metabolic Rate: Raw Data
#' @description The dataset contains raw data of active metabolic rate measurements obtained by using the function \code{\link{import.meas}})
#' @usage AMR.raw
#' @format A data frame with 1800 rows and 16 variables:
#' \describe{
#'   \item{Date.Time}{date and time (yyyy/mm/dd hh:mm:ss)}
#'   \item{Phase}{the type of phase and an ordinal number of measurements (e.g. M1, F3)}
#'   \item{Temp.1}{temperature at each second (\eqn{C^{o}})}
#'   \item{Ox.1}{actual level of dissolved oxygen at each second (mgO2/L)}
#'   \item{Real.Time}{time (hh:mm:ss)}
#'   \item{Date}{date (yyyy/mm/dd)}
#'   \item{Time}{ordinal number of seconds in each measurement phase (1-600)}
#'   \item{Start.Meas}{the first second of a measurement phase (hh:mm:ss)}
#'   \item{End.Meas}{the last second of a measurement phase (hh:mm:ss)}
#'   \item{Total.Phases}{the total number of measurement phases (constant value)}
#'   \item{Ox.2}{see Ox.1}
#'   \item{Ox.3}{see Ox.1}
#'   \item{Ox.4}{see Ox.1}
#'   \item{Temp.2}{see Temp.1}
#'   \item{Temp.3}{see Temp.1}
#'   \item{Temp.4}{see Temp.1}
#' }
"AMR.raw"
