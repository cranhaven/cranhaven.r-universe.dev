#' @name SMR.clean
#' @aliases SMR.clean
#' @docType data
#' @title Standard Metabolic Rate: Corrected Raw Data
#' @description A dataset contains raw data of standard metabolic rate measurements corrected for background respiration using the function \code{\link{correct.meas}}
#' @usage SMR.clean
#' @format A data frame with 76800 rows and 17 variables:
#' \describe{
#'   \item{Date.Time}{date and time (yyyy/mm/dd hh:mm:ss)}
#'   \item{Date}{date (yyyy/mm/dd)}
#'   \item{Real.Time}{time (hh:mm:ss)}
#'   \item{Time}{ordinal number of seconds in each measurement phase (1-1200)}
#'   \item{Phase}{the type of phase and an ordinal number of measurements (e.g. M1, F3)}
#'   \item{Start.Meas}{the first second of a measurement phase (hh:mm:ss)}
#'   \item{End.Meas}{the last second of a measurement phase (hh:mm:ss)}
#'   \item{Chamber.No}{the number of a chamber}
#'   \item{Ind}{ID of an animal}
#'   \item{Mass}{wet mass of an animal (g)}
#'   \item{Volume}{the volume of a chamber (mL)}
#'   \item{Init.O2}{initial level of dissolved oxygen (mgO2/L)}
#'   \item{Temp}{temperature at each second (\eqn{C^{o}})}
#'   \item{O2}{actual level of dissolved oxygen at each second (mgO2/L)}
#'   \item{BR}{slope of background respiration (\eqn{mg O_{2}\;L^{-1} s^{-1}})}
#'   \item{O2.correct}{actual level of dissolved oxygen at each second corrected by slope of background respiration (mgO2/L)}
#'   \item{DO.unit}{the measure unit of DO concentration}
#' }
"SMR.clean"
