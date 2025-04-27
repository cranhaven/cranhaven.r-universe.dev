#' @name SMR
#' @aliases SMR
#' @docType data
#' @title Standard Metabolic Rate: Final Data
#' @description A dataset contains background respiration, absolute and mass-specific standard metabolic rate data obtained by using the function \code{\link{calculate.MR}}
#' @usage SMR
#' @format A data frame with 12 rows and 16 variables:
#' \describe{
#'   \item{Chamber.No}{the number of a chamber}
#'   \item{Ind}{ID of an animal}
#'   \item{Mass}{wet mass of an animal (g)}
#'   \item{Volume}{the volume of a chamber (mL)}
#'   \item{Date.Time}{date and time of a measurement phase (yyyy/mm/dd hh:mm:ss)}
#'   \item{Phase}{the type of phase and an ordinal number of measurements (e.g. M1)}
#'   \item{Temp}{average temperature over the period of a measurement phase (\eqn{C^{o}})}
#'   \item{Slope.with.BR}{slope of animal oxygen consumption with slope of background respiration (\eqn{mg O_{2}\;L^{-1} s^{-1}})}
#'   \item{Slope}{slope of animal oxygen consumption without background respiration (\eqn{mg O_{2}\;L^{-1} s^{-1}})}
#'   \item{SE}{standard error of a slope of animal oxygen consumption without background respiration (\eqn{mg O_{2}\;L^{-1} s^{-1}})}
#'   \item{R2}{\eqn{r^{2}}  of a slope of animal oxygen consumption without background respiration}
#'   \item{MR.abs.with.BR}{absolute SMR with background respiration (\eqn{mg O_{2}\;h^{-1}})}
#'   \item{BR}{percentage rate of background respiration}
#'   \item{MR.abs}{absolute SMR (\eqn{mg O_{2}\;h^{-1}})}
#'   \item{MR.mass}{mass-specific SMR (\eqn{mg O_{2}\;kg^{-1} h^{-1}})}
#'   \item{DO.unit}{the measure unit of DO concentration}
#' }
"SMR"
