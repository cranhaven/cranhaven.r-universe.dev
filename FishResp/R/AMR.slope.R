#' @name AMR.slope
#' @aliases AMR.slope
#' @docType data
#' @title Active Metabolic Rate: Extracted Slope(s)
#' @description A dataset contains extracted slopes for further AMR calculations and other attributes of active metabolic rate measurements obtained by using the function \code{\link{extract.slope}}
#' @usage AMR.slope
#' @format A data frame with 12 rows and 12 variables:
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
#'   \item{R2}{\eqn{r^{2}} of a slope of animal oxygen consumption without background respiration}
#'   \item{DO.unit}{the measure unit of DO concentration}
#' }
"AMR.slope"
