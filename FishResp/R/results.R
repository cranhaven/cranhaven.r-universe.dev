#' @name results
#' @aliases results
#' @docType data
#' @title Results of Analysis: SMR, AMR and MS
#' @description A final dataset containing information about both standard and active metabolic rates, and metabolic scope obtained by using the function \code{\link{export.MR}}.
#' @usage results
#' @format A data frame with 36 rows and 18 variables:
#' \describe{
#'   \item{Chamber.No}{The number of a chamber}
#'   \item{Ind}{ID of an animal}
#'   \item{Mass}{wet mass of an animal (g)}
#'   \item{Volume}{the volume of a chamber (mL)}
#'   \item{DO.unit}{the measure unit of DO concentration}
#'   \item{SMR_Temp}{Average temperature over a period of a measurement phase (\eqn{C^{o}})}
#'   \item{SMR_R2}{\eqn{r^{2} = 0} of a slope of animal oxygen consumption without background respiration}
#'   \item{SMR_BR}{Percentage rate of background respiration}
#'   \item{SMR_MR.abs}{Absolute SMR (\eqn{mg O_{2}\;h^{-1}})}
#'   \item{SMR_MR.mass}{Mass-specific SMR (\eqn{mg O_{2}\;kg^{-1} h^{-1}})}
#'   \item{AMR_Temp}{Average temperature over a period of a measurement phase (\eqn{C^{o}})}
#'   \item{AMR_R2}{\eqn{r^{2} = 0} of a slope of animal oxygen consumption without background respiration}
#'   \item{AMR_BR}{Percentage rate of background respiration}
#'   \item{AMR_MR.abs}{Absolute AMR (\eqn{mg O_{2}\;h^{-1}})}
#'   \item{AMR_MR.mass}{Mass-specific AMR (\eqn{mg O_{2}\;kg^{-1} h^{-1}})}
#'   \item{MS.abs}{Absolute metabolic scope: the difference between absolute AMR and SMR (\eqn{mg O_{2}\;h^{-1}})}
#'   \item{MS.mass}{Mass-specific metabolic scope: the difference between mass-specific AMR and SMR (\eqn{mg O_{2}\;kg^{-1} h^{-1}})}
#'   \item{MS.fact}{Factorial metabolic scope: the ratio between AMR and SMR}
#' }
"results"
