#' Dataset for the standard components values
#'
#' A list containing the standard values provided for each component.
#' "Res" corresponds to the values of R1, R2, R3 and R4,
#' The values in the same index of "ThBeta" and "ThVal" provides nine
#' types of (beta, Rb) of a thermistor.
#'
#' @docType data
#'
#' @usage data(CompValues)
#'
#' @format A named list containing the standard values of each component:
#' \describe{
#'   \item{"Res"}{the standard values of R1, R2, R3 and R4}
#'   \item{"ThBeta"}{the standard values of temperature coefficient}
#'   \item{"ThVal"}{the standard values of nominal thermistor resistances}
#' }
#'
#' @keywords dataset
#'
#'
#' @examples
#' data(EzGP_data)
"CompValues"
