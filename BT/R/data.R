#' Simulated Database.
#'
#' A simulated database used for examples and vignettes.
#' The variables are related to a motor insurance pricing context.
#'
#' @format
#' A simulated data frame with 50,000 rows and 7 columns, containing simulation of different policyholders:
#' \describe{
#' \item{Gender}{Gender, varying between male and female.}
#' \item{Age}{Age, varying from 18 to 65years old.}
#' \item{Split}{Noisy variable, not used to simulate the response variable. It allows to assess how the algorithm handle these features.}
#' \item{Sport}{Car type, varying between yes (sport car) or no.}
#' \item{ExpoR}{Yearly exposure-to-risk, varying between 0 and 1.}
#' \item{Y}{Yearly claim number, simulated thanks to Poisson distribution.}
#' \item{Y_normalized}{Yearly claim frequency, corresponding to the ratio between Y and ExpoR.}
#' }
"BT_Simulated_Data"
