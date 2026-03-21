#' Simulated time data of the coffee process
#'
#' This dataset shows the actions required to make coffee. 
#' The simulated dataset of 10 subjects correspond to the timestamps (in s)
#' of each action. Each value is the time elapse between the beginning of the 
#' coffee process and the execution of an action.
#'
#' @docType data
#'
#' @usage data(coffee)
#'
#' @format A data frame with 10 rows and 6 variables:
#' \describe{
#'   \item{id}{Midwife students ID.}
#'   \item{coffee}{Time (in s) when the subject takes the coffee capsule.}
#'   \item{fill_coffee}{Time (in s) when the subject puts the coffee capsule into the machine.}
#'   \item{fill_water}{Time (in s) when the subject fills the water tank of the coffee machine.}
#'   \item{push_B}{Time (in s) when the subject pushes the button to start the machine.}
#'   \item{drink}{Time (in s) when the subject drinks the coffee.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(coffee)
#' head(coffee)
"coffee"
