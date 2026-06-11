#' Convert Continuous BMI Values into 4-Level Factor
#' 
#' Converts a continuous BMI variable into a 4-level factor variable: 
#' Underweight if \code{[-Inf, 18.5)}, Normal weight if \code{[18.5, 25)}, 
#' Overweight if \code{[25, 30)}, and Obese if \code{[30, Inf)}.
#' 
#' @param x Numeric vector of BMI values.
#' @param labels If \code{TRUE}, factor levels are labeled \code{"Underweight"}, 
#' \code{"Normal weight"}, \code{"Overweight"}, and \code{"Obese"}; if 
#' \code{FALSE}, factor levels are \code{[-Inf, 18.5)}, \code{[18.5, 25)}, 
#' \code{[25, 30)}, and \code{[30, Inf)}.
#' 
#' @return Factor variable with 4 levels.
#' 
#' @export
bmi4 <- function(x, labels = TRUE) {
  if (labels) {
    y <- cut(x, breaks = c(-Inf, 18.5, 25, 30, Inf), right = F,
             labels = c("Underweight", "Normal weight", "Overweight", "Obese"))
  } else {
    y <- cut(x, breaks = c(-Inf, 18.5, 25, 30, Inf), right = F)
  }
  return(y)
}