#' Convert Continuous BMI Values into 3-Level Factor
#' 
#' Converts a continuous BMI variable into a 3-level factor variable: Normal 
#' weight if \code{[-Inf, 25)}, Overweight if \code{[25, 30)}, and Obese if 
#' \code{[30, Inf)}.
#' 
#' @param x Numeric vector of BMI values.
#' @param labels If \code{TRUE}, factor levels are labeled 
#' \code{"Normal weight"}, \code{"Overweight"}, and \code{"Obese"}; if 
#' \code{FALSE}, factor levels are \code{[-Inf, 25)}, \code{[25, 30)}, and 
#' \code{[30, Inf)}.
#' 
#' @return Factor variable with 3 levels.
#' 
#' @export
bmi3 <- function(x, labels = TRUE) {
  if (labels) {
    y <- cut(x, breaks = c(-Inf, 25, 30, Inf), right = F,
             labels = c("Normal weight", "Overweight", "Obese"))
  } else {
    y <- cut(x, breaks = c(-Inf, 25, 30, Inf), right = F)
  }
  return(y)
}