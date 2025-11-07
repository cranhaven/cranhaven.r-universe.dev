#' mSHAP
#' 
#' A function for calculation SHAP values of two-part models.
#' 
#' This function allows the user to input the SHAP values for two separate 
#' models (along with the expected values), and mSHAP then outputs the SHAP
#' values of the two model predictions multiplied together.
#' 
#' An included feature of the function is the ability to pass data frames 
#' that do not have the same number of columns. Say for instance that one
#' model benefits from a certain variable but the other does not. As long
#' as the `shap_*_names` arguments are supplied, the function will 
#' automatically add a column of 0's for missing variables in either data frame
#' (matrix). This corresponds to a SHAP value of 0, which of course is accurate
#' if the variable was not included in the model.
#'
#' @param shap_1,shap_2 The SHAP values that will be multiplied together.  They
#'   may be matrices or data frames, and up to one may be a list where each 
#'   element is a matrix or data frame (this is necessary when one of the 
#'   models is a multinomial classifier, for instance).  Each data frame or 
#'   matrix here must have the same number of rows, and if there are different
#'   numbers of columns or the columns are not the same, then `shap_*_names` 
#'   must be specified.
#' @param ex_1,ex_2 The expected values of the models across the training set.
#'   If one of the arguments `shap_*` is a list, then the corresponding `ex_*`
#'   argument must be a vector (or array) of the same length as the list.
#' @param shap_1_names,shap_2_names The character vector containing the names 
#'   of the columns in `shap_1` and `shap_2`, respectively.  These must be in 
#'   the same order as the columns themselves.  If a list is passed to one of 
#'   the `shap_*` arguments, it does NOT affect the corresponding 
#'   `shap_*_names` argument, which will still be a single character vector.
#'
#' @return A list containing the multiplied SHAP values and the expected value.
#'   Or, in the case of a list passed as one of the `shap_*` augments, a list 
#'   of lists where each element corresponds to the same element in the list
#'   passed to `shap_*`.
#' @export
#' 
#' @importFrom magrittr %>%
#' 
#' @examples 
#' 
#' if (interactive()) {
# Simulate shap values
#' shap1 <- data.frame(
#'   age = runif(1000, -5, 5),
#'   income = runif(1000, -5, 5),
#'   married = runif(1000, -5, 5),
#'   sex = runif(1000, -5, 5)
#' )
#' shap2 <- list(
#'   data.frame(
#'     age = runif(1000, -5, 5),
#'     income = runif(1000, -5, 5),
#'     married = runif(1000, -5, 5),
#'     sex = runif(1000, -5, 5)
#'   ),
#'   data.frame(
#'     age = runif(1000, -5, 5),
#'     income = runif(1000, -5, 5),
#'     married = runif(1000, -5, 5),
#'     sex = runif(1000, -5, 5)
#'   ),
#'   data.frame(
#'     age = runif(1000, -5, 5),
#'     income = runif(1000, -5, 5),
#'     married = runif(1000, -5, 5),
#'     sex = runif(1000, -5, 5)
#'   )
#' )
#' 
#' ex1 <- 3
#' ex2 <- c(4, 5, 6)
#' 
#' # Case where both models have a single output
#' res1 <- mshap(
#'   shap_1 = shap1,
#'   shap_2 = shap2[[1]],
#'   ex_1 = ex1,
#'   ex_2 = ex2[1]
#' )
#' View(res1$shap_vals)
#' res1$expected_value
#' 
#' # Case where one of your models has multiple outputs that are explained
#' res2 <- mshap(
#'   shap_1 = shap1,
#'   shap_2 = shap2,
#'   ex_1 = ex1,
#'   ex_2 = ex2
#' )
#' View(res2[[1]]$shap_vals)
#' res2[[1]]$expected_value
#' 
#' # Case where the models have different variables
#' res3 <- mshap(
#'   shap_1 = shap1,
#'   shap_2 = shap2,
#'   ex_1 = ex1,
#'   ex_2 = ex2,
#'   shap_1_names = c("Age", "Income", "Married", "Sex"),
#'   shap_2_names = c("Age", "Income", "Children", "American")
#' )
#' # Note how there are now 6 columns of SHAP values, since there are 6
#' # distinct variables
#' View(res3[[1]]$shap_vals)
#' res3[[1]]$expected_value
#' }
mshap <- function(
  shap_1, 
  shap_2, 
  ex_1, 
  ex_2,
  shap_1_names = NULL,
  shap_2_names = NULL
) {
  
  if ("list" %in% class(shap_1) & "list" %in% class(shap_2)) {
    stop("`mshap::mshap()` is not currently set up to handle multiple matrices in each `shap_*` argument.  Did you accidentally wrap a matrix in a `list()`?")
  } else if ("list" %in% class(shap_1) | "list" %in% class(shap_2)) {
    if ("list" %in% class(shap_1)) {
      main <- shap_1
      main_ex <- ex_1
      secondary <- shap_2
      sec_ex <- ex_2
    } else {
      main <- shap_2
      main_ex <- ex_2
      secondary <- shap_1
      sec_ex <- ex_1
    }
    
    l <- purrr::map2(
      .x = main,
      .y = main_ex,
      .f = ~{
        multiply_shap(
          shap_1 = .x,
          shap_2 = secondary,
          ex_1 = .y,
          ex_2 = sec_ex,
          shap_1_names = shap_1_names,
          shap_2_names = shap_2_names
        )
      }
    )
    
  } else {
    l <- multiply_shap(
      shap_1,
      shap_2,
      ex_1,
      ex_2,
      shap_1_names,
      shap_2_names
    )
  }
  return(l)
}