
#' @title Multiply SHAP
#' 
#' A Helper Function for the mSHAP function
#'
#' @param shap_1,shap2 matrices or data frames of SHAP values to be multiplied
#' @param ex_1,ex_2 the expected values of the models that correspond to the 
#'   shap values
#' @param shap_1_names,shap_2_names the names of the variables in the data 
#'   that was passed as shap values
#'
#' @return a list containing a data frame of SHAP values and the expected value
#' 
#' @noRd
multiply_shap <- function(
  shap_1, 
  shap_2, 
  ex_1, 
  ex_2,
  shap_1_names = NULL,
  shap_2_names = NULL
) {
  # Error Checking
  l <- validate_shap(shap_1, shap_2, ex_1, ex_2, shap_1_names, shap_2_names)
  
  # Assign variables with updated, error-free values
  shap_1 <- l$shap_1
  shap_2 <- l$shap_2
  ex_1 <- l$ex_1
  ex_2 <- l$ex_2
  
  d <- purrr::map_dfc(
    .x = 1:ncol(shap_1),
    .f = ~{
      (shap_1 %>% dplyr::pull(.x)) * c(ex_2) + 
        (shap_2 %>% dplyr::pull(.x)) * c(ex_1) + 
        ((shap_1 %>% dplyr::pull(.x)) * (shap_2 %>% rowSums())) / 2 +
        ((shap_1 %>% rowSums()) * (shap_2 %>% dplyr::pull(.x))) / 2
    }
  ) %>%
    magrittr::set_colnames(colnames(shap_1)) %>%
    suppressMessages()
  
  preds_1 <- rowSums(shap_1 %>% dplyr::mutate(ex_val = ex_1))
  preds_2 <- rowSums(shap_2 %>% dplyr::mutate(ex_val = ex_2))
  
  preds_3 <- preds_1 * preds_2
  
  expected_value <- mean(preds_3)
  
  tot_s <- rowSums(abs(d))
  shap_vals <- purrr::map_dfc(
    .x = d,
    .f = ~{
      .x + (abs(.x) / tot_s) * (ex_1 * ex_2 - expected_value)
    }
  )
  
  # return a list with what we want
  return(
    list(
      shap_vals = shap_vals,
      expected_value = expected_value
    )
  )
}


#' @title Validate the SHAP values passed to the function
#' 
#' A Simple function that throws errors when certain conditions are not met for
#' The SHAP values passed to the mSHAP function.
#'
#' @param shap_1,shap2 matrices or data frames of SHAP values to be multiplied
#' @param ex_1,ex_2 the expected values of the models that correspond to the 
#'   shap values
#' @param shap_1_names,shap_2_names the names of the variables in the data 
#'   that was passed as shap values
#'
#' @return NULL
#' 
#' @noRd
validate_shap <- function(
  shap_1, 
  shap_2, 
  ex_1, 
  ex_2,
  shap_1_names,
  shap_2_names
) {
  ## Error Checking
  if ("matrix" %in% class(shap_1)) {
    shap_1 <- as.data.frame(shap_1)
  }
  if ("matrix" %in% class(shap_2)) {
    shap_2 <- as.data.frame(shap_2)
  }
  
  # Check the column types of shap_1 and shap_2
  shap_1_class <- purrr::map_chr(shap_1, class)
  shap_2_class <- purrr::map_chr(shap_2, class)
  if (min(c(shap_1_class, shap_2_class) %in% c("integer", "numeric")) == FALSE) {
    stop("`shap1` and `shap2` must be only composed of numerical values")
  }
  # Check the type of the input on the expected values
  if (!is.numeric(ex_1) | !is.numeric(ex_2)) {
    stop("`ex_1` and `ex_2` must be numeric")
  }
  if (length(ex_1) > 1) {
    warning("`ex1` has a length greater than 1, only using first element")
    ex_1 <- ex_1[1]
  }
  if (length(ex_2) > 1) {
    warning("`ex2` has a length greater than 1, only using first element")
    ex_2 <- ex_2[1]
  }
  if ("array" %in% class(ex_1)) {
    ex_1 <- c(ex_1)
  }
  if ("array" %in% class(ex_2)) {
    ex_2 <- c(ex_2)
  }
  
  if (nrow(shap_1) != nrow(shap_2)) {
    stop("`shap_1` and `shap_2` (or their elements) must have the same number of rows")
  }
  if (sum(c(is.null(shap_1_names), is.null(shap_2_names))) == 1) {
    stop("You cannot specify only one of `shap_1_names` and `shap_2_names`. Please specify none or both.")
  }
  
  if ((is.null(shap_1_names) | is.null(shap_2_names)) & (min(dim(shap_1) == dim(shap_2)) == FALSE)) {
    stop("`shap1` and `shap2` must have the same dimensions, or you must supply `shap_1_names` and `shap_2_names`")
  } else if ((min(dim(shap_1) == dim(shap_2)) == FALSE) | !is.null(shap_1_names)) {
    shap_2_missing_names <- setdiff(shap_1_names, shap_2_names)
    shap_1_missing_names <- setdiff(shap_2_names, shap_1_names)
    colnames(shap_1) <- shap_1_names
    colnames(shap_2) <- shap_2_names
    if (length(shap_2_missing_names) > 0) {
      shap_2_missing <- matrix(0, nrow = nrow(shap_1), ncol = length(shap_2_missing_names)) %>%
        as.data.frame() %>%
        magrittr::set_colnames(shap_2_missing_names)
      shap_2 <- shap_2 %>%
        dplyr::bind_cols(shap_2_missing)
    }
    if (length(shap_1_missing_names > 0)) {
      shap_1_missing <- matrix(0, nrow = nrow(shap_2), ncol = length(shap_1_missing_names)) %>%
        as.data.frame() %>%
        magrittr::set_colnames(shap_1_missing_names)
      shap_1 <- shap_1 %>%
        dplyr::bind_cols(shap_1_missing)
    }
    # Make sure the columns are in the same order
    shap_2 <- shap_2 %>%
      dplyr::select(colnames(shap_1))
  }
  
  list(
    shap_1 = shap_1,
    shap_2 = shap_2,
    ex_1 = ex_1,
    ex_2 = ex_2
  )
}


# Global Variable Bindings so I can use unquoted variables without 
# package check warnings
globalVariables(
  c(
    ".",
    "avg_value",
    "bar_end",
    "bar_start",
    "covariate",
    "desc",
    "head",
    "installed.packages",
    "is_positive",
    "setNames",
    "shap_val",
    "value",
    "var_val",
    "variable"
  )
)