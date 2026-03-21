#' @details 
#' 
#' `na.true` and `na.false` replace missing values with `TRUE` and `FALSE` 
#' respectively.  
#' 
#' @examples 
#'   na.true( c(TRUE, NA_logical, FALSE) )   # T T F
#'   na.false( c(TRUE, NA_logical, FALSE) )  # T F F
#' 
#' @examples 
#'  
#'            
#' @rdname impute-constant
#' @aliases na.true
#' @export

na.true <- function(.x) { 
  na.replace(.x,TRUE)
}

#' @rdname impute-constant
#' @aliases na.true
#' @export

na.false <- function(.x) { 
  na.replace(.x,FALSE)
}
