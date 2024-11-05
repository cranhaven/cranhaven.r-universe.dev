#' Simulate missing data
#' 
#' Insert NA or another replacement value for some proportion of specified 
#' columns to simulate missing data. 
#'
#' @param data the tbl
#' @param prop the proportion of data to mess up
#' @param ... the columns to mess up (as a vector of column names or numbers)
#' @param replace the replacement value (defaults to NA)
#'
#' @return the messed up table
#' @export
#'
#' @examples
#' messy(iris, 0.1, "Species", replace = "NO SPECIES")
#' messy(iris, 0.5, 1:4)
messy <- function(data, prop = 0, ..., replace = NA) {
  if (any(prop > 1)) stop("proportions cannot be greater than 1")
  if (any(prop < 0)) stop("proportions cannot be less than 0")
  
  n <- nrow(data)
  cols <- getcols(data, ...)
  size <- floor(n*prop) %>% rep(length.out = length(cols))
  
  for (i in seq_along(cols)) {
    thecol <- cols[i]
    
    if (data[[thecol]] %>% is.factor()) { # add replace value to factor levels 
      new_levels <- data[[thecol]] %>% levels() %>% c(replace)
      levels(data[[thecol]]) <- new_levels
    }
    
    to_replace <- sample.int(n, size[i])
    data[[thecol]][to_replace] <- replace
  }
  
  data
}



