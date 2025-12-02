#' Code_extr
#' @description
#' This function extracts the numerical value from NUTS-level strings.
#' @param level  A character vector representing NUTS-level codes (e.g., `c("NUTS2", "NUTS3")`).
#'
#' @return A sorted numeric vector containing the extracted NUTS levels.
#'
code_extr <- function(level) {
  code <- vector()
  for (i in level) {
    if (nchar(i) == 5) {
      code <- c(code, as.numeric(substr(i, 5, 5)))
    } else {
      code <- c(code, 4)
    }
  }
  return(sort(code))
}
