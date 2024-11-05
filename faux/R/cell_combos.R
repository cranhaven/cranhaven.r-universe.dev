#' Cell combos
#' 
#' Creates wide cell combination names, such as A1_B1, A2_B1, A1_B2, A2_B2.
#' 
#' @param factors A list of lists of named factor levels
#' @param dv name of dv column ("y") to be used if there are no factors
#' @param sep separator
#' 
#' @return a list
#' @keywords internal
cell_combos <- function(factors, dv = "y", sep = faux_options("sep")) {
  if (length(factors) == 0) {
    cells = dv
  } else {
    fnames <- lapply(factors, names)
    exp <- expand.grid(rev(fnames))
    cells <- apply(exp, 1, function(x) { 
      paste(rev(x), collapse = sep) 
    })
  }
  
  cells
}
