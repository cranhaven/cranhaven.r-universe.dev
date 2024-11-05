#' Fix name labels
#' 
#' Fixes if a factor list does not have named levels or has special characters in the names
#' 
#' @param x the vector or list to fix
#' @param pattern regex pattern to replace; defaults to non-word characters and the value of faux_options("sep") (default = _)
#' @param replacement the character to replace; defaults to . (or _ if faux_options("sep") == ".")
#' 
#' @return a named list with fixed names
#' @export
#' 
#' @examples 
#' source <- list("full.stop", " space ", "under_score", "plus+", "dash-", "tab\t", "line\nbreak")
#' fix_name_labels(source)
#' 
fix_name_labels <- function(x, pattern = NA, replacement = ".") {
  if (!is.list(x) & !is.vector(x) & !is.factor(x)) 
    stop("x must be a vector or list")

  if (is.null(names(x))) { names(x) <- x }
  nm <- names(x)
  
  if (!is.null(pattern)) {
    if (is.na(pattern)) {
      sep_pat <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", faux_options("sep"))
      pattern <- paste0("(\\W|", sep_pat, ")")
    }
    # set replacement to _ if the separator is a full stop
    if (faux_options("sep") == ".") {
      replacement = "_"
    }
    
    # replace non-word characters and underscores with full stops
    names(x) <- gsub(pattern = pattern, replacement = replacement, x = nm) 
  }
  as.list(x)
}
