.getBoundRemember <- NULL
.getBound <- function(x, parent = parent.frame(2)) {
  ## nocov start
  if (!is.null(.getBoundRemember)) return(.getBoundRemember)
  bound <- do.call("c", lapply(ls(globalenv()), function(cur) {
    if (identical(parent[[cur]], x)) {
      return(cur)
    }
    return(NULL)
  }))
  if (length(bound) > 1) bound <- bound[1]
  if (length(bound) == 0) {
    bound <- do.call("c", lapply(ls(parent), function(cur) {
      if (identical(parent[[cur]], x)) {
        return(cur)
      }
      return(NULL)
    }))
    if (length(bound) > 1) bound <- bound[1]
    if (length(bound) == 0) {
      bound <- ""
    }
  }
  return(bound)
  ## nocov end
}

#' @inherit base::print
#' @return This returns invisibly the model variables object
#' @export
print.rxModelVars <- function(x, ...) {
  .bound <- .getBound(x, parent.frame(2))
  cat("rxode2 model variables (see str to see all variables)\n")
  .cur <- x$state
  if (length(.cur) > 0) {
    cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$state"), ": ", paste(.cur, collapse = ", "), "\n"))
  }
  .cur <- x$stateExtra
  if (length(.cur) > 0) {
    cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$stateExtra"), ": ", paste(.cur, collapse = ", "), "\n"))
  }
  .cur <- x$params
  if (length(.cur) > 0) {
    cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$params"), ": ", paste(.cur, collapse = ", "), "\n"))
  }
  .cur <- x$lhs
  if (length(.cur) > 0) {
    cat(paste0(crayon::yellow(.bound), crayon::blue$bold("$lhs"), ": ", paste(.cur, collapse = ", "), "\n"))
  }
  invisible(x)
}
