#'Returns string w/o leading whitespace
#'@param x a string to trim
trim.leading <- function (x)  sub("^\\s+", "", x)

#'Returns string w/o trailing whitespace
#'@param x a string to trim
trim.trailing <- function (x) sub("\\s+$", "", x)

#'Returns string w/o leading or trailing whitespace
#'@param x a string to trim
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' function loading results in global environment
#' @param pos defaults to 1 which equals an assingment to global environment
#' @param what variable to assign to
#' @param value value to assign
assign_to_global <- function(pos=1, what, value){
  assign(what, value, envir=as.environment(pos) )
}
