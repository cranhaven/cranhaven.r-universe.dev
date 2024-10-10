#' @importFrom rlang .data
#' @importFrom utils globalVariables
utils::globalVariables(".") # This is needed to avoid R CMD check NOTEs
pkgEnv <- new.env(parent=emptyenv())