#' @title Utilities for Scheduling Functions to Execute Later with Event Loops
#' @description Executes arbitrary R or C functions some time after the current
#'  time, after the R execution stack has emptied. The functions are scheduled
#'  in an event loop. This is a derived work from the 'later' package aiming
#'  to reduce the number of dependencies.
#' @useDynLib later2, .registration=TRUE
"_PACKAGE"
