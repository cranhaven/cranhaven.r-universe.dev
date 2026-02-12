# Filename: print.summary.mosaSTRS.R
#
# Date: 14.07.2025; Updated: 21.08.2025
# Author: Felix Willems
# Contact: mail.willemsf+MOSAlloc@gmail.com 
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
# Licensing: GPL-3.0-or-later
#
# Please report any bugs or unexpected behavior to
# mail.willemsf+MOSAlloc@gmail.com
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
#
#---------------------------------------------------------------------------
#' @title Print a summary.mosaSTRS object
#'
#' @description Print-function for class \code{summary.mosaSTRS}.
#'
#' @param x an object inheriting from class \code{summary.mosaSTRS},
#' representing the results of the function \code{mosallocSTRS()}
#'
#' @param ... some methods for this generic require additional arguments.
#' None are used in this method.
#'
#' @method print summary.mosaSTRS
#'
#' @return Invisibly returns \code{x}.
#'
#' @export

print.summary.mosaSTRS <- function(x, ...) {

  # get method name
  if (x$method == "WSS") {
    method_name <- "WSS (Weighted Sum Scalarization)"
  }
  if (x$method == "WCM") {
    method_name <- "WCM (Weighted Chebyshev Minimization)"
  }

  # print summary
  cat(paste0("\n-------------------------------------",
             "-------------------------------------\n",
             "                     Summary of a ",
             "mosaSTRS object             ",
             "\n-------------------------------------",
             "-------------------------------------\n"))
  cat(" Sense of optimization: ", x$sense, "\n",
      "Method used          : ", method_name, "\n")
  cat("\n To see the problem matrices and vectors, type:",
      paste0(x$vname, "$problem_components"), "\n")
  cat("\n Objectives and preference weighting:\n")
  print(x$objout)
  cat("\n Precision constraints:\n")
  print(x$precision)
  cat("\n Cost constraints:\n")
  print(x$cost)

  cat("\n The corresponding optimal sample sizes are:\n")
  print(x$n_opt)
  cat(paste0("-------------------------------------",
             "-------------------------------------\n"))
  invisible(x)
}