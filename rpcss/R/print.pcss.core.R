### This file is part of 'rpcss' package for R.

### Copyright (C) 2024-2025, ICAR-NBPGR.
#
# rpcss is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# rpcss is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/

#' Prints summary of \code{pcss.core} object
#'
#' \code{pcss.core} prints to console the summary of an object of class
#' \code{pcss.core} including the dimensionality reduction method used, the
#' basic details including parameters and the information on the core sets that
#' can be constituted.
#'
#' @param x An object of class \code{pcss.core}.
#' @param ... Unused.
#' @seealso \code{\link[rpcss]{pcss.core}}
#'
#' @return The argument x, invisibly as for all \code{\link[base]{print}}
#'   methods.
#'
#' @export
print.pcss.core <- function(x, ...) {

  cat("\nMethod\n")
  cat("========================\n")

  print(attr(x, "method"))

  cat("\nDetails\n")
  cat("========================\n")

  print(x$details)

  cat("\nCore sets\n")
  cat("=========================\n")

  print(x$cores.info)

}
