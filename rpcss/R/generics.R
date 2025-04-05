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

#' Generic function for \code{coreplot.pcss.core}
#'
#' @param x An object of class \code{pcss.core}.
#'
#' @param ...  Unused.
#'
#' @export
#' @keywords internal
#'
coreplot <- function(x, ...) {
  UseMethod("coreplot")
}

#' Generic function for \code{contrib.pcss.core}
#'
#' @param x An object of class \code{pcss.core}.
#'
#' @param ...  Unused.
#'
#' @export
#' @keywords internal
#'
contrib <- function(x, ...) {
  UseMethod("contrib")
}
