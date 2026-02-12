# Filename: summary.mosaSTRS.R
#
# Date: 13.07.2025; modified 30.12.2025
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
#' @title Summary a mosaSTRS object
#'
#' @description Summary-function for class \code{mosaSTRS}
#'
#' @param object an object inheriting from class \code{mosaSTRS},
#' representing the results of the function \code{mosallocSTRS()}. This
#' can also be a list of \code{mosaSTRS} objects.
#'
#' @param ... some methods for this generic require additional arguments.
#' None are used in this method.
#'
#' @method summary mosaSTRS
#'
#' @return Either a \code{summary.mosaSTRS} object for a \code{mosaSTRS}
#' object or a list of \code{summary.mosaSTRS} objects for a list of
#' \code{mosaSTRS} objects. A \code{summary.mosaSTRS} object is a list
#' containing the following components:
#' @returns \code{$vname} Name of \code{object}.
#' @returns \code{$sense}
#' Sense of optimization; \code{max precision} or \code{min_cost}.
#' @returns \code{$method}
#' The method used weighted sum scalarization (WSS) or weighted Chebyshev
#' minimization (WCM).
#' @returns \code{$objout}
#' A data frame corresponding to the objectives, including the values, the
#' sensitivity, the weights and the RSE.
#' @returns \code{$precision}
#' A data frame corresponding to the precision constraints.
#' @returns \code{$cost}
#' A data frame corresponding to the cost constraints.
#' @returns \code{$n_opt}
#' A vector of optimal sample sizes w.r.t the \code{weights}.
#'
#' @export

summary.mosaSTRS <- function(object, ...) {
  vname <- substitute(object)
  out <- object#[-length(object)]


  if (inherits(out[[1]], "mosaSTRS")) {
    for (i in 1:length(out)) {
      out[[i]]$vname <- paste0(vname, "[[", i, "]]")
    }
    if (out[[1]]$output_mosalloc$Ecosolver$Ecoredcodes[1] == 1) {
      stop("\n The sample allocation problem is infeasible!")
    }
    lapply(out, function(iout) {
      o <- sumMosaSTRSObj(iout)
      class(o) <- "summary.mosaSTRS"
      o
    })
  } else {
    out$vname <- vname
    if (out$output_mosalloc$Ecosolver$Ecoredcodes[1] == 1) {
      stop("\n The sample allocation problem is infeasible!")
    }
    o <- sumMosaSTRSObj(out)
    class(o) <- "summary.mosaSTRS"
    o
  }
}
