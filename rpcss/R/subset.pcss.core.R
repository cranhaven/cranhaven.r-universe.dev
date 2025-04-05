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

#' Fetch the names of individuals/genotypes in the core set generated from
#' \code{pcss.core} Output
#'
#' \code{subset.pcss.core} returns names of individuals/genotypes in the core
#' collection from \code{pcss.core} Output.
#'
#' Use \code{"size"} to return names of individuals/genotypes in the core
#' collection according to the threshold \code{size} criterion or use
#' \code{"variance"} to return names according to the variability threshold
#' criterion or use  \code{"logistic"} to return names according to inflection
#' point of rate of progress of cumulative variability retained identified by
#' logistic regression.
#'
#' @param x An object of class \code{pcss.core}.
#' @param criterion The core collection generation criterion. Either
#'   \code{"size"}, \code{"variance"}, or \code{"logistic"}. See
#'   \strong{Details}.
#' @param ... Unused.
#'
#' @return The names of individuals/genotypes in the core collection as a
#'   character vector.
#'
#' @seealso \code{\link[rpcss]{pcss.core}}
#'
#' @importFrom methods is
#' @exportS3Method rpcss::subset
#'
#' @examples
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Prepare example data
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' suppressPackageStartupMessages(library(EvaluateCore))
#'
#' # Get data from EvaluateCore
#'
#' data("cassava_EC", package = "EvaluateCore")
#' data = cbind(Genotypes = rownames(cassava_EC), cassava_EC)
#' quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW", "AVPW",
#'            "ARSR", "SRDM")
#' qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#' rownames(data) <- NULL
#'
#' # Convert qualitative data columns to factor
#' data[, qual] <- lapply(data[, qual], as.factor)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # With quantitative data
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out1 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = NULL, eigen.threshold = NULL, size = 0.2,
#'                   var.threshold = 0.75)
#'
#' # Core sets
#' out1$cores.info
#'
#' # Fetch genotype names of core set by size criterion
#' subset(x = out1, criterion = "size")
#'
#' # Fetch genotype names of core set by variance criterion
#' subset(x = out1, criterion = "variance")
#'
#' # Fetch genotype names of core set by logistic regression criterion
#' subset(x = out1, criterion = "logistic")
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
#'                   qualitative = qual, eigen.threshold = NULL,
#'                   size = 0.2, var.threshold = 0.75)
#'
#' # Core sets
#' out2$cores.info
#'
#' # Fetch genotype names of core set by size criterion
#' subset(x = out2, criterion = "size")
#'
#' # Fetch genotype names of core set by variance criterion
#' subset(x = out2, criterion = "variance")
#'
#' # Fetch genotype names of core set by logistic regression criterion
#' subset(x = out2, criterion = "logistic")
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (quantitative and qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out3 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = qual, eigen.threshold = NULL)
#'
#' # Core sets
#' out3$cores.info
#'
#' # Fetch genotype names of core set by size criterion
#' subset(x = out3, criterion = "size")
#'
#' # Fetch genotype names of core set by variance criterion
#' subset(x = out3, criterion = "variance")
#'
#' # Fetch genotype names of core set by logistic regression criterion
#' subset(x = out3, criterion = "logistic")
#'
subset.pcss.core <- function(x,
                             criterion = c("size", "variance", "logistic"),
                             ...) {

  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  criterion <- match.arg(criterion)

  gssdf <- x$variability.ret

  # By size specified ----

  if (criterion == "size") {
    size.sel <-
      x$cores.info[x$cores.info$Method == "By size specified", ]$Size

    subset <- gssdf[1:size.sel, ]$Id
  }

  # By threshold variance ----

  if (criterion == "variance") {
    var.sel <-
      x$cores.info[x$cores.info$Method == "By threshold variance", ]$Size

    subset <- gssdf[1:var.sel, ]$Id
  }

  # With logistic regression ----

  if (criterion == "logistic") {
    reg.sel <-
      x$cores.info[x$cores.info$Method == "By logistic regression", ]$Size

    subset <- gssdf[1:reg.sel, ]$Id
  }

  return(subset)

}
