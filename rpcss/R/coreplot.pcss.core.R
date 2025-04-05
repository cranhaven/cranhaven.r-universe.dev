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

#' Plot the cumulative variability retained by individuals/genotypes from
#' \code{pcss.core} Output
#'
#' \code{coreplot.pcss.core} generates plots of cumulative variability retained
#' by individuals/genotypes from \code{pcss.core} Output. The size of core
#' collection and the corresponding cumulative variance retained are highlighted
#' according to the criterion used.
#'
#' Use \code{"size"} to highlight core collection according to the threshold
#' \code{size} criterion or use \code{"variance"} to highlight core collection
#' according to the variability threshold criterion or use  \code{"logistic"} to
#' highlight core collection generated according to inflection point of rate of
#' progress of cumulative variability retained identified by logistic
#' regression.
#'
#' @param x An object of class \code{pcss.core}.
#' @param criterion The core collection generation criterion. Either
#'   \code{"size"}, \code{"variance"}, or \code{"logistic"}. See
#'   \strong{Details}.
#' @param ... Unused.
#'
#' @return A plot of cumulative variability retained by individuals/genotypes as
#'   a \code{ggplot} object. In case of \code{criterion = "logistic"}, a list
#'   with plots of cumulative variability retained by individuals/genotypes and
#'   rate of progress of cumulative contribution to variability. The size and
#'   variability retained by core collection are highlighted in each plot.
#'
#' @seealso \code{\link[rpcss]{pcss.core}}
#'
#' @import ggplot2
#' @export
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
#'
#' library(FactoMineR)
#' suppressPackageStartupMessages(library(factoextra))
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
#' # For core set constituted by size criterion
#' coreplot(x = out1, criterion = "size")
#'
#' # For core set constituted by variance criterion
#' coreplot(x = out1, criterion = "variance")
#'
#' # For core set constituted by logistic regression criterion
#' coreplot(x = out1, criterion = "logistic")
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
#'                   qualitative = qual, eigen.threshold = NULL,
#'                   size = 0.2, var.threshold = 0.75)
#'
#' # For core set constituted by size criterion
#' coreplot(x = out2, criterion = "size")
#'
#' # For core set constituted by variance criterion
#' coreplot(x = out2, criterion = "variance")
#'
#' # For core set constituted by logistic regression criterion
#' coreplot(x = out2, criterion = "logistic")
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (quantitative and qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out3 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = qual, eigen.threshold = NULL)
#'
#' # For core set constituted by size criterion
#' coreplot(x = out3, criterion = "size")
#'
#' # For core set constituted by variance criterion
#' coreplot(x = out3, criterion = "variance")
#'
#' # For core set constituted by logistic regression criterion
#' coreplot(x = out3, criterion = "logistic")
#'
coreplot <- function(x,
                     criterion = c("size", "variance", "logistic"),
                     ...) {
  UseMethod("coreplot")
}

#' @name coreplot
#' @method coreplot default
#' @export
coreplot.default <- function(x,
                             criterion = c("size", "variance", "logistic"),
                             ...) {

  coreplot.pcss.core(x,
                     criterion = c("size", "variance", "logistic"),
                     ...)
}

#' @name coreplot
#' @export
coreplot.pcss.core <- function(x,
                               criterion = c("size", "variance", "logistic"),
                               ...) {

  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  criterion <- match.arg(criterion)

  gssdf <- x$variability.ret

  N <- nrow(gssdf)

  propname <- "Proportion of selected individuals"

  # By size specified ----

  if (criterion == "size") {

    size.sel <-
      x$cores.info[x$cores.info$Method == "By size specified", ]$Size
    size.var <-
      x$cores.info[x$cores.info$Method == "By size specified", ]$VarRet

    size.segdf <- data.frame(x = c(-Inf, size.sel),
                             xend = c(size.sel, size.sel),
                             y = c(size.var, -Inf),
                             yend = c(size.var, size.var))
    size.segdf$label <- as.character(c(round(size.segdf[1, "y"], 2),
                                       size.segdf[2, "x"]))

    size.gssg <- ggplot(gssdf, aes(x = Rank, y = VarRet)) +
      geom_point() +
      geom_segment(data = size.segdf, aes(x = x, xend = xend,
                                          y = y, yend = yend),
                   colour = "red") +
      geom_text(data = size.segdf, aes(x = x, y = y, label = label),
                vjust = -0.5, hjust = -0.5, colour = "red") +
      scale_x_continuous(name = "Number of selected individuals",
                         sec.axis = sec_axis(transform = ~. / N,
                                             name = propname)) +
      ylab("Variability retained (%)") +
      theme_bw()

    return(size.gssg)
  }

  # By threshold variance ----

  if (criterion == "variance") {

    var.sel <-
      x$cores.info[x$cores.info$Method == "By threshold variance", ]$Size
    var.threshold <-
      x$cores.info[x$cores.info$Method == "By threshold variance", ]$VarRet

    var.segdf <- data.frame(x = c(-Inf, var.sel),
                            xend = c(var.sel, var.sel),
                            y = c(var.threshold, -Inf),
                            yend = c(var.threshold, var.threshold))
    var.segdf$label <- as.character(c(round(var.segdf[1, "y"], 2),
                                      var.segdf[2, "x"]))

    var.gssg <- ggplot(gssdf, aes(x = Rank, y = VarRet)) +
      geom_point() +
      geom_segment(data = var.segdf, aes(x = x, xend = xend,
                                         y = y, yend = yend),
                   colour = "red") +
      geom_text(data = var.segdf, aes(x = x, y = y, label = label),
                vjust = -0.5, hjust = -0.5, colour = "red") +
      scale_x_continuous(name = "Number of selected individuals",
                         sec.axis = sec_axis(transform = ~. / N,
                                             name = propname)) +
      ylab("Variability retained (%)") +
      theme_bw()

    return(var.gssg)
  }

  # With logistic regression ----

  if (criterion == "logistic") {

    reg.sel <-
      x$cores.info[x$cores.info$Method == "By logistic regression", ]$Size
    reg.var <-
      x$cores.info[x$cores.info$Method == "By logistic regression", ]$VarRet

    b <- attributes(x)$slope

    reg.segdf <- data.frame(x = c(-Inf, reg.sel),
                            xend = c(reg.sel, reg.sel),
                            y = c(reg.var, -Inf),
                            yend = c(reg.var, reg.var))
    reg.segdf$label <- as.character(c(round(reg.segdf[1, "y"], 2),
                                      reg.segdf[2, "x"]))

    reg.gssg <- ggplot(gssdf, aes(x = Rank, y = VarRet)) +
      geom_point() +
      geom_segment(data = reg.segdf, aes(x = x, xend = xend,
                                         y = y, yend = yend),
                   colour = "red") +
      geom_text(data = reg.segdf, aes(x = x, y = y, label = label),
                vjust = -0.5, hjust = -0.5, colour = "red") +
      scale_x_continuous(name = "Number of selected individuals",
                         sec.axis = sec_axis(transform = ~. / N,
                                             name = propname)) +
      ylab("Variability retained (%)") +
      theme_bw()

    dat <- data.frame(n = gssdf$Rank, y = gssdf$VarRet)
    dat$rate <- -b * dat$y * (100 - dat$y)

    reg.segdf2 <- data.frame(x = c(-Inf, reg.sel),
                             xend = c(reg.sel, reg.sel),
                             y = c(max(dat$rate), -Inf),
                             yend = c(max(dat$rate), max(dat$rate)))
    reg.segdf2$label <- as.character(c(round(reg.segdf2[1, "y"], 2),
                                       reg.segdf2[2, "x"]))

    reg.gssrateg <- ggplot(data = dat, aes(x = n, y = rate)) +
      geom_point() +
      geom_segment(data = reg.segdf2, aes(x = x, xend = xend,
                                          y = y, yend = yend),
                   colour = "red") +
      geom_text(data = reg.segdf2, aes(x = x, y = y, label = label),
                vjust = -0.5, hjust = -0.5, colour = "red") +
      scale_x_continuous(name = "Number of selected individuals",
                         sec.axis = sec_axis(transform = ~. / N,
                                             name = propname)) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      ylab("Rate of increase in variability retained (%)") +
      theme_bw()

    return(list(`Cumulative contribution` = reg.gssg,
                `Rate of cumulative contribution` = reg.gssrateg))
  }

}
