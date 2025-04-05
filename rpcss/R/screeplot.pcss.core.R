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

#' Plot Eigen values as a Scree Plot from \code{pcss.core} Output
#'
#' \code{screeplot.pcss.core} generates a scree plot of eigen values from the
#' output of \code{pcss.core}.
#'
#' @param x An object of class \code{pcss.core}.
#' @param ndim The number of eigen values to be plotted in the scree plot.
#' @param show.values If \code{TRUE}, the eigen values are shown in the plot as
#'   annotation labels. Default is \code{TRUE}.
#' @param ... Unused.
#'
#' @return The scree plot as a \code{ggplot} object.
#'
#' @seealso \code{\link[rpcss]{pcss.core}},
#'   \code{\link[factoextra]{fviz_screeplot}}
#'
#' @import ggplot2
#' @importFrom stats screeplot
#' @exportS3Method rpcss::screeplot
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
#' # Plot scree plot
#' screeplot(x = out1)
#'
#' # Plot biplot with factoextra
#' fviz_screeplot(out1$raw.out)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
#'                   qualitative = qual, eigen.threshold = NULL,
#'                   size = 0.2, var.threshold = 0.75)
#'
#' # Plot scree plot
#' screeplot(x = out2)
#'
#' # Plot biplot with factoextra
#' fviz_screeplot(out2$raw.out)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (quantitative and qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out3 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = qual, eigen.threshold = NULL)
#'
#' # Plot scree plot
#' screeplot(x = out3)
#'
#' # Plot biplot with factoextra
#' fviz_screeplot(out3$raw.out)
#'
#'
#'
screeplot.pcss.core <- function(x, ndim = NULL, show.values = TRUE, ...) {

  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  if (!is.null(ndim)) {
    # check if 'ndim' argument is integer vector of unit length
    if (!(as.integer(ndim) == ndim && length(ndim) == 1)) {
      stop('"ndim" should be a integer vector of unit length.')
    }

    # check if at least one dimension are plotted in ndim
    if (ndim < 1L) {
      stop('At least 1 dimension is to be specified in "ndim".')
    }

    # check if ndim is not greater than total dimensions
    total.ndim <- ncol(x$raw.out$svd$V)

    if (ndim > total.ndim) {
      warning('"ndim" is greater than the total number of dimensions.\n',
              paste('Using the total number of dimensions (',
                    total.ndim, ') as "ndim".', sep = ""))
      ndim <- total.ndim
    }

  }

  eigen.threshold <- x$eigen.threshold
  eig <- x$eigen$`Eigen value`

  eigen.threshold_label <- eigen.threshold
  if (nchar(eigen.threshold > 5)) {
    eigen.threshold_label <- round(eigen.threshold, 3)
  }

  K <- x$details[x$details$Detail == "Number of eigen values selected", "Value"]
  K <- as.numeric(K)

  # Plot eigen values ----
  eigdf <- data.frame(sl = seq_along(eig), eig = eig)
  eigdf$gp <- ifelse(eigdf$sl <= K, 1, 0)
  eigdf$gp <- as.factor(eigdf$gp)

  if (!is.null(ndim)) {
    if (nrow(eigdf) > ndim) {
      eigdf <- eigdf[1:ndim, ]
    }
  }

  eigg <- ggplot(eigdf) +
    geom_segment(aes(x = sl, y = 0, xend = sl, yend = eig),
                 linewidth = 2, colour = "gray20") +
    geom_line(aes(x = sl, y = eig)) +
    geom_point(aes(x = sl, y = eig, colour = gp), pch = 18, size = 3,
               show.legend = FALSE) +
    scale_colour_manual(values = c("gray", "red")) +
    geom_hline(yintercept = eigen.threshold, linetype = 2) +
    geom_label(x = length(eig), y = eigen.threshold, colour = "red",
               label = eigen.threshold_label) +
    xlab("Factors") +
    ylab("Eigen value") +
    theme_bw()

  if (show.values) {
    eigg <- eigg +
      geom_label(aes(x = sl, y = eig, label = round(eig, 3)),
                 vjust = 1, hjust = -0.2) +
      scale_x_continuous(breaks = eigdf$sl,
                         limits = c(1, length(eig) + 0.5)) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.05)))

  }

  return(eigg)
}
