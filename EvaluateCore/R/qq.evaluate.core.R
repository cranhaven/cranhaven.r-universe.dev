### This file is part of 'EvaluateCore' package for R.

### Copyright (C) 2018-2022, ICAR-NBPGR.
#
# EvaluateCore is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# EvaluateCore is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/


#' Quantile-Quantile Plots
#'
#' Plot Quantile-Quantile (QQ) plots
#' \insertCite{wilk_probability_1968}{EvaluateCore} to graphically compare the
#' probability distributions of quantitative traits between entire collection
#' (EC) and core set (CS).
#'
#' @inheritParams snk.evaluate.core
#'
#' @return A list with the \code{ggplot} objects of QQ plots of CS vs EC for
#'   each trait specified as \code{quantitative}.
#'
#' @seealso \code{\link[stats:qqnorm]{qqplot}}
#'
#' @import ggplot2
#' @importFrom stats qqplot
#' @export
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#'
#' data("cassava_CC")
#' data("cassava_EC")
#'
#' ec <- cbind(genotypes = rownames(cassava_EC), cassava_EC)
#' ec$genotypes <- as.character(ec$genotypes)
#' rownames(ec) <- NULL
#'
#' core <- rownames(cassava_CC)
#'
#' quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW", "AVPW",
#'            "ARSR", "SRDM")
#' qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#'
#' ec[, qual] <- lapply(ec[, qual],
#'                      function(x) factor(as.factor(x)))
#'
#' qq.evaluate.core(data = ec, names = "genotypes",
#'                  quantitative = quant, selected = core)
#'
#'
qq.evaluate.core <- function(data, names, quantitative, selected) {
  # Checks
  checks.evaluate.core(data = data, names = names,
                       quantitative = quantitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  dataf <- data[, c(names, quantitative)]

  datafcore <- dataf[dataf[, names] %in% selected, ]

  dataf$`[Type]` <- "EC"
  datafcore$`[Type]` <- "CS"

  dataf <- rbind(dataf, datafcore)
  rm(datafcore)

  outlist <- vector(mode = "list", length = length(quantitative))
  names(outlist) <- quantitative

  for (i in seq_along(quantitative)) {
    # Create the quantile-quantile data table
    qqdf <- qqplot(x = dataf[dataf$`[Type]` == "CS", quantitative[i]],
                   y = dataf[dataf$`[Type]` == "EC", quantitative[i]],
                   plot.it = FALSE)
    qqdf <- as.data.frame(qqdf)

    # Set the x and y limits
    xylim <- range(c(qqdf$x, qqdf$y))

    # Generate the QQ plot
    outlist[[i]] <- ggplot(qqdf, aes(x = x, y = y)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      coord_fixed(ratio = 1, xlim = xylim, ylim = xylim) +
      xlab("Core Set") +
      ylab("Entire Collection") +
      ggtitle(quantitative[i]) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black"))

    rm(xylim, qqdf)
  }


  return(outlist)

}
