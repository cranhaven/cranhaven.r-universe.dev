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


#' Frequency Distribution Histogram
#'
#' Plot stacked frequency distribution histogram to graphically compare the
#' probability distributions of traits between entire collection (EC) and core
#' set (CS).
#'
#' @inheritParams snk.evaluate.core
#' @inheritParams chisquare.evaluate.core
#' @param highlight Individual names to be highlighted as a character vector.
#' @param include.highlight If \code{TRUE}, the highlighted individuals are
#'   included in the frequency distribution histogram. Default is \code{TRUE}.
#' @param highlight.se Optional data frame of standard errors for the
#'   individuals specified in \code{highlight}. It should have the same column
#'   names as in \code{data}.
#' @param highlight.col The colour(s) to be used to highlighting individuals in
#'   the plot as a character vector of the same length as \code{highlight}. Must
#'   be valid colour values in R (named colours, hexadecimal representation,
#'   index of colours [\code{1:8}] in default R \code{palette()} etc.).
#'
#' @return A list with the \code{ggplot} objects of stacked frequency
#'   distribution histograms plots for each trait specified as
#'   \code{quantitative} and \code{qualitative}.
#'
#' @seealso \code{\link[graphics]{hist}}, \code{\link[ggplot2]{geom_histogram}}
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices col2rgb
#' @importFrom grDevices nclass.Sturges
#' @importFrom grDevices nclass.scott
#' @importFrom stats dnorm
#' @importFrom stats na.omit
#' @export
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
#' \donttest{
#' freqdist.evaluate.core(data = ec, names = "genotypes",
#'                        quantitative = quant, qualitative = qual,
#'                        selected = core)
#'
#' checks <- c("TMe-1199", "TMe-1957", "TMe-3596", "TMe-3392")
#'
#' freqdist.evaluate.core(data = ec, names = "genotypes",
#'                        quantitative = quant, qualitative = qual,
#'                        selected = core,
#'                        highlight = checks, highlight.col = "red")
#'
#' quant.se <- data.frame(genotypes = checks,
#'                        NMSR = c(0.107, 0.099, 0.106, 0.062),
#'                        TTRN = c(0.081, 0.072, 0.057, 0.049),
#'                        TFWSR = c(0.089, 0.031, 0.092, 0.097),
#'                        TTRW = c(0.064, 0.031, 0.071, 0.071),
#'                        TFWSS = c(0.106, 0.071, 0.121, 0.066),
#'                        TTSW = c(0.084, 0.045, 0.066, 0.054),
#'                        TTPW = c(0.098, 0.052, 0.111, 0.082),
#'                        AVPW = c(0.074, 0.038, 0.054, 0.061),
#'                        ARSR = c(0.104, 0.019, 0.204, 0.044),
#'                        SRDM = c(0.078, 0.138, 0.076, 0.079))
#'
#' freqdist.evaluate.core(data = ec, names = "genotypes",
#'                        quantitative = quant,
#'                        selected = core,
#'                        highlight = checks, highlight.col = "red",
#'                        highlight.se = quant.se)
#' }
#'
freqdist.evaluate.core <- function(data, names, quantitative, qualitative,
                                   selected, highlight = NULL,
                                   include.highlight = TRUE,
                                   highlight.se = NULL,
                                   highlight.col = "red") {

  if (missing(quantitative)) {
    quantitative <- NULL
  }

  if (missing(qualitative)) {
    qualitative <- NULL
  }

  # Checks
  checks.evaluate.core(data = data, names = names,
                       quantitative = quantitative,
                       qualitative = qualitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  if (!is.null(highlight)) {
    if (!is.character(highlight)) {
      stop('"highlight" should be a character vector')
    }
    # highlight are present in treatment levels
    if (FALSE %in% c(highlight %in% data[, names])) {
      miss <- paste(highlight[!(highlight %in% data[, names])], collapse = ", ")
      stop(paste('Following individual(s) specified in "highlight"',
                 'are not present in', names, 'column of "data":\n',
                 paste(miss, collapse = ", ")))
    }
  }

  if (!is.null(highlight) & !all(iscolour(highlight.col))) {
    stop('"highlight.col" specifies invalid colour(s)')
  }

  if (!is.null(highlight) & length(highlight.col) != 1) {
    if (length(highlight.col) != length(highlight)) {
      stop('"highlight" and "highlight.col" are of unequal lengths')
    }
  }

  if (!is.null(highlight) & !is.null(highlight.se)) {
    # check if 'data' is a data frame object
    if (!is.data.frame(highlight.se)) {
      stop('"highlight.se" should be a data frame object')
    }

    if (any(c("tbl_dataf", "tbl") %in% class(highlight.se))) {
      warning('"highlight.se" is of type tibble\nCoercing to data frame')
      highlight.se <- as.data.frame(highlight.se)
    }

    if (dim(highlight.se)[1] != length(highlight)) {
      stop('"highlight.se" is of incorrect dimensions')
    }

    # check if 'names' column is present in 'data'
    if (!(names %in% colnames(highlight.se))) {
      stop(paste('Column ', names,
                 ' specified as the "names" column is not present in "highlight.se"',
                 sep = ""))
    }

    # check if 'quantitative' columns are present in 'highlight.se'
    if (!is.null(quantitative)) {
      if (FALSE %in% (quantitative %in% colnames(highlight.se)))  {
        stop(paste('The following column(s) specified in "quantitative" not present in "highlight.se":\n',
                   paste(quantitative[!(quantitative %in% colnames(highlight.se))],
                         collapse = ", "),
                   sep = ""))
      }
    }

    # check if 'qualitative' columns are present in 'highlight.se'
    if (!is.null(qualitative)) {
      if (FALSE %in% (qualitative %in% colnames(highlight.se)))  {
        stop(paste('The following column(s) specified in "qualitative" not present in "highlight.se":\n',
                   paste(qualitative[!(qualitative %in% colnames(highlight.se))],
                         collapse = ", "),
                   sep = ""))
      }
    }

    # check if names to be highlighted are present in highlight.se
    if (!all(highlight %in% highlight.se[, names])) {
      miss <- paste(highlight[!(highlight %in% highlight.se[, names])], collapse = ", ")
      stop(paste('Following individual(s) specified in "highlight"',
                 'are not present in', names, 'column of "highlight.se":\n',
                 paste(miss, collapse = ", ")))
    }
  }

  dataf <- data[, c(names, quantitative, qualitative)]

  datafcore <- dataf[dataf[, names] %in% selected, ]

  dataf$`[Type]` <- "EC"
  datafcore$`[Type]` <- "CS"

  dataf <- rbind(dataf, datafcore)
  rm(datafcore)

  dataf[, qualitative] <- lapply(dataf[, qualitative],
                                 function(x) as.numeric(as.factor(x)))
  traits <- c(quantitative, qualitative)

  outlist <- vector(mode = "list", length = length(traits))
  names(outlist) <- traits

  traits2 <- paste("`", traits, "`", sep = "")

  dataf$`[Type]` <- factor(dataf$`[Type]`)
  dataf$`[Type]` <- factor(dataf$`[Type]`, levels(dataf$`[Type]`)[c(2, 1)])

  # Fetch highlights
  if (!is.null(highlight)) {
    datah <- unique(dataf[dataf[, names] %in% highlight, c(names, traits)])
    datah[, names] <- as.factor(datah[, names])
  }

  # Remove highlights
  if (!is.null(highlight) & !include.highlight) {
    dataf <- dataf[!(dataf[, names] %in% highlight), ]
  }

  for (i in seq_along(traits)) {

    # Generate the freq dist histogram
    bw <- binw(dataf[, traits[i]], "sturges")
    NN <- length(dataf[, traits[i]])

    G1 <- ggplot(dataf, aes_string(x = traits2[i], fill = "`[Type]`")) +
      geom_histogram(position = "stack", alpha = 0.5,
                     colour = "black", binwidth = bw) +
      scale_fill_manual(values = c("lemonchiffon", "grey20")) +
      ylab("Frequency") +
      xlab(traits[i]) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black"))

    # plot normal distribution curve if quantitative
    if (traits[i] %in% quantitative) {
      G1 <- G1 +
        stat_function(geom = "line", fun = function(x, mean, sd, n, bw) {
          dnorm(x = x, mean = mean, sd = sd) * n * bw},
          args = list(mean = mean(data[, traits[i]], na.rm = TRUE),
                      sd = sd(data[, traits[i]], na.rm = TRUE),
                      n = NN, bw = bw), colour = "blue")
    }


    # if (traits[i] %in% quantitative) {
    #   G1 <- G1 +
    #     scale_x_continuous(limits = c((min(dataf[, traits[i]],
    #                                        na.rm = TRUE)),
    #                                   (max(dataf[, traits[i]],
    #                                        na.rm = TRUE))))
    # }
    #
    # if (traits[i] %in% qualitative) {
    #   G1 <- G1 +
    #     scale_x_continuous(limits = c((min(dataf[, traits[i]],
    #                                        na.rm = TRUE)) - 1,
    #                                   (max(dataf[, traits[i]],
    #                                        na.rm = TRUE)) + 1))
    # }

    if (!is.null(highlight)) {

      sedf <- dataf[dataf[, names] %in% highlight, c(names, traits[i])]

      if (!is.null(highlight) & !is.null(highlight.se)) {

        sedf <- highlight.se[, c(names, traits[i])]
        names(sedf)[2] <- "se"
        sedf <- merge(datah[, c(names, traits[i])], sedf, by = names)
        sedf$lower <- sedf[, traits[i]] - sedf$se
        sedf$upper <- sedf[, traits[i]] + sedf$se

      }

      G2 <- ggplot(sedf, aes_string(y = names, x = traits2[i])) +
        geom_point(colour = highlight.col) +
        labs(x = NULL, y = NULL) +
        theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        theme(legend.position = "none") +
        theme(plot.margin = unit(c(0.25, 0.1, 0, 0.25), "cm"),
              axis.text = element_text(colour = "black"))

      # if (traits[i] %in% quantitative) {
      #   G2 <- G2 +
      #     scale_x_continuous(limits = c((min(dataf[, traits[i]],
      #                                        na.rm = TRUE)),
      #                                   (max(dataf[, traits[i]],
      #                                        na.rm = TRUE))))
      # }
      #
      # if (traits[i] %in% qualitative) {
      #   G2 <- G2 +
      #     scale_x_continuous(limits = c((min(dataf[, traits[i]],
      #                                        na.rm = TRUE)) - 1,
      #                                   (max(dataf[, traits[i]],
      #                                        na.rm = TRUE)) + 1))
      # }

      if (!is.null(highlight) & !is.null(highlight.se)) {

        G2 <- G2 +
          geom_errorbar(aes(xmin = lower, xmax = upper),
                        colour = highlight.col,
                        width  = 0.25)
      }

      legind <- grep("guide-box", ggplotGrob(G1)$grobs)
      leg <- ggplotGrob(G1)$grobs[[legind]]
      G1 <- G1 + theme(legend.position = "none")

      G <- rbind(ggplotGrob(G2), ggplotGrob(G1), size = "max")
      G <- resize_heights(G, c(1, 3))
      G <- grid.arrange(G, leg, nrow = 1, widths = c(10, 1))

      outlist[[i]] <- G

    } else  {

      outlist[[i]] <- G1

    }
  }

  return(outlist)

}


binw <- function(x, method = c("fd", "scott", "sturges")) {
  method <- match.arg(method)

  if (method == "fd") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.FD(na.omit(x)),
                   min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.FD(na.omit(x)),
             min.n = 1, right = TRUE)[1]
  }
  if (method == "scott") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.scott(na.omit(x)),
                   min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.scott(na.omit(x)),
             min.n = 1, right = TRUE)[1]
  }
  if (method == "sturges") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.Sturges(na.omit(x)),
                   min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.Sturges(na.omit(x)),
             min.n = 1, right = TRUE)[1]
  }
  return(bw)
}

if (getRversion() >= "4.0.0")  {
  resize_heights <- function(g, heights = rep(1, length(idpanels))) {
    idpanels <- unique(g$layout[grepl("panel", g$layout$name), "t"])
    g$heights <- grid::unit(g$heights, "null")
    g$heights[idpanels] <- grid::unit(do.call(grid::unit,
                                              list(heights, 'null')), "null")
    g
  }
} else {
  unit.list <- getFromNamespace("unit.list", "grid")

  resize_heights <- function(g, heights = rep(1, length(idpanels))) {
    idpanels <- unique(g$layout[grepl("panel", g$layout$name), "t"])
    g$heights <- unit.list(g$heights)
    hunits <- lapply(heights, unit, "null")
    class(hunits) <- class(g$heights[idpanels])
    g$heights[idpanels] <- hunits
    g
  }
}

iscolour <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

# add to yml
