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

#' Generate Biplots from \code{pcss.core} Output
#'
#' \code{biplot.pcss.core} generates biplots of scores of genotypes with or
#' without vectors for traits from the output of \code{pcss.core}.
#'
#' Use \code{"size"} to highlight core collection according to the threshold
#' \code{size} criterion or use \code{"variance"} to highlight core collection
#' according to the variability threshold criterion or use  \code{"logistic"} to
#' highlight core collection generated according to inflection point of rate of
#' progress of cumulative variability retained identified by logistic
#' regression. Use \code{"none"} to not highlight any accessions.
#'
#' @param x An object of class \code{pcss.core}.
#' @param ndim The number of dimensions for which biplots have to plotted.
#' @param highlight.core The core collection to be highlighted. Either
#'   \code{"size"}, \code{"variance"}, \code{"logistic"}, or \code{"none"}. See
#'   \strong{Details}.
#' @param show.traits Which kind of the traits to be shown in the biplot. Either
#'   \code{"all"}, \code{"none"}, \code{"quantitative"} or \code{"qualitative"}.
#' @param qual.scale A scale factor to be applied to qualitative trait
#'   coordinates plotted in biplot.
#' @param quant.scale A scale factor to be applied to quantitative trait
#'   coordinates plotted in biplot.
#' @param point.alpha Alpha transparency value for biplot points.
#' @param segment.alpha Alpha transparency value for biplot segments.
#' @param ... Unused.
#'
#' @return A list of biplots as \code{ggplot} objects.
#'
#' @seealso \code{\link[rpcss]{pcss.core}}, \code{\link[FactoMineR]{plot.PCA}},
#'   \code{\link[FactoMineR]{plot.MCA}}, \code{\link[FactoMineR]{plot.FAMD}},
#'   \code{\link[factoextra]{fviz_pca}}, \code{\link[factoextra]{fviz_mca}},
#'   \code{\link[factoextra]{fviz_famd}}
#'
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats biplot
#' @importFrom utils combn
#' @import ggplot2
#' @exportS3Method rpcss::biplot
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
#'\donttest{
#' # Plot biplot
#' biplot(out1, ndim = 3, highlight.core = "size", quant.scale = 3,
#'        point.alpha = 0.5)
#'
#' # Plot biplot with FactoMineR
#' plot(out1$raw.out, choix=c("ind"), label  = "none", axes = c(1, 2))
#'
#' plot(out1$raw.out, choix=c("ind"), label  = "none", axes = c(1, 3))
#'
#' plot(out1$raw.out, choix=c("ind"), label  = "none", axes = c(2, 3))
#'
#' # Plot biplot with factoextra
#' fviz_pca_biplot(out1$raw.out, geom.ind = "point", axes = c(1, 2))
#'
#' fviz_pca_biplot(out1$raw.out, geom.ind = "point", axes = c(1, 3))
#'
#' fviz_pca_biplot(out1$raw.out, geom.ind = "point", axes = c(2, 3))
#'}
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
#'                   qualitative = qual, eigen.threshold = NULL,
#'                   size = 0.2, var.threshold = 0.75)
#'
#'\donttest{
#' # Plot biplot
#' biplot(out2, ndim = 3, highlight.core = "size", qual.scale = 1,
#'        point.alpha = 0.5)
#'
#' # Plot biplot with FactoMineR
#' plot(out2$raw.out, choix=c("ind"), label  = "none", axes = c(1, 2))
#'
#' plot(out2$raw.out, choix=c("ind"), label  = "none", axes = c(1, 3))
#'
#' plot(out2$raw.out, choix=c("ind"), label  = "none", axes = c(2, 3))
#'
#' # Plot biplot with factoextra
#' fviz_mca_biplot(out2$raw.out, geom.ind = "point", axes = c(1, 2))
#'
#' fviz_mca_biplot(out2$raw.out, geom.ind = "point", axes = c(1, 3))
#'
#' fviz_mca_biplot(out2$raw.out, geom.ind = "point", axes = c(2, 3))
#'}
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (quantitative and qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out3 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = qual, eigen.threshold = NULL)
#'
#'\donttest{
#' # Plot biplot
#' biplot(out3, ndim = 3, highlight.core = "size",
#'        quant.scale = 3, qual.scale = 1,
#'        point.alpha = 0.5)
#'
#' # Plot biplot with FactoMineR
#' plot(out3$raw.out, choix=c("ind"), label  = "none", axes = c(1, 2))
#'
#' plot(out3$raw.out, choix=c("ind"), label  = "none", axes = c(1, 3))
#'
#' plot(out3$raw.out, choix=c("ind"), label  = "none", axes = c(2, 3))
#'
#' # Plot biplot with factoextra
#'
#' # Fix rownames
#' row.names(out3$raw.out$quali.var$coord) <-
#'   unlist(lapply(seq_along(data[, qual]),
#'                 function(i) paste(qual[i],
#'                                   levels(data[, qual[i]]), sep = "_")))
#'
#' fviz_famd_ind(out3$raw.out, geom = "point", axes = c(1, 2))
#'
#' fviz_famd_ind(out3$raw.out, geom = "point", axes = c(1, 3))
#'
#' fviz_famd_ind(out3$raw.out, geom = "point", axes = c(2, 3))
#'}
#'
biplot.pcss.core <- function(x,
                             ndim = 3, # at least 2
                             highlight.core = c("size", "variance",
                                                "logistic", "none"),
                             show.traits = c("all", "none",
                                             "quantitative", "qualitative"),
                             qual.scale = 1,
                             quant.scale = 1,
                             point.alpha = 0.8,
                             segment.alpha = 0.8, ...) {

  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  # check if 'ndim' argument is integer vector of unit length
  if (!(as.integer(ndim) == ndim && length(ndim) == 1)) {
    stop('"ndim" should be a integer vector of unit length.')
  }

  # check if at least two dimensions are plotted in ndim
  if (ndim < 2) {
    stop('At least 2 dimensions are to be specified in "ndim".')
  }

  method <- attr(x, "method")

  # check if ndim is not greater than total dimensions
  total.ndim <- ncol(x$raw.out$ind$coord)

  if (ndim > total.ndim) {
    warning('"ndim" is greater than the total number of dimensions.\n',
            paste('Using the total number of dimensions (',
                  total.ndim, ') as "ndim".', sep = ""))
    ndim <- total.ndim
  }

  # check show.traits argument
  show.traits <- match.arg(show.traits)

  if (method == "MCA" || method == "FAMD") {
    # check qual.scale argument is numeric vector of unit length
    if (!(is.numeric(qual.scale) && length(qual.scale) == 1)) {
      stop('"qual.scale" should be a numeric vector of unit length.')
    }
  }

  if (method == "PCA" || method == "FAMD") {
    # check quant.scale argument is numeric vector of unit length
    if (!(is.numeric(quant.scale) && length(quant.scale) == 1)) {
      stop('"quanti.scale" should be a numeric vector of unit length.')
    }
  }

  # check point.alpha argument is numeric vector of unit length
  if (!(is.numeric(point.alpha) && length(point.alpha) == 1)) {
    stop('"point.alpha" should be a numeric vector of unit length.')
  }

  # check if 'point.alpha' is a value between 0 and 1
  if (point.alpha <= 0 || point.alpha >= 1) {
    stop('"point.alpha" should be a value between 0 and 1.')
  }

  # check segment.alpha argument is numeric vector of unit length
  if (!(is.numeric(segment.alpha) && length(segment.alpha) == 1)) {
    stop('"segment.alpha" should be a numeric vector of unit length.')
  }

  # check if 'segment.alpha' is a value between 0 and 1
  if (segment.alpha <= 0 || segment.alpha >= 1) {
    stop('"segment.alpha" should be a value between 0 and 1.')
  }

  # Check 'highlight.core'
  highlight.core <- match.arg(highlight.core)

  # Get coordinates for biplot ----

  if (method == "PCA") {

    pca_out <- x$raw.out

    ## Coordinates for biplot
    ind_coord <- pca_out$ind$coord
    ind_coord <- ind_coord[, 1:ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    quant_coord <- pca_out$var$coord
    quant_coord <- quant_coord[, 1:ndim]
    colnames(quant_coord) <- gsub("Dim.", "Dim ", colnames(quant_coord))
    quant_coord <- quant_coord * quant.scale
  }

  if (method == "MCA") {

    mca_out <- x$raw.out

    ## Coordinates for biplot
    ind_coord <- mca_out$ind$coord
    ind_coord <- ind_coord[, 1:ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    qual_coord <- mca_out$var$coord
    qual_coord <- qual_coord[, 1:ndim]
    colnames(qual_coord) <- gsub("Dim.", "Dim ", colnames(qual_coord))
    qual_coord <- qual_coord * qual.scale
  }

  if (method == "FAMD") {

    famd_out <- x$raw.out

    ## Coordinates for biplot
    ind_coord <- famd_out$ind$coord
    ind_coord <- ind_coord[, 1:ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    quant_coord <- famd_out$quanti.var$coord
    quant_coord <- quant_coord[, 1:ndim]
    colnames(quant_coord) <- gsub("Dim.", "Dim ", colnames(quant_coord))
    quant_coord <- quant_coord * quant.scale

    qual_coord <- famd_out$quali.var$coord
    qual_coord <- qual_coord[, 1:ndim]
    colnames(qual_coord) <- gsub("Dim.", "Dim ", colnames(qual_coord))
    qual_coord <- qual_coord * qual.scale

    quali.levels <- attr(x, "quali.levels")
    qual_levels <-
      lapply(seq_along(quali.levels), function(i) {
        data.frame(qual = names(quali.levels)[i],
                   qual_levels = quali.levels[[i]])
      })
    qual_levels <- dplyr::bind_rows(qual_levels)

    if (any(qual_levels$qual_levels != rownames(qual_coord))) {
      warning('Mismatch in levels of qualitative traits and ',
              'the names of qualitative trait level coordinates.')
    } else {
      rownames(qual_coord) <- paste(qual_levels$qual,
                                    qual_levels$qual_levels, sep = "_")
    }
  }

  if (highlight.core != "none") {
    core <- subset(x = x, criterion = highlight.core)

    ind_coord <- data.frame(ind_coord, check.names = FALSE)

    ind_coord$core <- rownames(ind_coord) %in% core

    ind_coord$core <- ifelse(ind_coord$core == "TRUE", "red", "black")

  } else {

    ind_coord <- data.frame(ind_coord, check.names = FALSE)
  }

  # Plot Biplot ----

  imp <- x$eigen

  biplot_comb <- data.frame(t(combn(x = setdiff(colnames(ind_coord), "core"),
                                    m = 2)))
  biplot_comb$label <- paste(biplot_comb$X1, biplot_comb$X2, sep = " vs. ")

  biplot_list <- vector("list", length = nrow(biplot_comb))
  names(biplot_list) <- biplot_comb$label

  for (i in seq_along(biplot_list)) {

    xlb <- paste(biplot_comb[i, 1], " (",
                 round(imp[biplot_comb[i, 1], ]$`Percentage of variance`, 2),
                 "% explained variance)", sep = "")
    ylb <- paste(biplot_comb[i, 2], " (",
                 round(imp[biplot_comb[i, 2], ]$`Percentage of variance`, 2),
                 "% explained variance)", sep = "")

    if (highlight.core != "none") {

      bipg <- ggplot(data = ind_coord,
                     aes(x = .data[[biplot_comb[i, 1]]],
                         y = .data[[biplot_comb[i, 2]]])) +
        geom_vline(xintercept = 0, linetype = 2, colour = "gray20") +
        geom_hline(yintercept = 0, linetype = 2, colour = "gray20") +
        geom_point(alpha = point.alpha, show.legend = FALSE,
                   colour = ind_coord$core) +
        xlab(label = xlb) +
        ylab(label = ylb) +
        theme_bw()

    } else {

      bipg <- ggplot(data = ind_coord,
                     aes(x = .data[[biplot_comb[i, 1]]],
                         y = .data[[biplot_comb[i, 2]]])) +
        geom_vline(xintercept = 0, linetype = 2, colour = "gray20") +
        geom_hline(yintercept = 0, linetype = 2, colour = "gray20") +
        geom_point(alpha = point.alpha) +
        xlab(label = xlb) +
        ylab(label = ylb) +
        theme_bw()

    }

    method <- attributes(x)$method

    if ((method == "PCA" &&
         (show.traits == "all" || show.traits == "quantitative")) ||
        method == "FAMD" && show.traits == "quantitative") {

      bipg <- bipg +
        geom_segment(data = quant_coord,
                     aes(x = 0, y = 0,
                         xend = .data[[biplot_comb[i, 1]]],
                         yend = .data[[biplot_comb[i, 2]]]),
                     arrow = arrow(length = unit(0.2, "cm")),
                     color = "#619CFF", alpha = segment.alpha) +
        geom_label_repel(data = quant_coord,
                         aes(x = .data[[biplot_comb[i, 1]]],
                             y = .data[[biplot_comb[i, 2]]],
                             label = rownames(quant_coord)),
                         # vjust = -0.5,
                         fill = "#619CFF", colour = "white",
                         alpha = segment.alpha)
    }

    if ((method == "MCA" &&
         (show.traits == "all" || show.traits == "qualitative")) ||
        method == "FAMD" && show.traits == "qualitative") {

      bipg <- bipg +
        geom_segment(data = qual_coord,
                     aes(x = 0, y = 0,
                         xend = .data[[biplot_comb[i, 1]]],
                         yend = .data[[biplot_comb[i, 2]]]),
                     arrow = arrow(length = unit(0.2, "cm")),
                     color = "#00BA38", alpha = segment.alpha) +
        geom_label_repel(data = qual_coord,
                         aes(x = .data[[biplot_comb[i, 1]]],
                             y = .data[[biplot_comb[i, 2]]],
                             label = rownames(qual_coord)),
                         # vjust = -0.5,
                         fill = "#00BA38", colour = "white",
                         alpha = segment.alpha)
    }

    if (method == "FAMD" && show.traits == "all") {

      trait_coord <- rbind(cbind(data.frame(quant_coord, check.names = FALSE),
                                 Type = "Quantitative"),
                           cbind(data.frame(qual_coord, check.names = FALSE),
                                 Type = "Qualitative"))

      bipg <- bipg +
        geom_segment(data = trait_coord,
                     aes(x = 0, y = 0,
                         xend = .data[[biplot_comb[i, 1]]],
                         yend = .data[[biplot_comb[i, 2]]],
                         color = Type),
                     arrow = arrow(length = unit(0.2, "cm")),
                     alpha = segment.alpha, show.legend = FALSE) +
        geom_label_repel(data = trait_coord,
                         aes(x = .data[[biplot_comb[i, 1]]],
                             y = .data[[biplot_comb[i, 2]]],
                             label = rownames(trait_coord),
                             fill = Type),
                         # vjust = -0.5,
                         colour = "white",
                         alpha = segment.alpha, show.legend = FALSE) +
        scale_fill_manual(values = c("#00BA38", "#619CFF")) +
        scale_colour_manual(values = c("#00BA38", "#619CFF"))
    }


    biplot_list[[i]] <- bipg

    rm(bipg, xlb, ylb)

  }

  # patchwork::wrap_plots(biplot_list)

  return(biplot_list)

}
